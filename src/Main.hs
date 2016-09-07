{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures -fno-warn-type-defaults #-}

-- | Take in Haskell code and output a vector of source spans and
-- their associated node type and case.

module Main (main) where

import           Control.Applicative
import           Data.Data
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Descriptive
import           Descriptive.Options
import           Language.Haskell.Exts
import           System.Environment

-- | A generic Dynamic-like constructor -- but more convenient to
-- write and pattern match on.
data D = forall a. Data a => D a

-- | A parser. Presently there is only 'parseTopLevel', but in the
-- past, and in the future, there will be the facility to parse
-- specific parse of declarations, rather than re-parsing the whole
-- declaration which can be both slow and brittle.
type Parser = ParseMode -> String -> ParseResult D

-- | The 'empty' method isn't (shouldn't be) used, so this isn't a
-- real Alternative instance (perhaps a Semigroup might do?). But it's
-- handy.
instance Alternative ParseResult where
  empty = ParseFailed undefined undefined
  ParseFailed{} <|> x = x
  x <|> _             = x

--- | Main entry point.
main :: IO ()
main =
  do code <- getContents
     args <- getArgs
     case consume options (map T.pack args) of
       Succeeded (action,typ,exts) ->
         outputWith action typ exts code
       _ ->
         error (T.unpack (textDescription (describe options [])))

-- | Action to perform.
data Action = Parse | Check

-- | Thing to parse.
data ParseType = Decl | Stmt

-- | Command line options.
options :: Monad m => Consumer [Text] (Option ()) m (Action,ParseType,[Extension])
options = (,,) <$> action <*> typ <*> exts
  where action =
          constant "parse" "Parse and spit out spans" Parse <|>
          constant "check" "Just check the syntax" Check
        typ =
          constant "decl" "Parse a declaration" Decl <|>
          constant "stmt" "Parse a statement" Stmt
        exts =
          fmap getExtensions
               (many (prefix "X" "Language extension"))

-- | Output some result with the given action (check/parse/etc.),
-- parsing the given type of AST node. In the past, the type was any
-- kind of AST node. Today, it's just a "decl" which is covered by
-- 'parseTopLevel'.
outputWith :: Action -> ParseType -> [Extension] -> String -> IO ()
outputWith action typ exts code =
  case typ of
    Decl -> output action parseTopLevel exts code
    Stmt -> output action parseSomeStmt exts code

-- | Output AST info for the given Haskell code.
output :: Action -> Parser -> [Extension] -> String -> IO ()
output action parser exts code =
  case parser mode code of
    ParseFailed _ e -> error e
    ParseOk (D ast) ->
      case action of
        Check -> return ()
        Parse ->
          putStrLn ("[" ++
                    concat (genHSE mode ast) ++
                    "]")
  where mode = parseMode {extensions = exts}

-- | An umbrella parser to parse:
--
-- * A declaration.
--
-- * An import line (not normally counted as a declaration).
--
-- * A module header (not normally counted either).
--
-- * A module pragma (normally part of the module header).
--
parseTopLevel :: ParseMode -> String -> ParseResult D
parseTopLevel mode code =
  ((D . fix) <$> parseDeclWithMode mode code) <|>
  (D <$> parseImport mode code) <|>
  ((D . fix) <$> parseModuleWithMode mode code) <|>
  (D <$> parseModulePragma mode code)

-- | Parse a do-notation statement.
parseSomeStmt :: ParseMode -> String -> ParseResult D
parseSomeStmt mode code =
  ((D . fix) <$> parseStmtWithMode mode code) <|>
  ((D . fix) <$> parseExpWithMode mode code) <|>
  (D <$> parseImport mode code)

-- | Apply fixities after parsing.
fix ast = fromMaybe ast (applyFixities baseFixities ast)

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode {extensions = defaultExtensions
                   ,fixities = Nothing}

-- | Generate a list of spans from the HSE AST.
genHSE :: Data a => ParseMode -> a -> [String]
genHSE mode x =
  case gmapQ D x of
    zs@(D y:ys) ->
      case cast y of
        Just s ->
          spanHSE (show (show (typeOf x)))
                  (showConstr (toConstr x))
                  (srcInfoSpan s) :
          concatMap (\(i,D d) -> pre x i ++ genHSE mode d)
                    (zip [0..] ys) ++
              post mode x
        _ ->
          concatMap (\(D d) -> genHSE mode d) zs
    _ -> []

-- | Pre-children tweaks for a given parent at index i.
--
pre :: (Typeable a) => a -> Integer -> [String]
pre x i =
  case cast x of
    -- <foo { <foo = 1> }> becomes <foo <{ <foo = 1> }>>
    Just (RecUpdate SrcSpanInfo{srcInfoPoints=(start:_),srcInfoSpan=end} _ _)
      | i == 1 ->
        [spanHSE (show "RecUpdates")
                 "RecUpdates"
                 (SrcSpan (srcSpanFilename start)
                          (srcSpanStartLine start)
                          (srcSpanStartColumn start)
                          (srcSpanEndLine end)
                          (srcSpanEndColumn end))]
    _ -> case cast x :: Maybe (Deriving SrcSpanInfo)  of
           -- <deriving (X,Y,Z)> becomes <deriving (<X,Y,Z>)
           Just (Deriving _ ds@(_:_)) ->
             [spanHSE (show "InstHeads")
                      "InstHeads"
                      (SrcSpan (srcSpanFilename start)
                               (srcSpanStartLine start)
                               (srcSpanStartColumn start)
                               (srcSpanEndLine end)
                               (srcSpanEndColumn end))
             |Just (IRule _ _ _ (IHCon (SrcSpanInfo start _) _)) <- [listToMaybe ds]
             ,Just (IRule _ _ _ (IHCon (SrcSpanInfo end _) _)) <- [listToMaybe (reverse ds)]]
           _ -> []

-- | Post-node tweaks for a parent, e.g. adding more children.
post :: (Typeable a) => ParseMode -> a ->  [String]
post mode x =
  case cast x of
    Just (QuasiQuote (base :: SrcSpanInfo) qname content) ->
      case parseExpWithMode mode content of
        ParseOk ex -> genHSE mode (fmap (redelta qname base) ex)
        ParseFailed _ e -> error e
    _ -> []

-- | Apply a delta to the positions in the given span from the base.
redelta :: String -> SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo
redelta qname base (SrcSpanInfo (SrcSpan fp sl sc el ec) pts) =
  SrcSpanInfo
    (if sl == 1
        then SrcSpan fp
                     (sl + lineOffset)
                     (sc + columnOffset)
                     (el + lineOffset)
                     (if el == sl
                         then ec + columnOffset
                         else ec)
        else SrcSpan fp
                     (sl + lineOffset)
                     sc
                     (el + lineOffset)
                     ec)
    pts
  where lineOffset = sl' - 1
        columnOffset =
          sc' - 1 +
          length ("[" :: String) +
          length qname +
          length ("|" :: String)
        (SrcSpanInfo (SrcSpan _ sl' sc' _ _) _) = base

-- | Generate a span from a HSE SrcSpan.
spanHSE :: String -> String -> SrcSpan -> String
spanHSE typ cons SrcSpan{..} = "[" ++ spanContent ++ "]"
  where unqualify   = dropUntilLast '.'
        spanContent =
          unwords [unqualify typ
                  ,cons
                  ,show srcSpanStartLine
                  ,show srcSpanStartColumn
                  ,show srcSpanEndLine
                  ,show srcSpanEndColumn]

------------------------------------------------------------------------------
-- General Utility

-- | Like 'dropWhile', but repeats until the last match.
dropUntilLast :: Char -> String -> String
dropUntilLast ch = go []
  where
    go _ (c:cs) | c == ch = go [] cs
    go acc (c:cs)         = go (c:acc) cs
    go acc []             = reverse acc

--------------------------------------------------------------------------------
-- Parsers that HSE hackage doesn't have

parseImport :: ParseMode -> String -> ParseResult (ImportDecl SrcSpanInfo)
parseImport mode code =
  case parseModuleWithMode mode code of
    ParseOk (Module _ _ _ [i] _) -> return i
    ParseOk _ -> ParseFailed noLoc "parseImport"
    ParseFailed x y -> ParseFailed x y

parseModulePragma :: ParseMode -> String -> ParseResult (ModulePragma SrcSpanInfo)
parseModulePragma mode code =
  case parseModuleWithMode mode (code ++ "\nmodule X where") of
    ParseOk (Module _ _ [p] _ _) -> return p
    ParseOk _ -> ParseFailed noLoc "parseModulePragma"
    ParseFailed x y -> ParseFailed x y

--------------------------------------------------------------------------------
-- Extensions stuff stolen from hlint

-- | Consume an extensions list from arguments.
getExtensions :: [Text] -> [Extension]
getExtensions = foldl f defaultExtensions . map T.unpack
  where f _ "Haskell98" = []
        f a ('N':'o':x)
          | Just x' <- readExtension x =
            delete x' a
        f a x
          | Just x' <- readExtension x =
            x' :
            delete x' a
        f _ x = error $ "Unknown extension: " ++ x

-- | Parse an extension.
readExtension :: String -> Maybe Extension
readExtension x =
  case classifyExtension x of
    UnknownExtension _ -> Nothing
    x' -> Just x'

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions =
  [e | e@EnableExtension{} <- knownExtensions] \\
  map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ]
