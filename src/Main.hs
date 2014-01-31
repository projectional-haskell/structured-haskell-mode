{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Take in Haskell code and output a vector of source spans and
-- their associated node type and case.

module Main (main) where

import Control.Applicative
import Data.Data
import Data.Maybe
import Language.Haskell.Exts.Annotated
import System.Environment


data D = forall a. Data a => D a

type Parser = ParseMode -> String -> ParseResult D

instance Alternative ParseResult where
  empty = ParseFailed undefined undefined
  ParseFailed{} <|> x = x
  x <|> _             = x

-- | Main entry point.
main :: IO ()
main = do
  code <- getContents
  action:typ:_ <- getArgs
  outputWith action typ code

outputWith :: String -> String -> String -> IO ()
outputWith action typ code = case typ of
  "decl" ->
      output action parseTopLevelElement code
  _ -> error "Unknown parser type."

parseTopLevelElement :: ParseMode -> String -> ParseResult D
parseTopLevelElement mode code =
  D . fix <$> parseDeclWithMode mode code   <|>
  D       <$> parseImport mode code         <|>
  D . fix <$> parseModuleWithMode mode code <|>
  D       <$> parseModulePragma mode code
  
  where
    fix :: AppFixity ast => ast SrcSpanInfo -> ast SrcSpanInfo
    fix ast = fromMaybe ast (applyFixities baseFixities ast)

-- | Get the type of the parser.
parserRep :: Typeable ast => ast -> String
parserRep = show . head . typeRepArgs . typeOf

-- | Output AST info for the given Haskell code.
output :: String -> Parser -> String -> IO ()
output action parseWithMode code = do
  case parseWithMode parseMode code of
    ParseFailed _ e -> error e
    ParseOk (D ast) -> case action of
        "check" -> return ()
        "parse" -> putStrLn ("[" ++ concat (genHSE ast) ++ "]")
        _       -> error "unknown action"

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode = defaultParseMode { extensions = allExtensions
                             , fixities   = Nothing
                             }
 where allExtensions = filter isDisabledExtention knownExtensions
       isDisabledExtention (DisableExtension _) = False
       isDisabledExtention _                    = True

-- | Generate a list of spans from the HSE AST.
genHSE :: Data a => a -> [String]
genHSE x =
  case gmapQ D x of
    zs@(D y:ys) ->
      case cast y of
        Just s ->
          spanHSE (show (show (typeOf x)))
                  (showConstr (toConstr x))
                  (srcInfoSpan s) :
          concat (map (\(i,(D d)) -> pre x i ++ genHSE d)
                      (zip [0..] ys))
        _ ->
          concatMap (\(D d) -> genHSE d) zs
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
             |Just (IHead (SrcSpanInfo start _) _ _) <- [listToMaybe ds]
             ,Just (IHead (SrcSpanInfo end _) _ _) <- [listToMaybe (reverse ds)]]
           _ -> []

-- | Generate a span from a HSE SrcSpan.
spanHSE :: String -> String -> SrcSpan -> String
spanHSE typ cons (SrcSpan _ a b c d) =
  concat ["["
         ,unwords [unqualify typ
                  ,cons
                  ,show a
                  ,show b
                  ,show c
                  ,show d]
         ,"]"]
  where unqualify = go [] where
          go acc ('.':cs) = go [] cs
          go acc (c:cs)   = go (c:acc) cs
          go acc []       = reverse acc

-- | Pretty print a source location.
printSrcLoc :: SrcLoc -> String
printSrcLoc SrcLoc{..} =
  srcFilename ++ ":" ++ show srcLine ++ ":" ++ show srcColumn

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
