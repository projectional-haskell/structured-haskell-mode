{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Take in Haskell code and output a vector of source spans and
-- their associated node type and case.

module Main (main) where

import Control.Applicative
import Data.Data
import Data.Maybe
import Language.Haskell.Exts.Annotated
import System.Environment

data TopLevelSrcContainer = forall ast. Data ast => TopLevelSrcContainer ast

type Parser = ParseMode -> String -> ParseResult TopLevelSrcContainer

instance Alternative ParseResult where
  empty = ParseFailed  undefined undefined
  ParseFailed{} <|> x = x
  x <|> _             = x
               
main :: IO ()
main = do
  code <- getContents
  action:typ:_ <- getArgs
  outputWith action typ code
  
outputWith :: String -> String -> String -> IO ()
outputWith action typ code = case typ of
  "decl" -> output action parseTopLevelElement code
  _ -> error "Unknown parser type."

-- | Output AST info for the given Haskell code.
output :: String -> Parser -> String -> IO ()
output action parser code = case parser parseMode code of
  ParseFailed _ e -> error e
  ParseOk (TopLevelSrcContainer ast) -> case action of
    "check" -> return ()
    "parse" -> putStrLn ("[" ++ concat (genHSE ast) ++ "]")
    _       -> error "unknown action"

parseTopLevelElement :: ParseMode -> String -> ParseResult TopLevelSrcContainer
parseTopLevelElement mode code =
  TopLevelSrcContainer . fix <$> parseDeclWithMode mode code   <|>
  TopLevelSrcContainer       <$> parseImport mode code         <|>
  TopLevelSrcContainer . fix <$> parseModuleWithMode mode code <|>
  TopLevelSrcContainer       <$> parseModulePragma mode code

fix :: AppFixity ast => ast SrcSpanInfo -> ast SrcSpanInfo
fix ast = fromMaybe ast (applyFixities baseFixities ast)
    
-- | Generate a list of spans from the HSE AST.
genHSE :: Data a => a -> [String]
genHSE x = case gmapQ TopLevelSrcContainer x of
  zs@(TopLevelSrcContainer y:ys) -> case cast y of
    Just s -> spanHSE (show (show (typeOf x)))
                      (showConstr (toConstr x))
                      (srcInfoSpan s) :
                        concatMap (\(i,TopLevelSrcContainer d) -> pre x i ++ genHSE d)
                                  (zip [0..] ys)
    _      -> concatMap (\(TopLevelSrcContainer d) -> genHSE d) zs
  _ -> []

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode = defaultParseMode { extensions = allExtensions
                             , fixities   = Nothing
                             }
 where allExtensions = filter isDisabledExtention knownExtensions
       isDisabledExtention (DisableExtension _) = False
       isDisabledExtention _                    = True

-- | Pre-children tweaks for a given parent at index i.
--
pre :: (Typeable a) => a -> Integer -> [String]
pre x i = case cast x of
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
           | Just (IHead (SrcSpanInfo start _) _ _) <- [listToMaybe ds]
           , Just (IHead (SrcSpanInfo end _) _ _) <- [listToMaybe (reverse ds)]]
         _ -> []

-- | Generate a span from a HSE SrcSpan.
spanHSE :: String -> String -> SrcSpan -> String
spanHSE typ cons SrcSpan{..} = "[" ++ spanStr ++ "]"
  where unqualify = dropUntilLastOccurrence '.'
        spanStr   = unwords [ unqualify typ
                            , cons
                            , show srcSpanStartLine
                            , show srcSpanStartColumn
                            , show srcSpanEndLine
                            , show srcSpanStartColumn
                            ]

dropUntilLastOccurrence :: Eq a => a -> [a] -> [a]
dropUntilLastOccurrence ch = go []
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
