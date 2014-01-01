{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Take in Haskell code and output a vector of source spans and
-- their associated node type and case.

module Main (main) where

import Control.Applicative
import Data.Char
import Data.Data
import Data.Maybe
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.Fixity
import System.Environment
import System.IO

type Parser = ParseMode -> String -> ParseResult D

data D = forall a. Data a => D a

-- | Main entry point.
main :: IO ()
main = do
  code <- getContents
  action:typ:_ <- getArgs
  outputWith action typ code

outputWith :: String -> String -> String -> IO ()
outputWith action typ code =
  case typ of
    "decl" ->
      output action
             (\mode code ->
                fmap D (fmap fix (parseDeclWithMode mode code)) <|>
                fmap D (parseImport mode code) <|>
                fmap D (fmap fix (parseModuleWithMode mode code)) <|>
                fmap D (parseModulePragma mode code))
             code
    _ -> error "Unknown parser type."
  where fix ast = fromMaybe ast (applyFixities baseFixities ast)

instance Alternative ParseResult where
  empty = ParseFailed undefined undefined
  ParseFailed{} <|> x = x
  x <|> _             = x

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
parseMode =
  defaultParseMode { extensions = allExtensions
                   , fixities   = Nothing
                   }
  where allExtensions =
          filter (\x ->
                   case x of
                     DisableExtension x -> False
                     _ -> True)
                 knownExtensions

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
-- <foo { <foo = 1> }> becomes <foo <{ <foo = 1> }>>
--
pre :: (Typeable a) => a -> Integer -> [String]
pre x i =
  case cast x  of
    Just (RecUpdate SrcSpanInfo{srcInfoPoints=(start:_),srcInfoSpan=end} _ _)
      | i == 1 ->
        [spanHSE (show "RecUpdates SrcSpanInfo")
                 "RecUpdates"
                 (SrcSpan (srcSpanFilename start)
                          (srcSpanStartLine start)
                          (srcSpanStartColumn start)
                          (srcSpanEndLine end)
                          (srcSpanEndColumn end))]
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
