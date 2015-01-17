{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Command-line options parser.

module Descriptive.Options where

import           Descriptive

import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | Description of a commandline option.
data Option
  = AnyString !Text
  | Constant !Text
  | Flag !Text !Text
  | Arg !Text !Text
  | Prefix !Text !Text
  deriving (Show)

-- | Make a text description of the command line options.
textDescription :: Description Option -> Text
textDescription = go . clean
  where clean (And None a) = clean a
        clean (And a None) = clean a
        clean (Or a None) = clean a
        clean (Or None a) = clean a
        clean (And a b) = And (clean a) (clean b)
        clean (Or a b) = Or (clean a) (clean b)
        clean a = a
        go d =
          case d of
            Unit o -> textOpt o
            Bounded min' max' d' ->
              "[" <> go d' <> "]" <>
              if min' == 0
                 then "*"
                 else "+"
            And a b -> go a <> " " <> go b
            Or a b -> "(" <> go a <> "|" <> go b <> ")"
            Sequence xs ->
              T.intercalate " "
                            (map go xs)
            Wrap o d -> textOpt o <> " " <> go d
            None -> ""

-- | Make a text description of an option.
textOpt (AnyString t) = T.map toUpper t
textOpt (Constant t) = t
textOpt (Flag t _) = "-f" <> t
textOpt (Arg t _) = "-" <> t <> " <...>"
textOpt (Prefix t _) = "-" <> t <> "<...>"

-- | Consume one argument from the argument list.
anyString :: Text -> Consumer [Text] Option Text
anyString help =
  consumer (d,)
           (\s ->
              case s of
                [] -> (Left d,s)
                (x:s') -> (Right x,s'))
  where d = Unit (AnyString help)

-- | Consume one argument from the argument list.
constant :: Text -> Consumer [Text] Option Text
constant x' =
  consumer (d,)
           (\s ->
              case s of
                (x:s') | x == x' ->
                  (Right x,s')
                _ -> (Left d,s))
  where d = Unit (Constant x')

-- | Find a short boolean flag.
flag :: Text -> Text -> Consumer [Text] Option Bool
flag name help =
  consumer (d,)
           (\s ->
              (Right (elem ("-f" <> name) s),filter (/= "-f" <> name) s))
  where d = Unit (Flag name help)

-- | Find an argument prefixed by -X.
prefix :: Text -> Text -> Consumer [Text] Option Text
prefix pref help =
  consumer (d,)
           (\s ->
              case find (T.isPrefixOf ("-" <> pref)) s of
                Nothing -> (Left d,s)
                Just a -> (Right (T.drop (T.length pref + 1) a), delete a s))
  where d = Unit (Prefix pref help)

-- | Find a named argument.
arg :: Text -> Text -> Consumer [Text] Option Text
arg name help =
  consumer (d,)
           (\s ->
              let indexedArgs =
                    zip [0 :: Integer ..] s
              in case find ((== "--" <> name) . snd) indexedArgs of
                   Nothing -> (Left d,s)
                   Just (i,_) ->
                     case lookup (i + 1) indexedArgs of
                       Nothing -> (Left d,s)
                       Just text ->
                         (Right text
                         ,map snd (filter (\(j,_) -> j /= i && j /= i + 1) indexedArgs)))
  where d = Unit (Arg name help)
