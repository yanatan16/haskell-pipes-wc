{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (String, ($))
import Control.Monad (Monad(..))
import Control.Applicative ((<$>))
import Data.Monoid (Monoid(..), (<>))
import Data.List (map, elem)

import Data.WordCount.Core (lineCounter, wordCounter, charCounter, byteCounter, CounterM)
import Data.WordCount.Files (wcStdin, wcFiles)

import System.IO
import System.Environment (getArgs)

main = do
  (counter, files) <- parseArgs
  case files of
    [] -> wcStdin counter
    fs -> wcFiles fs counter

parseArgs :: IO (CounterM, [FilePath])
parseArgs = do
  args <- getArgs
  case args of
    ('-':opts):files -> return (optsToCounter opts, files)
    files -> return (defaultCounter, files)

defaultCounter :: CounterM
defaultCounter = lineCounter <> wordCounter <> charCounter

optsToCounter :: String -> CounterM
optsToCounter s = mconcat $ map charMap s
  where
    charMap 'l' = lineCounter
    charMap 'w' = wordCounter
    charMap 'c' = charCounter
    charMap 'm' = if elem 'c' s then mempty else byteCounter
