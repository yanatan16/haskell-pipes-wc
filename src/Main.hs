{-# LANGUAGE NoImplicitPrelude #-}

---------------------
-- Here we parse arguments and call Data.WordCount.Files
---------------------

module Main where

import Prelude (String, ($), error)
import Control.Monad (Monad(..))
import Control.Applicative ((<$>))
import Data.Monoid (Monoid(..), (<>))
import Data.List (map, elem)

import Data.WordCount.Types (Counter(..))
import Data.WordCount.Pipes.Core (lineCounter, wordCounter, charCounter, byteCounter, PipeCounter)
import Data.WordCount.Pipes.Files ()

import System.IO
import System.Environment (getArgs)

main = do
  (counter, files) <- parseArgs
  case files of
    [] -> countStdin counter
    fs -> countFiles fs counter

-- | parse arguments and return a counter object (to count lines/words/chars) and files to count
parseArgs :: IO (PipeCounter, [FilePath])
parseArgs = do
  args <- getArgs
  case args of
    ('-':opts):files -> return (optsToCounter opts, files)
    files -> return (defaultCounter, files)

-- | Default is count lines, words, and chars
defaultCounter = lineCounter <> wordCounter <> charCounter

-- | Convert options to a counter object
optsToCounter s = mconcat $ map charMap s
  where
    charMap 'l' = lineCounter
    charMap 'w' = wordCounter
    charMap 'c' = charCounter
    charMap 'm' = if elem 'c' s then mempty else byteCounter
    charMap _ = error "usage: wc-pipes [-lwcm] [file1 file2 ...]"
