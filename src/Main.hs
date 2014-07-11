{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Bool(..), ($), (>), (++), show)
import Control.Monad (Monad(..), forM_)
import Control.Applicative ((<$>),(<*>))
import Data.Monoid (Monoid(..))

import Data.WordCount

import System.IO

data Config = Config {
  linesEnabled :: Bool,
  wordsEnabled :: Bool,
  charsEnabled :: Bool,
  bytesEnabled :: Bool
}

main = do
  counts <- countFile stdin (lineCounter `mappend` wordCounter `mappend` charCounter)
  printFileCounts "stdin" counts

countFile :: Handle -> Counter CountsM () -> IO Counts
countFile handle counter = do
  contents <- hGetContents handle
  return $ runCounter contents counter

printFileCounts :: FilePath -> Counts -> IO ()
printFileCounts fn cnts = do
  forM_ [_lines, _words, _chars, _bytes] $ \ext -> do
    if ext cnts > 0
      then putStr $ "\t" ++ (show $ ext cnts)
      else return ()
  putStrLn $ "\t" ++ fn