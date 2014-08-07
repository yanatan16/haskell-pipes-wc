{-# LANGUAGE NoImplicitPrelude #-}

-------------
--- Basic types for all `wc` applications
--- Counts holder and instances
--- Counter typeclass for counting files or stdin
--- CountsUpdater to convert Ints to Counts objects
--------------

module Data.WordCount.Types (
  Counts(..),
  addCounts, printCounts, summarize,
  Counter(..),
  CountsUpdater, upLines, upWords, upChars, upBytes
) where

import Prelude (max, (+), (++), ($), (>), flip, FilePath, String, Int, Show(..), IO)
import Data.Monoid (Monoid(..))
import Control.Monad (Monad(..), forM_)
import System.IO (putStr)

------ Counts ------
-- | A counts object has lines, words, chars, and bytes counters
-- | Its a Monoid which takes the maximum of each field
data Counts = Counts {
  _lines :: Int,
  _words :: Int,
  _chars :: Int,
  _bytes :: Int
} deriving (Show)

instance Monoid Counts where
  mempty = Counts 0 0 0 0
  (Counts l1 w1 c1 b1) `mappend` (Counts l2 w2 c2 b2) = Counts (max l1 l2) (max w1 w2) (max c1 c2) (max b1 b2)

-- | Add two counters together (for totalling)
addCounts :: Counts -> Counts -> Counts
addCounts (Counts l1 w1 c1 b1) (Counts l2 w2 c2 b2) = Counts (l1 + l2) (w1 + w2) (c1 + c2) (b1 + b2)

-- | Convert Ints to Counts for `mappend`
type CountsUpdater = Int -> Counts
upLines n = Counts n 0 0 0
upWords n = Counts 0 n 0 0
upChars n = Counts 0 0 n 0
upBytes n = Counts 0 0 0 n

-- | Typeclass Counter is implemented by each `wc` implementation
class Monoid c => Counter c where
  countFiles :: [FilePath] -> c -> IO ()
  countStdin :: c -> IO ()

-- | Utility for printing
printCounts :: Counts -> String -> IO ()
printCounts cnts fn = do
  forM_ [_lines, _words, _chars, _bytes] $ \ext -> do
    if ext cnts > 0
      then putStr ("\t" ++ (show $ ext cnts))
      else return ()
  putStr ("\t" ++ fn ++ "\n")

-- | Utility for printing
summarize :: Counts -> IO ()
summarize = flip printCounts "total"