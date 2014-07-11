{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.WordCount (
  lineCounter,
  wordCounter,
  charCounter,
  byteCounter,
  runCounter,
  Counts(..),
  CountsM,
  Counter,

  (>->)
)
where

import Prelude (Show (..), Char, String, Int, (.), ($), (+), max, words, lines)

import Data.Word (Word8)
import Data.ByteString.Lazy (ByteString, unpack) -- unpack :: ByteString -> [Word8]
import Data.ByteString.Lazy.Char8 (pack)         -- pack :: String -> ByteString

import Data.Monoid (Monoid(..))
import Data.Maybe (Maybe(..))
import Control.Monad (mapM_, forever, Monad(..), (=<<), (>>))
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT)
import Control.Monad.Identity (Identity, runIdentity)

import Pipes.Prelude (tee, last)
import Pipes ((>->), Consumer, Producer, Producer, Pipe, await, yield, each, cat)

data Counts = Counts {
  _lines :: Int,
  _words :: Int,
  _chars :: Int,
  _bytes :: Int
} deriving (Show)

instance Monoid Counts where
  mempty = Counts 0 0 0 0
  (Counts l1 w1 c1 b1) `mappend` (Counts l2 w2 c2 b2) = Counts (max l1 l2) (max w1 w2) (max c1 c2) (max b1 b2)

type CountsWriterT m = WriterT Counts m
type CountsUpdater = Int -> Counts
type Counter m r = Pipe String String m r

instance (Monad m) => Monoid (Counter m r) where
  mempty = cat
  mappend = (>->)

upLines n = Counts n 0 0 0
upWords n = Counts 0 n 0 0
upChars n = Counts 0 0 n 0
upBytes n = Counts 0 0 0 n

type CountsM = WriterT Counts Identity

textProducer :: Monad m => String -> Producer String m ()
textProducer s = each [s]

runCounter :: String -> Counter CountsM () -> Counts
runCounter s c = runCounts (textProducer s >-> c)

runCounts :: Producer String CountsM () -> Counts
runCounts = runCountsM . countAll

runCountsM :: CountsM r -> Counts
runCountsM = runIdentity . execWriterT

countAll :: Producer String CountsM () -> CountsM (Maybe String)
countAll c = last c

-- | Line Count
lineCounter :: MonadWriter Counts m => Counter m ()
lineCounter = tee $ (lines' >-> countConsumer upLines)

-- | Word Count
wordCounter :: MonadWriter Counts m => Counter m ()
wordCounter = tee $ lines' >-> words' >-> countConsumer upWords

-- | Char Count
charCounter :: MonadWriter Counts m => Counter m ()
charCounter = tee $ chars' >-> countConsumer upChars

-- | Byte Count
byteCounter :: MonadWriter Counts m => Counter m ()
byteCounter = tee $ bytes' >-> countConsumer upBytes

countConsumer :: MonadWriter Counts m => CountsUpdater -> Consumer a m ()
countConsumer update = go 0
  where
    go n = do
      await
      tell $ update (n + 1)
      go (n + 1)

bytes' :: Monad m => Pipe String Word8 m ()
bytes' = forever $ mapM_ yield . unpack . pack =<< await

chars' :: Monad m => Pipe String Char m ()
chars' = forever $ mapM_ yield =<< await

words' :: Monad m => Pipe String String m ()
words' = forever $ mapM_ yield . words =<< await

lines' :: Monad m => Pipe String String m ()
lines' = forever $ mapM_ yield . lines =<< await