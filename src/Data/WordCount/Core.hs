{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.WordCount.Core (
  lineCounter,
  wordCounter,
  charCounter,
  byteCounter,
  runCounter,
  Counts(..),
  CountsM,
  addCounts,
  CounterM
)
where

import Prelude (Show (..), Char, String, Int, (.), ($), (+), max, words, lines, init, null, last)

import Data.Word (Word8)
import Data.ByteString.Lazy (ByteString, unpack) -- unpack :: ByteString -> [Word8]
import Data.ByteString.Lazy.Char8 (pack)         -- pack :: String -> ByteString

import Data.Monoid (Monoid(..))
import Data.Maybe (Maybe(..))
import Control.Monad (mapM_, forever, Monad(..), (=<<), (>>))
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT)
import Control.Monad.Identity (Identity, runIdentity)

import qualified Pipes.Prelude as P (tee, last)
import Pipes ((>->), Consumer, Producer, Pipe, await, yield, each, cat)

data Counts = Counts {
  _lines :: Int,
  _words :: Int,
  _chars :: Int,
  _bytes :: Int
} deriving (Show)

instance Monoid Counts where
  mempty = Counts 0 0 0 0
  (Counts l1 w1 c1 b1) `mappend` (Counts l2 w2 c2 b2) = Counts (max l1 l2) (max w1 w2) (max c1 c2) (max b1 b2)

addCounts :: Counts -> Counts -> Counts
addCounts (Counts l1 w1 c1 b1) (Counts l2 w2 c2 b2) = Counts (l1 + l2) (w1 + w2) (c1 + c2) (b1 + b2)

type Counter m r = Pipe String String m r
type CountsM = WriterT Counts Identity
type CounterM = Counter CountsM ()

instance (Monad m) => Monoid (Counter m r) where
  mempty = cat
  mappend = (>->)

type CountsUpdater = Int -> Counts
upLines n = Counts n 0 0 0
upWords n = Counts 0 n 0 0
upChars n = Counts 0 0 n 0
upBytes n = Counts 0 0 0 n

runCounter :: String -> CounterM -> Counts
runCounter s c = execCounts (yield s >-> c)

execCounts :: Producer String CountsM () -> Counts
execCounts = runIdentity . execWriterT . P.last

-- | Line Count
lineCounter :: MonadWriter Counts m => Counter m ()
lineCounter = P.tee $ (lines' >-> countConsumer upLines)

-- | Word Count
wordCounter :: MonadWriter Counts m => Counter m ()
wordCounter = P.tee $ lines' >-> words' >-> countConsumer upWords

-- | Char Count
charCounter :: MonadWriter Counts m => Counter m ()
charCounter = P.tee $ chars' >-> countConsumer upChars

-- | Byte Count
byteCounter :: MonadWriter Counts m => Counter m ()
byteCounter = P.tee $ bytes' >-> countConsumer upBytes

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
lines' = forever go
  where
    go = do
      s <- await
      let l = lines s
      mapM_ yield $ init l
      if null $ last l
        then return ()
        else yield $ last l