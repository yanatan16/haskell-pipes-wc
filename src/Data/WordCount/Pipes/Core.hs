{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

------------
--- Implement the core Pipes-based count some stuff in a string logic
--- Counters are Pipe String String objects which simply count all objects and write to the underlying monad
--- They are combined by connecting the stream
--- They are run on a string and folded up to return a Counts object
--- Pipes must be run with a monad as its base, we avoid IO monad by using a WriterT Counts
-------------

module Data.WordCount.Pipes.Core (
  lineCounter,
  wordCounter,
  charCounter,
  byteCounter,
  runCounter,
  CountsM,
  PipeCounter
)
where

import Data.WordCount.Types (Counts(..), CountsUpdater, upLines, upWords, upChars, upBytes)

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

type PipeCounterBase m r = Pipe String String m r

-- | We run the Pipes system with this monad, a simple WriterT of Identity
type CountsM = WriterT Counts Identity

-- | This is the real Counter Object
type PipeCounter = PipeCounterBase CountsM ()

instance (Monad m) => Monoid (PipeCounterBase m r) where
  mempty = cat
  mappend = (>->)

runCounter :: String -> PipeCounter -> Counts
runCounter s c = execCounts (yield s >-> c)

execCounts :: Producer String CountsM () -> Counts
execCounts = runIdentity . execWriterT . P.last

-- | Line Count
lineCounter :: PipeCounter
lineCounter = P.tee $ (lines' >-> countConsumer upLines)

-- | Word Count
wordCounter :: PipeCounter
wordCounter = P.tee $ lines' >-> words' >-> countConsumer upWords

-- | Char Count
charCounter :: PipeCounter
charCounter = P.tee $ chars' >-> countConsumer upChars

-- | Byte Count
byteCounter :: PipeCounter
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