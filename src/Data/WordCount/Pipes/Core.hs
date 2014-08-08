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

import Prelude (Show (..), String, Int, (.), ($), (+), (==), and, not, const, length, otherwise, max, and, words, lines, init, null, last)

import Data.Tuple (uncurry)
import Data.Text.Lazy (Text, uncons, chunksOf)
import Data.Char (Char, isSpace)
import Data.Maybe (Maybe(..))
import Data.ByteString.Lazy (ByteString, unpack) -- unpack :: ByteString -> [Word8]
import Data.ByteString.Lazy.Builder (charUtf8, toLazyByteString)         -- pack :: String -> ByteString

import Data.Monoid (Monoid(..))
import Control.Monad (mapM_, forever, Monad(..), (>>), (>=>))
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT)
import Control.Monad.Identity (Identity, runIdentity)

import qualified Pipes.Prelude as P (tee, last)
import Pipes ((>->), Consumer, Producer, Pipe, await, yield, each, cat)

type PipeCounterBase m r = Pipe (Maybe Char) (Maybe Char) m r

-- | We run the Pipes system with this monad, a simple WriterT of Identity
type CountsM = WriterT Counts Identity

-- | This is the real Counter Object
type PipeCounter = PipeCounterBase CountsM ()

instance (Monad m) => Monoid (PipeCounterBase m r) where
  mempty = cat
  mappend = (>->)

runCounter :: Text -> PipeCounter -> Counts
runCounter s c = execCounts (text' s >-> chars' >-> c)

execCounts :: Producer (Maybe Char) CountsM () -> Counts
execCounts = runIdentity . execWriterT . P.last

-- | Line Count
lineCounter :: PipeCounter
lineCounter = P.tee $ counter upLines (\c -> if c == '\n' then 1 else 0)

-- | Word Count
wordCounter :: PipeCounter
wordCounter = P.tee $ counterState upWords cntr 'a'
  where
    cntr c c'
      | and [isSpace c, not $ isSpace c'] = (1, c')
      | otherwise                         = (0, c')


-- | Char Count
charCounter :: PipeCounter
charCounter = P.tee $ counter upChars (const 1)

-- | Byte Count
byteCounter :: PipeCounter
byteCounter = P.tee $ counter upBytes cntr
  where cntr = length . unpack . toLazyByteString . charUtf8

counter :: MonadWriter Counts m => CountsUpdater -> (a -> Int) -> Consumer (Maybe a) m ()
counter update f = counterState update (\_ a -> (f a, ())) ()

counterState :: MonadWriter Counts m => CountsUpdater -> (b -> a -> (Int, b)) -> b -> Consumer (Maybe a) m ()
counterState update f st = go 0 st
  where
    go n st = do
      mc <- await
      case mc of
        Nothing -> tell $ update n
        Just c -> do
          let (i, st') = f st c
          go (n + i) st'

chars' :: Monad m => Pipe (Maybe Text) (Maybe Char) m ()
chars' = forever $ await >>= go
  where
    go Nothing = yield Nothing
    go (Just txt) = do
      case uncons txt of
        Nothing -> return ()
        Just (c, txt') -> yield (Just c) >> go (Just txt')

text' :: Monad m => Text -> Producer (Maybe Text) m ()
--text' = (mapM_ (yield . Just) . chunksOf 64) >=> \_ -> yield Nothing
text' = yield . Just