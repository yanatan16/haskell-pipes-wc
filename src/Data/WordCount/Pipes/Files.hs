{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


----------
--- We just export the Counter typeclass instance for PipeCounter so it can be run on files
--- We use a full stream here to pipe files through and then an End object, to total and return

module Data.WordCount.Pipes.Files (
) where

import Prelude (($), flip, (+), (>), IO, show)
import Data.List ((++), map)
import Data.String (String)
import Data.Monoid (Monoid(..))
import System.IO (stdin, openFile, putStr, putStrLn, Handle, FilePath, IOMode(ReadMode))
import Data.Text.Lazy.IO (hGetContents)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (Monad(..), forM_, mapM_, liftM, liftM2, forever)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad.Morph (lift)

import Data.WordCount.Types (Counts(..), addCounts, Counter(..), printCounts, summarize)
import Data.WordCount.Pipes.Core (CountsM, PipeCounter, runCounter)

import Pipes.Prelude (fromHandle, last)
import Pipes ((>->), Consumer, Producer, Pipe, await, yield, runEffect)

data FileStream = File FilePath | StdIn | End

-- | The main export from here is how to run counters on files
instance Counter PipeCounter where
  countFiles = wcFiles
  countStdin = wcStdin

wcFiles :: [FilePath] -> PipeCounter -> IO ()
wcFiles fps cm = runFileStream (map File fps) cm

wcStdin :: PipeCounter -> IO ()
wcStdin cm = runFileStream [StdIn] cm

-- | Run a counter on a list of FileStreams (files/stdin) and print the results
runFileStream :: [FileStream] -> PipeCounter -> IO ()
runFileStream fss cm = runEffect $ fileStreams fss >-> fileCounts cm >-> collect >-> printem

-- | Array->Producer of FileStreams
-- | When this ends, the stream ends.
fileStreams :: MonadIO m => [FileStream] -> Producer FileStream m ()
fileStreams fps = mapM_ yield (fps ++ [End])

-- | Run the counts from Data.WordCount.Pipes.Core by reading the file/stdin and running it
fileCounts :: MonadIO m => PipeCounter -> Pipe FileStream (FileStream, Counts) m ()
fileCounts cm = forever go
  where
    go = do
      v <- await
      case v of
        File fp -> do
          c <- lift $ liftIO $ countFile cm fp
          yield (File fp, c)
        StdIn -> do
          c <- lift $ liftIO $ countHandle cm stdin
          yield (StdIn, c)
        End -> yield (End, mempty)

-- | Collect up the counts for totalling
collect :: MonadIO m => Pipe (FileStream, Counts) (FileStream, Counts) m ()
collect = go mempty
  where
    go c = do
      v <- await
      case v of
        (End,_) -> yield (End,c)
        (fs, c2) -> do
          yield (fs, c2)
          go (c `addCounts` c2)

-- | Print the results
printem :: MonadIO m => Consumer (FileStream, Counts) m ()
printem = go 0
  where
    go i = do
      v <- await
      lift $ liftIO $ case v of
        (File fp, c) -> printCounts c fp
        (StdIn, c) -> printCounts c ""
        (End, c) -> if i > 1
          then printCounts c "total"
          else return ()
      go (i + 1)

countFile :: PipeCounter -> FilePath -> IO Counts
countFile cm fp = openFile fp ReadMode >>= countHandle cm

-- | Helper to actually count
countHandle :: PipeCounter -> Handle -> IO Counts
countHandle cm h = do
  contents <- hGetContents h
  return $ runCounter contents cm