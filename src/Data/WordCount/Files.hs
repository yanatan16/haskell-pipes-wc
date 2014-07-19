{-# LANGUAGE NoImplicitPrelude #-}

module Data.WordCount.Files (
  wcFiles,
  wcStdin
) where

import Prelude (($), flip, (+), (>), IO, show)
import Data.List ((++), map)
import Data.String (String)
import Data.Monoid (Monoid(..))
import System.IO (stdin, openFile, putStr, putStrLn, Handle, hGetContents, FilePath, IOMode(ReadMode))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (Monad(..), forM_, mapM_, liftM, liftM2, forever)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad.Morph (lift)

import Data.WordCount.Core (Counts(..), CountsM, CounterM, addCounts, runCounter)

import Pipes.Prelude (fromHandle, last)
import Pipes ((>->), Consumer, Producer, Pipe, await, yield, runEffect)

data FileStream = File FilePath | StdIn | End

wcFiles :: [FilePath] -> CounterM -> IO ()
wcFiles fps cm = runFileStream (map File fps) cm

wcStdin :: CounterM -> IO ()
wcStdin cm = runFileStream [StdIn] cm

runFileStream :: [FileStream] -> CounterM -> IO ()
runFileStream fss cm = runEffect $ fileStreams fss >-> fileCounts cm >-> collect >-> printem

fileStreams :: MonadIO m => [FileStream] -> Producer FileStream m ()
fileStreams fps = mapM_ yield (fps ++ [End])

fileCounts :: MonadIO m => CounterM -> Pipe FileStream (FileStream, Counts) m ()
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

countFile :: CounterM -> FilePath -> IO Counts
countFile cm fp = openFile fp ReadMode >>= countHandle cm

countHandle :: CounterM -> Handle -> IO Counts
countHandle cm h = do
  contents <- hGetContents h
  return $ runCounter contents cm

printCounts :: Counts -> String -> IO ()
printCounts cnts fn = do
  forM_ [_lines, _words, _chars, _bytes] $ \ext -> do
    if ext cnts > 0
      then putStr ("\t" ++ (show $ ext cnts))
      else return ()
  putStr ("\t" ++ fn ++ "\n")

summarize :: Counts -> IO ()
summarize = flip printCounts "total"