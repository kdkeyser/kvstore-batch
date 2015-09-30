{-# LANGUAGE GADTs #-}

module Aggregator where

import Data.IORef
import Control.Concurrent
import Safe.Exact
import Backend

import qualified SimpleAction
import qualified Data.List
import qualified Future

type AggregatorEntry = IO ()

type Aggregator = (IORef [(BackendAction, BackendResult -> IO ())], Backend)

create :: Backend -> IO Aggregator
create backend = do
    ioRef <- newIORef []
    return (ioRef, backend)

add :: Aggregator -> SimpleAction.SimpleAction a -> IO (Future.FutureT IO a)
add aggregator@(entriesRef, _backend) action = do
    mVar <- newEmptyMVar
    let newAction = case action of
            SimpleAction.Get key -> (GetAction key, \(GetResult result) -> putMVar mVar result)
            SimpleAction.Put key value -> (PutAction key value, \(PutResult result) -> putMVar mVar result)
    atomicModifyIORef entriesRef (\entries -> (newAction:entries, ()))
    return $ Future.Future (execute aggregator) (readMVar mVar)

execute :: Aggregator -> IO ()
execute (entriesRef, Backend executeBatch) = do
   l <- atomicModifyIORef entriesRef (\entries -> ([], entries))
   executeEntries $ Data.List.reverse l
 where
   executeEntries [] = return ()
   executeEntries l =
       let (batch, actions) = Data.List.unzip l
       in do
         results <- executeBatch batch
         let l2 = zipExact results actions
         mapM_ (\(result,trigger) -> trigger result) l2

