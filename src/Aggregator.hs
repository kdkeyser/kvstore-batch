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

add :: Aggregator -> SimpleAction.SimpleAction a -> IO (Future.Future a)
add aggregator@(entriesRef, _backend) action =
    case action of
        SimpleAction.Get key -> do
            mVar <- newEmptyMVar
            let getAction = GetAction key
            let trigger (GetResult result) = putMVar mVar result
            atomicModifyIORef entriesRef (\entries -> ((getAction,trigger):entries, ()))
            return $ Future.Future (execute aggregator) (takeMVar mVar)
        SimpleAction.Put key value -> do
            mVar <- newEmptyMVar
            let putAction = PutAction key value
            let trigger (PutResult result) = putMVar mVar result
            atomicModifyIORef entriesRef (\entries -> ((putAction,trigger):entries, ()))
            return $ Future.Future (execute aggregator) (takeMVar mVar)

execute :: Aggregator -> IO ()
execute (entriesRef, Backend executeBatch) = do
   l <- atomicModifyIORef entriesRef (\entries -> ([], entries))
   executeEntries $ Data.List.reverse l
 where
   executeEntries [] = return ()
   executeEntries l =
       let batch = map fst l
           actions = map snd l
       in do
         results <- executeBatch batch
         let l2 = zipExact results actions
         mapM_ (\(result,trigger) -> trigger result) l2

