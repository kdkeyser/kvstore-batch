{-# LANGUAGE GADTs #-}
module BatchedTransactionBuilder where

import Prelude hiding (mapM)

import Data.Map.Strict as Data.Map
import Data.Traversable
import Control.Concurrent.STM
import Control.Applicative
import Transaction
import Future

import qualified KVOperation
import qualified Aggregator

type Cache = Map String (FutureT IO String)
data TransactionBuilder = TransactionBuilder (TVar Cache) (TVar Cache) (TVar Cache) Aggregator.Aggregator

create :: Aggregator.Aggregator -> IO TransactionBuilder
create aggregator = do
    tVarCache <- newTVarIO Data.Map.empty
    tVarAsserts <- newTVarIO Data.Map.empty
    tVarPuts <- newTVarIO Data.Map.empty
    return $ TransactionBuilder tVarCache tVarAsserts tVarPuts aggregator

toTransaction :: TransactionBuilder -> IO Transaction
toTransaction (TransactionBuilder _ tVarAsserts tVarPuts _) = do
    (asserts, puts) <- atomically $ do
          asserts <- readTVar tVarAsserts
          puts <- readTVar tVarPuts
          return (asserts,puts)
    evaluatedAsserts <- mapM force asserts
    evaluatedPuts <- mapM force puts
    return $ Transaction (Asserts $ Data.Map.assocs evaluatedAsserts) (Puts $ Data.Map.assocs evaluatedPuts)
   
transactionBuilderGet :: TransactionBuilder -> String -> IO (FutureT IO String)
transactionBuilderGet (TransactionBuilder tVarCache tVarAsserts _ aggregator) key = do
    cache <- readTVarIO tVarCache
    case Data.Map.lookup key cache of
        Just value -> return value
        Nothing -> do
            result <- Aggregator.add aggregator (KVOperation.Get key)
            atomically $ do
                modifyTVar tVarCache $ Data.Map.insert key result
                asserts <- readTVar tVarAsserts
                case Data.Map.lookup key asserts of
                  Just value -> return ()
                  Nothing -> modifyTVar tVarAsserts $ Data.Map.insert key result
                return result

transactionBuilderPut :: TransactionBuilder -> String -> String -> IO (FutureT IO ())
transactionBuilderPut (TransactionBuilder tVarCache _ tVarPuts _) key value =
    let futureValue = pure value in do
    atomically $ 
        mapM_ (\tVar -> modifyTVar tVar $ Data.Map.insert key futureValue) [tVarCache, tVarPuts]
    return $ pure ()

add :: TransactionBuilder -> KVOperation.KVOperation a -> IO (FutureT IO a)
add transactionBuilder action =
    case action of
        KVOperation.Get key -> transactionBuilderGet transactionBuilder key
        KVOperation.Put key value -> transactionBuilderPut transactionBuilder key value
