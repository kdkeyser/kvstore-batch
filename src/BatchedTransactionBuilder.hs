{-# LANGUAGE GADTs #-}
module BatchedTransactionBuilder where

import Prelude hiding (mapM)

import Data.Map.Strict as Data.Map
import Data.Traversable
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Trans
import Transaction
import Future

import qualified SimpleAction
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
    putStrLn "Building transaction"
    (asserts, puts) <- atomically $ do
          asserts <- readTVar tVarAsserts
          puts <- readTVar tVarPuts
          return (asserts,puts)
    putStrLn "Building transaction (2)"
    evaluatedAsserts <- mapM force asserts
    evaluatedPuts <- mapM force puts
    putStrLn "Building transaction (3)"
    return $ Transaction (Asserts $ Data.Map.assocs evaluatedAsserts) (Puts $ Data.Map.assocs evaluatedPuts)
   
transactionBuilderGet :: TransactionBuilder -> String -> FutureT IO String
transactionBuilderGet (TransactionBuilder tVarCache tVarAsserts _ aggregator) key = do
    cache <- liftIO $ readTVarIO tVarCache
    case Data.Map.lookup key cache of
        Just value -> value
        Nothing -> do
            let result = Aggregator.add aggregator (SimpleAction.Get key)
            liftIO $ atomically $ do
                modifyTVar tVarCache $ Data.Map.insert key result
                asserts <- readTVar tVarAsserts
                case Data.Map.lookup key asserts of
                  Just value -> return ()
                  Nothing -> modifyTVar tVarAsserts $ Data.Map.insert key result
            result

transactionBuilderPut :: TransactionBuilder -> String -> String -> FutureT IO ()
transactionBuilderPut (TransactionBuilder tVarCache _ tVarPuts _) key value =
    let futureValue = pure value in do
    liftIO $ atomically $ 
        mapM_ (\tVar -> modifyTVar tVar $ Data.Map.insert key futureValue) [tVarCache, tVarPuts]
    return ()

add :: TransactionBuilder -> SimpleAction.SimpleAction a -> FutureT IO a
add transactionBuilder action =
    case action of
        SimpleAction.Get key -> transactionBuilderGet transactionBuilder key
        SimpleAction.Put key value -> transactionBuilderPut transactionBuilder key value
