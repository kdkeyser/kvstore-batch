{-# LANGUAGE GADTs #-}
module TransactionBuilder where

import Data.Map

import Control.Concurrent.STM
import Backend
import Transaction

import qualified SimpleAction

type Cache = Map String String
data TransactionBuilder = TransactionBuilder (TVar Cache) (TVar Cache) (TVar Cache) Backend

create :: Backend -> STM TransactionBuilder
create backend = do
    tVarCache <- newTVar Data.Map.empty
    tVarAsserts <- newTVar Data.Map.empty
    tVarPuts <- newTVar Data.Map.empty
    return $ TransactionBuilder tVarCache tVarAsserts tVarPuts backend

toTransaction :: TransactionBuilder -> STM Transaction
toTransaction (TransactionBuilder _ tVarAsserts tVarPuts _) = do
    asserts <- readTVar tVarAsserts
    puts <- readTVar tVarPuts
    return $ Transaction (Asserts $ Data.Map.assocs asserts) (Puts $ Data.Map.assocs puts)
   
transactionBuilderGet :: TransactionBuilder -> String -> IO String
transactionBuilderGet (TransactionBuilder tVarCache tVarAsserts _ (Backend batch)) key = do
    cache <- readTVarIO tVarCache
    case Data.Map.lookup key cache of
        Just value -> return value
        Nothing -> do
            [GetResult result] <- batch [GetAction key]
            atomically $ do
                modifyTVar tVarCache $ Data.Map.insert key result
                asserts <- readTVar tVarAsserts
                case Data.Map.lookup key asserts of
                  Just value -> return ()
                  Nothing -> modifyTVar tVarAsserts $ Data.Map.insert key result
                return result

transactionBuilderPut :: TransactionBuilder -> String -> String -> IO ()
transactionBuilderPut (TransactionBuilder tVarCache _ tVarPuts _) key value =
    atomically $ 
        mapM_ (\tVar -> modifyTVar tVar $ Data.Map.insert key value) [tVarCache, tVarPuts]

add :: TransactionBuilder -> SimpleAction.SimpleAction a -> IO a
add transactionBuilder action =
    case action of
        SimpleAction.Get key -> transactionBuilderGet transactionBuilder key
        SimpleAction.Put key value -> transactionBuilderPut transactionBuilder key value
