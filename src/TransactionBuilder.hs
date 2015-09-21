{-# LANGUAGE GADTs #-}
module TransactionBuilder where

import Data.Map

import Control.Concurrent.STM
import Backend

import qualified SimpleAction

data Asserts = Asserts [(String, String)] deriving (Show)
data Puts = Puts [(String, String)] deriving (Show)
data Transaction = Transaction Asserts Puts deriving (Show)

emptyTransaction :: Transaction
emptyTransaction = Transaction (Asserts []) (Puts [])

type Cache = Map String String
data TransactionBuilder = TransactionBuilder (TVar Cache) (TVar Cache) Backend

create :: Backend -> STM TransactionBuilder
create backend = do
    tVarCache <- newTVar Data.Map.empty
    tVarPuts <- newTVar Data.Map.empty
    return $ TransactionBuilder tVarCache tVarPuts backend

toTransaction :: TransactionBuilder -> STM Transaction
toTransaction (TransactionBuilder tVarGets tVarPuts _) = do
    gets <- readTVar tVarGets
    puts <- readTVar tVarPuts
    return $ Transaction (Asserts $ Data.Map.assocs gets) (Puts $ Data.Map.assocs puts)
   
transactionBuilderGet :: TransactionBuilder -> String -> IO String
transactionBuilderGet (TransactionBuilder tVarGets _ (Backend batch)) key = do
    getsMap <- readTVarIO tVarGets
    case Data.Map.lookup key getsMap of
        Just value -> return value
        Nothing -> do
            [GetResult result] <- batch [GetAction key]
            atomically $ modifyTVar tVarGets $ Data.Map.insert key result
            return result

transactionBuilderPut :: TransactionBuilder -> String -> String -> IO ()
transactionBuilderPut (TransactionBuilder _ tVarPuts _) key value =
    atomically $ modifyTVar tVarPuts $ Data.Map.insert key value

add :: TransactionBuilder -> SimpleAction.SimpleAction a -> IO a
add transactionBuilder action =
    case action of
        SimpleAction.Get key -> transactionBuilderGet transactionBuilder key
        SimpleAction.Put key value -> transactionBuilderPut transactionBuilder key value
