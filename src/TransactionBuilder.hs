{-# LANGUAGE GADTs #-}
module TransactionBuilder where

import Data.Map
import Data.IORef
import Backend

import qualified SimpleAction

data CachedBackend = CachedBackend (Map String String) Backend
data CacheLookup = Hit | Miss

cachedBackendGet :: CachedBackend -> String -> IO (CachedBackend, CacheLookup, String)
cachedBackendGet cbackend@(CachedBackend cache backend@(Backend batch)) key = 
  case Data.Map.lookup key cache of
      Just value -> return (cbackend, Hit, value)
      Nothing -> do
                   [GetResult result] <- batch [GetAction key]
                   return (CachedBackend (Data.Map.insert key result cache) backend, Miss, result)

cachedBackendPut :: CachedBackend -> String -> String -> IO CachedBackend
cachedBackendPut (CachedBackend cache backend@(Backend batch)) key value = do
    [PutResult ()] <- batch [PutAction key value]
    return $ CachedBackend (Data.Map.insert key value cache) backend

data Transaction = Transaction [(String,String)] [(String,String)] deriving (Show)-- ASSERTS / PUTS

emptyTransaction :: Transaction
emptyTransaction = Transaction [] []

data TransactionBuilder = TransactionBuilder (IORef (CachedBackend,Transaction))

create :: Backend.Backend -> IO TransactionBuilder
create backend = do
    ioRef <- newIORef (CachedBackend Data.Map.empty backend, emptyTransaction)
    return $ TransactionBuilder ioRef

toTransaction :: TransactionBuilder -> IO Transaction
toTransaction (TransactionBuilder ioRef) = do
    (_,transaction) <- readIORef ioRef
    return transaction
    
transactionBuilderGet :: TransactionBuilder -> String -> IO String
transactionBuilderGet (TransactionBuilder ioRef) key = do
    (cachedBackend, _) <- readIORef ioRef
    (newCachedBackend, cacheLookup, value) <- cachedBackendGet cachedBackend key
    atomicModifyIORef ioRef $ \(_, Transaction asserts puts) ->
        case cacheLookup of
             Hit -> ((newCachedBackend, Transaction asserts puts), ())
             Miss -> ((newCachedBackend, Transaction (asserts ++ [(key,value)]) puts), ())
    return value

transactionBuilderPut :: TransactionBuilder -> String -> String -> IO ()
transactionBuilderPut (TransactionBuilder ioRef) key value = do
    (cachedBackend, _) <- readIORef ioRef     
    newCachedBackend <- cachedBackendPut cachedBackend key value
    atomicModifyIORef ioRef $ \(_, Transaction asserts puts) -> ((newCachedBackend, Transaction asserts (puts ++ [(key,value)])), ())
    return ()

add :: TransactionBuilder -> SimpleAction.SimpleAction a -> IO a
add transactionBuilder action =
    case action of
        SimpleAction.Get key -> transactionBuilderGet transactionBuilder key
        SimpleAction.Put key value -> transactionBuilderPut transactionBuilder key value
