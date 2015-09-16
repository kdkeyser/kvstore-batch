{-# LANGUAGE GADTs #-}

module MemBackend where

import Data.Map
import Data.IORef
import Data.Maybe
import Backend

type MemBackend = IORef (Map String String)

create :: IO Backend
create = do
    ioRef <- newIORef Data.Map.empty
    return $ Backend $ batch ioRef

get :: MemBackend -> String -> IO String
get backend key = do
    kvMap <- readIORef backend
    let result = fromMaybe "null" $ Data.Map.lookup key kvMap
    putStrLn $ "GET " ++ key ++ " -> " ++ result
    return result

put :: MemBackend -> String -> String -> IO ()
put backend key value = do
    putStrLn $ "PUT " ++ key ++ " -> " ++ value
    modifyIORef backend $ \kvMap -> insert key value kvMap

batch :: MemBackend -> [BackendAction] -> IO [BackendResult]
batch backend [] = return []
batch backend actions = do
    putStrLn "MemBackend: start batch"
    result <- sequence $ Prelude.map executeAction actions
    putStrLn "MemBackend: end batch"
    return result
  where
    executeAction (GetAction key) = fmap GetResult $ get backend key
    executeAction (PutAction key value) = fmap PutResult $ put backend key value

