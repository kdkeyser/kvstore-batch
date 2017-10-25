{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Applicative
import Control.Concurrent.STM

import KVOperation
import Action
import Backend
--import MonadAction
import DSL

import qualified MemBackend
import qualified Aggregator
import qualified Future
import qualified TransactionBuilder
import qualified BatchedTransactionBuilder
import qualified Transaction

-- | Generic runner
run :: (Monad m, Functor m, Applicative m) => (forall c. b c -> m c) -> DSL b a -> m a
run runDSL m =
    case m of
        Pure x ->
            return x
        Single (Action instruction f) ->
            fmap f $ runDSL instruction
        MonadBind a b ->
            run runDSL a >>= \result -> run runDSL $ b result
        ApBind a b ->
            run runDSL a <*> run runDSL b

-- | Execute optimzed: queue all backend operations on the Aggregator, wrap the result in a Future and only force the
-- | evaluation of the Future when we need the result. Piggy-back all outstanding operations on the back-end operation
-- | needed to force the evaluation of that specific Future.
recRunBatched :: (forall b. (c b -> IO (Future.FutureT IO b))) -> DSL c a -> IO (Future.FutureT IO a)
recRunBatched aggregate m =
    case m of
        Pure x -> return $ pure x
        Single (Action action f) -> do
            future <- aggregate action
            return $ fmap f future
        MonadBind a b -> do
            future <- recRunBatched aggregate a
            result <- Future.force future
            recRunBatched aggregate $ b result
        ApBind a b -> do
            future1 <- recRunBatched aggregate a
            future2 <- recRunBatched aggregate b
            return $ future1 <*> future2

runBatched :: Backend -> DSL KVOperation a -> IO a
runBatched backend m = do
  aggregator <- Aggregator.create backend
  result <- recRunBatched (Aggregator.add aggregator) m
  Future.force result

buildTransactionBatched :: Backend -> DSL KVOperation a -> IO (a, Transaction.Transaction)
buildTransactionBatched backend m = do
  aggregator <- Aggregator.create backend
  transactionBuilder <- BatchedTransactionBuilder.create aggregator
  futureResult <- recRunBatched (BatchedTransactionBuilder.add transactionBuilder) m
  result <- Future.force futureResult
  transaction <- BatchedTransactionBuilder.toTransaction transactionBuilder
  return (result, transaction)
  
-- | Build a transaction based on the GET/PUT operations
-- | Gather all the input data (i.e. GET's on keys that were not yet modified in this transaction)
-- | Return a list of ASSERTS and PUTS which defines the transaction
buildTransaction :: Backend -> DSL KVOperation a -> IO (a, Transaction.Transaction)
buildTransaction backend m = do
    transactionBuilder <- atomically $ TransactionBuilder.create backend
    result <- run (TransactionBuilder.add transactionBuilder) m
    transaction <- atomically $ TransactionBuilder.toTransaction transactionBuilder
    return (result, transaction)

-- | EXAMPLES

-- | Building blocks, shorthand for the 2 basic operations
get :: String -> DSL KVOperation String
get key = Single $ Action (Get key) id

put :: String -> String -> DSL KVOperation ()
put key value = Single $ Action (Put key value) (const ())

getBatchSampleBackend :: IO Backend
getBatchSampleBackend = do
   backend <- MemBackend.create
   runBatched backend $
        put "user_7_pass" "7DFJ45FDXM" *>
        put "user_7_site" "www.haskell.org" *>
        put "user_12_pass" "DFGRED43S" *>
        put "user_12_site" "www.ocaml.org" *>
        put "user_18_pass" "DSZWQ52VB23V" *>
        put "user_18_site" "www.scala.org" *>
        put "default_site" "www.zombo.com"
   return backend

getBatch :: DSL KVOperation (String,String,String)
getBatch = (,,) <$>
  get "user_7_pass" <*>
  get "user_12_pass" <*>
  get "user_18_pass" >>=
    \(pass7, pass12, pass18) -> (,,) <$>
          (if (check 7 pass7)  then get "user_7_site"  else get "default_site") <*>
      (if (check 12 pass12) then get "user_12_site" else get "default_site") <*>
        (if (check 18 pass18) then get "user_18_site" else get "default_site")
  where
    check 7 _  = True
    check 12 _ = False
    check 18 _ = False

getBatchDo :: DSL KVOperation (String,String,String)
getBatchDo = do
  pass7 <-  get "user_7_pass"
  pass12 <- get "user_12_pass"
  pass18 <- get "user_18_pass"
  site7 <- if (check 7 pass7) then get "user_7_site"  else get "default_site"
  site12 <- if (check 12 pass12) then get "user_12_site" else get "default_site"
  site18 <- if (check 18 pass18) then get "user_18_site" else get "default_site"
  pure (site7, site12, site18)
  where
    check 7 _  = True
    check 12 _ = False
    check 18 _ = False

getBatchDoReorder :: DSL KVOperation (String,String,String)
getBatchDoReorder = do
  pass7 <-  get "user_7_pass"
  site7 <- if (check 7 pass7) then get "user_7_site"  else get "default_site"
  pass12 <- get "user_12_pass"
  site12 <- if (check 12 pass12) then get "user_12_site" else get "default_site"
  pass18 <- get "user_18_pass"
  site18 <- if (check 18 pass18) then get "user_18_site" else get "default_site"
  pure (site7, site12, site18)
  where
    check 7 _  = True
    check 12 _ = False
    check 18 _ = False

batchStepDo :: String -> (String -> String -> Bool) -> DSL KVOperation String
batchStepDo userId check = do
  pass <- get $ "user_" ++ userId ++ "_pass"
  site <- if (check userId pass) then get $ "user_" ++ userId ++ "_site" else get "default_site"
  return site

getBatchDoStep :: DSL KVOperation (String,String,String)
getBatchDoStep = do
  site7 <- batchStepDo "7" check
  site12 <- batchStepDo "12" check
  site18 <- batchStepDo "18" check
  pure (site7, site12, site18)
  where
    check "7" _  = True
    check "12" _ = False
    check "18" _ = False

addUserSampleBackend :: IO Backend
addUserSampleBackend = do
   backend <- MemBackend.create
   runBatched backend $
        put "user_7_pass" "7DFJ45FDXM" *>
        put "user_7_site" "www.haskell.org" *>
        put "user_12_pass" "DFGRED43S" *>
        put "user_12_site" "www.ocaml.org" *>
        put "user_count" "17"
   return backend

addUser :: String ->  String -> DSL KVOperation ()
addUser password site = do
  numberOfUsers <- read <$> get "user_count"
  let nextUser = show $ numberOfUsers + 1
  put ("user_" ++ nextUser ++ "_pass") password
  put ("user_" ++ nextUser ++ "_site") site
  put "user_count" nextUser
 
batchExample :: IO ()
batchExample = do
  putStrLn "Batching example"
  putStrLn "\nFill backend"
  sampleBackend <- getBatchSampleBackend
  
  putStrLn "\nStart batched run"
  _result <- runBatched sampleBackend getBatchDoStep
  putStrLn "\nBatched run done"
  return ()

transactionExample :: IO ()
transactionExample = do
  putStrLn "Transaction example"
  putStrLn "\nFill backend"
  sampleBackend <- addUserSampleBackend
  putStrLn "\nStart transaction build"
  (_result, transaction) <- buildTransactionBatched sampleBackend $ addUser "rickAE870D" "www.clojure.org"
  putStrLn $ show transaction
  putStrLn "\nTransaction example done"

main :: IO ()
main = do
  batchExample
  --transactionExample

