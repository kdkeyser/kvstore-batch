{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative
import Control.Concurrent.STM

import SimpleAction
import Backend

import qualified MemBackend
import qualified Aggregator
import qualified Future
import qualified TransactionBuilder
import qualified BatchedTransactionBuilder
import qualified Transaction

-- | Action
-- | A wrapper type around a "DSL instructions type", providing a Functor instance
-- | First type parameter indicates the DSL, second indicates the result type of the -executed- action.
data Action (a :: * -> *) (b :: *) = forall c. Action (a c) (c -> b) 

instance Functor (Action a) where
    fmap f (Action s g) = Action s (f.g)

-- | MonadApplicative
-- | Used to construct a runtime represenation of the monadic/applicative code
-- | First type parameter indicates the DSL, second indicates the result type
data MonadApplicative (a :: * -> *) (b :: *) where
    Value :: b -> MonadApplicative a b
    SingleAction :: Action a b -> MonadApplicative a b
    MonadBind :: MonadApplicative a b -> (b -> MonadApplicative a c) -> MonadApplicative a c
    ApplicativeBind :: MonadApplicative a (b -> c) -> MonadApplicative a b -> MonadApplicative a c

instance Functor (MonadApplicative a) where
    fmap f (Value a) = Value $ f a
    fmap f (SingleAction a) = SingleAction $ fmap f a
    fmap f (MonadBind a b) = MonadBind a ( (fmap f) . b)
    fmap f (ApplicativeBind a b) = ApplicativeBind (fmap (f.) a) b

instance Monad (MonadApplicative a) where
    return x = Value x
    a >>= b = MonadBind a b

instance Applicative (MonadApplicative a) where
    pure x = Value x
    a <*> b = ApplicativeBind a b

-- | Generic runner
run :: (Monad m, Functor m, Applicative m) => (forall c. b c -> m c) -> MonadApplicative b a -> m a
run runDSL m =
    case m of
        Value x ->
            return x
        SingleAction (Action instruction f) ->
            fmap f $ runDSL instruction
        MonadBind a b ->
            run runDSL a >>= \result -> run runDSL $ b result
        ApplicativeBind a b ->
            run runDSL a <*> run runDSL b

-- | Execute optimzed: queue all backend operations on the Aggregator, wrap the result in a Future and only force the
-- | evaluation of the Future when we need the result. Piggy-back all outstanding operations on the back-end operation
-- | needed to force the evaluation of that specific Future.
recRunBatched :: (forall b. (c b -> IO (Future.FutureT IO b))) -> MonadApplicative c a -> IO (Future.FutureT IO a)
recRunBatched aggregate m =
    case m of
        Value x -> return $ pure x
        SingleAction (Action action f) -> do
            future <- aggregate action
            return $ fmap f future
        MonadBind a b -> do
            future <- recRunBatched aggregate a
            result <- Future.force future
            recRunBatched aggregate $ b result
        ApplicativeBind a b -> do
            future1 <- recRunBatched aggregate a
            future2 <- recRunBatched aggregate b
            return $ future1 <*> future2

runBatched :: Backend -> MonadApplicative SimpleAction a -> IO a
runBatched backend m = do
  aggregator <- Aggregator.create backend
  result <- recRunBatched (Aggregator.add aggregator) m
  Future.force result

buildTransactionBatched :: Backend -> MonadApplicative SimpleAction a -> IO (a, Transaction.Transaction)
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
buildTransaction :: Backend -> MonadApplicative SimpleAction a -> IO (a, Transaction.Transaction)
buildTransaction backend m = do
    transactionBuilder <- atomically $ TransactionBuilder.create backend
    result <- run (TransactionBuilder.add transactionBuilder) m
    transaction <- atomically $ TransactionBuilder.toTransaction transactionBuilder
    return (result, transaction)

-- | EXAMPLES

-- | Building blocks, shorthand for the 2 basic operations
get :: String -> MonadApplicative SimpleAction String
get key = SingleAction $ Action (Get key) id

put :: String -> String -> MonadApplicative SimpleAction ()
put key value = SingleAction $ Action (Put key value) (const ())

sampleBackend :: IO Backend
sampleBackend = do
   backend <- MemBackend.create
   runBatched backend $
        put "key1" "1" *>
        put "key2" "2" *>
        put "key3" "3" 
   return backend
 
-- | Example of automatic batching
batchExample :: IO ()
batchExample = do
    putStrLn "Batching example"
    putStrLn "\nFill backend"
    backend <- sampleBackend
    putStrLn "\nStart batch execution"

    _result <- runBatched backend $
          (,,) <$> 
          get "key1" <*>
          get "key2" <*>
          get "key3" >>=
          \(a,b,c) -> do
          v2 <- get ("key" ++ b)
          put v2 "test"
          d <- get v2
          return (a,b,c,d)
    return ()

-- | Example of transaction generation
transactionExample :: IO ()
transactionExample = do
    putStrLn "Transaction example"
    putStrLn "\nFill backend"
    backend <- sampleBackend
    putStrLn "\nStart transaction build"

    (_result, transaction) <- buildTransactionBatched backend $ do
        v1 <- get "key1"
        put "key2" $ v1 ++ v1
        v2 <- get "key2"
        put "key2" "rdn"
        v3 <- get "key3"
        return (v1,v2,v3)

    print transaction


-- | Example of transaction generation
transactionExample2 :: IO ()
transactionExample2 = do
    putStrLn "Transaction example"
    putStrLn "\nFill backend"
    backend <- sampleBackend
    putStrLn "\nStart transaction build"

    (_result, transaction) <- buildTransactionBatched backend $
          (,,) <$> 
          get "key1" <*>
          get "key2" <*>
          get "key3" >>=
          \(a,b,c) -> do
          v2 <- get ("key" ++ b)
          put v2 "test"
          d <- get v2
          return (a,b,c,d)

    print transaction
main :: IO ()
main = do
    --batchExample
    --putStrLn ""
    transactionExample2
