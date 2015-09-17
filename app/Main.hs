{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import SimpleAction
import Backend

import qualified MemBackend
import qualified Aggregator
import qualified Future

-- | Action
-- | A wrapper type around a "DSL instructions type", providing a Functor instance
-- | First type parameter indicates the DSL, second indicates the result type of the -executed- action.
data Action (a :: * -> *) (b :: *) = forall c. Action (a c) (c -> b) | Value b

instance Functor (Action a) where
    fmap f (Action s g) = Action s (f.g)
    fmap f (Value b) = Value $ f b

-- | MonadApplicative
-- | Used to construct a runtime represenation of the monadic/applicative code
-- | First type parameter indicates the DSL, second indicates the result type
data MonadApplicative (a :: * -> *) (b :: *) where
    Pure :: Action a b -> MonadApplicative a b
    MonadBind :: MonadApplicative a b -> (b -> MonadApplicative a c) -> MonadApplicative a c
    ApplicativeBind :: MonadApplicative a (b -> c) -> MonadApplicative a b -> MonadApplicative a c

instance Functor (MonadApplicative a) where
    fmap f (Pure a) = Pure $ fmap f a
    fmap f (MonadBind a b) = MonadBind a ( (fmap f) . b)
    fmap f (ApplicativeBind a b) = ApplicativeBind (fmap (f.) a) b

instance Monad (MonadApplicative a) where
    return x = Pure $ Value x
    a >>= b = MonadBind a b

instance Applicative (MonadApplicative a) where
    pure x = Pure $ Value x
    a <*> b = ApplicativeBind a b

-- | Generic runner
run :: (forall c. b c -> IO c) -> MonadApplicative b a -> IO a
run runDSL m =
    case m of
        Pure (Value x) -> do
            putStrLn "Value"
            return x
        Pure (Action instruction f) -> do
            putStrLn "DSL instruction"
            value <- runDSL instruction
            return $ f value
        MonadBind a b -> do
            putStrLn "MonadBind"
            result <- run runDSL a
            run runDSL $ b result
        ApplicativeBind a b -> do
            putStrLn "ApplicativeBind"
            f <- run runDSL a
            result <- run runDSL b
            return $ f result

-- | Building blocks, shorthand for the 2 basic operations
get :: String -> MonadApplicative SimpleAction String
get key = Pure $ Action (Get key) id

put :: String -> String -> MonadApplicative SimpleAction ()
put key value = Pure $ Action (Put key value) (const ())

-- | Execute optimzed: queue all backend operations on the Aggregator, wrap the result in a Future and only force the
-- | evaluation of the Future when we need the result. Piggy-back all outstanding operations on the back-end operation
-- | needed to force the evaluation of that specific Future.
recRunSimpleAction :: Aggregator.Aggregator -> MonadApplicative SimpleAction a -> IO (Future.Future a)
recRunSimpleAction aggregator m =
    case m of
        Pure (Value x) -> return $ pure x
        Pure (Action action f) -> do
            future <- Aggregator.add aggregator action
            return $ fmap f future
        MonadBind a b -> do
            future <- recRunSimpleAction aggregator a
            result <- Future.force future
            recRunSimpleAction aggregator $ b result
        ApplicativeBind a b -> do
            future1 <- recRunSimpleAction aggregator a
            future2 <- recRunSimpleAction aggregator b
            return $ future1 <*> future2

runSimpleAction :: Backend -> MonadApplicative SimpleAction a -> IO a
runSimpleAction backend m = do
  aggregator <- Aggregator.create backend
  result <- recRunSimpleAction aggregator m
  Future.force result

-- | Sample showing both monadic and applicative bind
sample2 :: MonadApplicative SimpleAction (String, String, String, String)
sample2 =
  (,,) <$> 
  get "key1" <*>
  get "key2" <*>
  get "key3" >>=
  (\(a,b,c) -> do
      v2 <- get ("key" ++ b)
      return (a,b,c,v2 ++ "_modified")
  )

-- | Small example
main :: IO ()
main = do
    backend@(Backend batch) <- MemBackend.create

    -- fill the backend with some sample data
    runSimpleAction backend $
        put "key1" "1" *>
        put "key2" "2" *>
        put "key3" "3"

    result <- runSimpleAction backend sample2
    print result
