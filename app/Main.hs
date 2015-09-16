{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

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
-- | A wrapper type around a DSL instructions type, providing a Functor instance
data Action (a :: * -> *) (b :: *) = forall c. Action (a c) (c -> b)

instance Functor (Action a) where
    fmap f (Action s g) = Action s (f.g)

-- | MonadApplicative
-- | Used to construct a runtime represenation of the monadic/applicative code
data MonadApplicative (a :: * -> *) (b :: *) where
    Pure :: a b -> MonadApplicative a b
    MonadBind :: MonadApplicative a b -> (b -> MonadApplicative a c) -> MonadApplicative a c
    ApplicativeBind :: MonadApplicative a (b -> c) -> MonadApplicative a b -> MonadApplicative a c

instance Functor a => Functor (MonadApplicative a) where
    fmap f (Pure a) = Pure $ fmap f a
    fmap f (MonadBind a b) = MonadBind a (fmap f . b)
    fmap f (ApplicativeBind a b) = ApplicativeBind (fmap (f.) a) b

-- | MonadApplicativeAction
-- | Monad / Applicative instances for the SimpleAction DSL: create a runtime (tree-like) representation
type MonadApplicativeAction = MonadApplicative (Action SimpleAction)

instance Monad MonadApplicativeAction where
    return x = Pure $ Action None $ const x
    a >>= b = MonadBind a b

instance Applicative MonadApplicativeAction where
    pure x = Pure $ Action None $ const x
    a <*> b = ApplicativeBind a b

-- | Building blocks, shorthand for the 2 basic operations
get :: String -> MonadApplicativeAction String
get key = Pure $ Action (Get key) id

put :: String -> String -> MonadApplicativeAction ()
put key value = Pure $ Action (Put key value) (const ())

-- | Execute naively, and dump the representation
printMAA :: Backend.Backend -> MonadApplicativeAction a -> IO a
printMAA backend@(Backend batch) m =
    case m of
        Pure (Action None f) -> do
            putStrLn "None"
            return $ f ()
        Pure (Action (Get key) f) -> do
            [GetResult value] <- batch [GetAction key]
            putStrLn $ "GET " ++ key ++ " = " ++ value
            return $ f value
        Pure (Action (Put key value ) f ) -> do
            batch [PutAction key value]
            putStrLn $ "PUT " ++ key ++ " -> " ++ value
            return $ f ()
        MonadBind a b -> do
            putStrLn "MonadBind"
            result <- printMAA backend a
            printMAA backend $ b result
        ApplicativeBind a b -> do
            putStrLn "ApplicativeBind"
            f <- printMAA backend a
            result <- printMAA backend b
            return $ f result

-- | Execute optimzed: queue all backend operations on the Aggregator, wrap the result in a Future and only force the
-- | evaluation of the Future when we need the result. Piggy-back all outstanding operations on the back-end operation
-- | needed to force the evaluation of the Future
recRun :: Aggregator.Aggregator -> MonadApplicativeAction a -> IO (Future.Future a)
recRun aggregator m =
    case m of
        Pure (Action action f) -> do
            future <- Aggregator.add aggregator action
            return $ fmap f future
        MonadBind a b -> do
            future <- recRun aggregator a
            result <- Future.force future
            recRun aggregator $ b result
        ApplicativeBind a b -> do
            future1 <- recRun aggregator a
            future2 <- recRun aggregator b
            return $ future1 <*> future2

run :: Backend -> MonadApplicativeAction a -> IO a
run backend m = do
  aggregator <- Aggregator.create backend
  result <- recRun aggregator m
  Future.force result

-- | Sample showing both monadic and applicative bind
sample2 :: MonadApplicativeAction (String, String, String, String)
sample2 =
  pure (,,) <*> 
  (get "key1") <*>
  (get "key2") <*>
  (get "key3") >>=
  (\(a,b,c) -> do
      v2 <- get ("key" ++ b)
      return (a,b,c,v2 ++ "_modified")
  )

-- | Small example
main :: IO ()
main = do
    backend@(Backend batch) <- MemBackend.create
    batch [PutAction "key1" "1", PutAction "key2" "2", PutAction "key3" "3"]
    result <- run backend sample2
    print result
