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

data Action (a :: * -> *) (b :: *) = forall c. Action (a c) (c -> b)

data MonadApplicative (a :: * -> *) (b :: *) where
    Pure :: a b -> MonadApplicative a b
    MonadBind :: MonadApplicative a b -> (b -> MonadApplicative a c) -> MonadApplicative a c
    ApplicativeBind :: MonadApplicative a (b -> c) -> MonadApplicative a b -> MonadApplicative a c

type MonadApplicativeAction = MonadApplicative (Action SimpleAction)

instance Functor (Action a) where
    fmap f (Action s g) = Action s (f.g)

instance Functor a => Functor (MonadApplicative a) where
    fmap f (Pure a) = Pure $ fmap f a
    fmap f (MonadBind a b) = MonadBind a (fmap f . b)
    fmap f (ApplicativeBind a b) = ApplicativeBind (fmap (f.) a) b

instance Monad MonadApplicativeAction where
    return x = Pure $ Action None $ const x
    a >>= b = MonadBind a b

instance Applicative MonadApplicativeAction where
    pure x = Pure $ Action None $ const x
    a <*> b = ApplicativeBind a b

get :: String -> MonadApplicativeAction String
get key = Pure $ Action (Get key) id

put :: String -> String -> MonadApplicativeAction ()
put key value = Pure $ Action (Put key value) (const ())
{-
printMAA :: Backend.Backend -> MonadApplicativeAction a -> IO a
printMAA backend m =
    case m of
        Pure a@(Action None f) -> do
            putStrLn "None"
            return $ f ()
        Pure a@(Action (Get key) f) -> do
            value <- Backend.get backend key
            putStrLn $ "GET " ++ key ++ " = " ++ value
            return $ f value
        Pure a@(Action (Put key value ) f ) -> do
            Backend.put backend key value
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
-}
runBatched :: Aggregator.Aggregator -> MonadApplicativeAction a -> IO (Future.Future a)
runBatched aggregator m =
    case m of
        Pure (Action action f) -> do
            future <- Aggregator.add aggregator action
            return $ fmap f future
        MonadBind a b -> do
            future <- runBatched aggregator a
            result <- Future.force future
            runBatched aggregator $ b result
        ApplicativeBind a b -> do
            future1 <- runBatched aggregator a
            future2 <- runBatched aggregator b
            return $ future1 <*> future2

sample :: MonadApplicativeAction String
sample = do
  v1 <- get "key1"
  v2 <- get "key2"
  put ("key"++ v1) v2
  v3 <- get "key3"
  put ("key" ++ v3) "sinterklaas"
  get ("key" ++ v1)

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

-- | The main entry point.
main :: IO ()
main = do
    backend@(Backend batch) <- MemBackend.create
    batch [PutAction "key1" "1", PutAction "key2" "2", PutAction "key3" "3"]
    aggregator <- Aggregator.create backend
    resultFuture <- runBatched aggregator sample2
    result <- Future.force resultFuture
    print result


