module MonadAction where

import Action

data MonadAction op a =   Pure a
                        | DSL (Action op (MonadAction op a))

instance Monad (MonadAction op) where
  return x = Pure x
  Pure x >>= g = g x
  DSL x >>= g = DSL (fmap (>>= g) x)

-- Needed for GHC 7.10 (Functor-Applicactive-Monad type class hierarchy)
instance Applicative (MonadAction op) where
  pure = Pure
  f1 <*> f2 = f1 >>= \v1 -> f2 >>= (pure . v1)

instance Functor (MonadAction op) where
  fmap f x = x >>= (\x -> Pure $ f x) 
