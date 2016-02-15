{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module DSL where

import Action

-- | DSL
-- | Used to construct a runtime represenation of the monadic/applicative code
-- | First type parameter indicates the DSL, second indicates the result type
data DSL (op :: * -> *) (a :: *) where
    Pure :: a -> DSL op a
    Single :: Action op a -> DSL op a
    MonadBind :: DSL op a -> (a -> DSL op b) -> DSL op b
    ApBind :: DSL op (a -> b) -> DSL op a -> DSL op b

instance Functor (DSL a) where
    fmap f (Pure a) = Pure $ f a
    fmap f (Single a) = Single $ fmap f a
    fmap f (MonadBind a b) = MonadBind a ( (fmap f) . b)
    fmap f (ApBind a b) = ApBind (fmap (f.) a) b

instance Monad (DSL a) where
    return x = Pure x
    a >>= b = MonadBind a b

instance Applicative (DSL a) where
    pure x = Pure x
    a <*> b = ApBind a b

