{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}

module Action where

-- | Action
-- | A wrapper type around a "DSL operation type", providing a Functor instance
-- | First type parameter indicates the DSL, second indicates the result type of the -executed- action.
data Action (op :: * -> *) (a :: *) = forall b. Action (op b) (b -> a) 

instance Functor (Action a) where
    fmap g (Action operation f) = Action operation (g.f)


