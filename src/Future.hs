module Future where

import Control.Applicative

data FutureT m a = Future (m ()) (m a) -- trigger / result

instance (Functor m) => Functor (FutureT m) where
    fmap f (Future trigger result) = Future trigger (fmap f result)

instance (Applicative m) => Applicative (FutureT m) where
    pure x = Future (pure ()) (pure x)
    (Future t1 r1) <*> (Future t2 r2) = Future trigger result
      where
        trigger = t1 *> t2
        result = r1 <*> r2

instance (Applicative m, Monad m) => Monad (FutureT m) where
    return x = pure x
    (Future t1 r1) >>= f = Future trigger result
      where
          trigger = t1
          result = do
              r <- r1
              let f2 = f r
              force f2

force :: (Monad m) => FutureT m a -> m a
force (Future trigger action) =
    trigger >> action