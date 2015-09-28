module Future where

import Control.Applicative

data Future a = Future (IO ()) (IO a) -- trigger / result

instance Functor Future where
    fmap f (Future trigger result) = Future trigger (fmap f result)

instance Applicative Future where
    pure x = Future (return ()) (return x)
    (Future t1 r1) <*> (Future t2 r2) = Future trigger result
      where
        trigger = t1 >> t2
        result = r1 <*> r2

force :: Future a -> IO a
force (Future trigger action) =
    trigger >> action