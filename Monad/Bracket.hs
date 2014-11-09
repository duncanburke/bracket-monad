module Monad.Bracket (
  BracketT,
  bracketC,
  bracketC_,
  runBracketT
  ) where

import Monad.Cont
import Monad.Try

import Control.Monad.Trans.Cont (ContT(ContT), runContT)
import Control.Monad.Trans.Class

type BracketT m = ContT () m

bracketC :: (MonadTry m) => m a -> (a -> m b) -> BracketT m a
bracketC start final = ContT $ bracket start final

bracketC_ :: (MonadTry m) => m a -> m b -> BracketT m a
bracketC_ start final = bracketC start $ const final

runBracketT :: (MonadTry m) => BracketT m a -> m ()
runBracketT b = runContT b (\_ -> return ())
