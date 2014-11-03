{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds #-}

module Monad.Bracket (
  BracketT,
  bracketC,
  bracketC_,
  runBracketC
  ) where

import Monad.Cont
import Monad.Try

import Control.Monad.Trans.Cont (ContT(ContT), runContT)
import Control.Monad.Trans.Class

-- type BracketT r m a = (MonadTry m) => ContT r m (m a)

-- bracketC :: (MonadTry m) => m a -> (a -> m b) -> BracketT r m a
-- bracketC init final = _ $ bracket init final

-- --bracketC_ :: (MonadTry m) => m a -> m b -> BracketT r m a
-- bracketC_ init final = bracketC init $ const final

-- runBracketC m = runContT m id

type BracketT r m a = ContT r m a

bracketC :: (MonadTry m) => m a -> (a -> m c) -> BracketT r m a
bracketC start final = ContT $ bracket start final

bracketC_ :: (MonadTry m) => m a -> m c -> BracketT r m a
bracketC_ start final = bracketC start $ const final

runBracketC :: (MonadTry m) => BracketT a m (m a) -> m a
runBracketC m = runContT m id
