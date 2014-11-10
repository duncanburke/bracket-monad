{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Monad.Bracket (
  BracketT,
  MonadBracket(..),
  runBracketT
  ) where

import Monad.Cont
import Monad.Try

import Control.Monad.Trans.Cont (ContT(ContT), runContT)
import Control.Monad.Trans.Class

import Control.Monad.Lift

type BracketT m a = (MonadTry m, Monad m) => ContT () m a

class (Monad m) => MonadBracket m where
  bracketC :: (Monad n, MonadTrans t, m ~ (t n)) => n a -> (a -> n b) -> m a
  bracketC_ :: (Monad n, MonadTrans t, m ~ (t n)) => n a -> n b -> m a
  bracketC_ start final = bracketC start $ const final

instance (MonadTry m) => MonadBracket (ContT () m) where
  bracketC start final = ContT $ bracket start final

runBracketT :: (MonadTry m) => BracketT m a -> m ()
runBracketT b = runContT b (\_ -> return ())
