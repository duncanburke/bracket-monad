module Control.Exception.BracketM
       (BracketM,
        bracketM,
        bracketM_,
        runBracketM,
        lift) where

import Control.Monad.Cont
import Control.Exception

type BracketM a = ContT a IO (IO a)

bracketM :: IO a -> (a -> IO b) -> BracketM a
bracketM start final = ContT $ bracket start final

bracketM :: IO a -> IO b -> BracketM a
bracketM start final = bracketM start $ const final

runBracketM :: BracketM a -> IO a
runBracketM m = runContT m id
