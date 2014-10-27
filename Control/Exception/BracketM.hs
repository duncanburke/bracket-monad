module Control.Exception.BracketM
       (BracketM,
        bracketM,
        bracketM_,
        runBracketM,
        lift) where

import Control.Monad.Cont
import Control.Exception

type BracketM r a = ContT r (IO) a

bracketM :: IO a -> (a -> IO c) -> BracketM r a
bracketM start final = ContT $ bracket start final

bracketM_ :: IO a -> IO c -> BracketM r a
bracketM_ start final = bracketM start $ const final

runBracketM :: BracketM a (IO a) -> IO a
runBracketM m = runContT m id
