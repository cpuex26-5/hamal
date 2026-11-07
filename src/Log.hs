module Log (Log, logMsg, runLog) where

import Control.Monad (when)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, send)
import System.IO (hPutStrLn, stderr)

data Log :: Effect where
  LogMsg :: String -> Log m ()

type instance DispatchOf Log = Dynamic

logMsg :: (HasCallStack, Log :> es) => String -> Eff es ()
logMsg = send . LogMsg

runLog :: (IOE :> es) => Bool -> Eff (Log : es) a -> Eff es a
runLog verbose = interpret $ \_ -> \case
  LogMsg s -> when verbose $ liftIO (hPutStrLn stderr s)
