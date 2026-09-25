{- |
Module      : UI.PendingRenders
Description : Global registry of in-flight renders, drained at app shutdown.

Each viewer window registers a "cancel my current render" action. They are
all run when the wx event loop exits, before the JIT session is torn down, so
no worker is still running compiled code that is about to be unmapped.

* Closing a viewer only hides it, so there is no per-window teardown hook.
* Cancelling a finished render is a no-op, so actions are never removed.
-}
module UI.PendingRenders
  ( PendingRenders
  , newPendingRenders
  , registerPendingRender
  , drainPendingRenders
  ) where

import Data.IORef

newtype PendingRenders = PendingRenders (IORef [IO ()])

newPendingRenders :: IO PendingRenders
newPendingRenders = PendingRenders <$> newIORef []

-- | Register an action that cancels a viewer window's current render. Call
-- once per viewer window, at creation time.
registerPendingRender :: PendingRenders -> IO () -> IO ()
registerPendingRender (PendingRenders ref) cancelAction =
  modifyIORef' ref (cancelAction :)

-- | Run every registered cancel action. Call once, after the wx event loop
-- has exited and before tearing down the JIT session.
drainPendingRenders :: PendingRenders -> IO ()
drainPendingRenders (PendingRenders ref) = readIORef ref >>= sequence_
