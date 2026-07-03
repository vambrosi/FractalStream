{- |
Module      : UI.PendingRenders
Description : Global registry of in-flight renders, drained at app shutdown.

Per-viewer windows in this app are never actually destroyed when "closed" --
'WX.windowOnClose' just hides them, so a session can be re-shown later. That
means there's no reliable per-window teardown hook: the /only/ point that is
guaranteed to run exactly once, after every viewer window's event handlers
have stopped mattering, is when the wx event loop itself exits (Cmd+Q,
closing the last window, etc.) and 'Graphics.UI.WX.start' returns.

This registry lets each viewer window register a "cancel my current render"
action once, when it's created. 'drainPendingRenders' is called after 'start'
returns, right before the JIT session is torn down, so no render worker is
still calling into compiled kernel code whose pages are about to be unmapped.

Cancelling an already-finished render (e.g. a window that closed normally
earlier) is a safe no-op -- GHC's 'throwTo' does nothing when the target
thread has already terminated -- so actions never need to be deregistered.
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
