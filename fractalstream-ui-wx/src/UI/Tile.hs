{- |
Module      : UI.Tile
Description : Creation and execution of viewer tiles.
-}
module UI.Tile ( Tile()
               , renderTile
               , cancelTile
               , cancelTileSync
               , tileRect
               , ifModified
               , ifElseModified
               , withSynchedTileBuffer
               ) where

import FractalStream.Prelude

import Task.Block
import Task.Concurrent
import Data.Planar

import Data.Color

import Control.Concurrent
import Control.Concurrent.Async

import Foreign.ForeignPtr
import Foreign.Ptr

newtype ImagePoint = ImagePoint (Double,Double)

instance Planar ImagePoint where
    toCoords (ImagePoint (x,y)) = (x, y)
    fromCoords = ImagePoint

-- | A tile in the image viewer.
data Tile = Tile
    { imageRect        :: Rectangle ImagePoint
      -- ^ The region in view space described by the tile.
    , tileBuffer       :: Synchronizable (ForeignPtr Word8)
      -- ^ The buffer into which the tile will draw.
    , tileWorker       :: Async ()
      -- ^ The worker thread which is drawing this tile.
    , shouldRedrawTile :: MVar ()
      -- ^ A value which signals that the tile needs to be redrawn.
    , tileCancelled    :: MVar ()
      -- ^ Filled in the first time 'cancelTileSync' actually runs for this
      --   tile. A tile can legitimately be cancelled from more than one place
      --   (e.g. a window's own close handler, *and* the global pending-renders
      --   registry drained at app shutdown, if the window was rebuilt after a
      --   config change and the old registry entry was never removed) -- this
      --   makes a second call a safe no-op instead of a double 'cancel'.
    }

-- | Cancel the tile, but don't wait for it to finish.
cancelTile :: Tile -> IO ()
cancelTile = void . forkIO . cancelTileSync

-- | Like 'cancelTile', but synchronous: blocks until the worker has actually
-- terminated before returning, instead of firing the cancellation off in the
-- background. 'cancel' from "Control.Concurrent.Async" already blocks until
-- the target thread is dead by design -- it just isn't safe to call directly
-- from a UI event handler for an in-progress render, which is why 'cancelTile'
-- wraps it in 'forkIO'.
--
-- Use this instead when the caller genuinely needs the worker gone before
-- proceeding, e.g. on window close, so nothing is still calling into a JIT
-- kernel whose code page is about to be unmapped.
--
-- Idempotent: only the first call for a given 'Tile' actually cancels
-- anything, so it's safe to call more than once on the same tile (see
-- 'tileCancelled').
cancelTileSync :: Tile -> IO ()
cancelTileSync tile = do
  firstTime <- tryPutMVar (tileCancelled tile) ()
  when firstTime $ do
    cancel (tileWorker tile)

withSynchedTileBuffer :: Tile -> (Ptr Word8 -> IO b) -> IO b
withSynchedTileBuffer tile action = synchedWith (tileBuffer tile) (`withForeignPtr` action)

-- | Unpack the width and height of a tile.
tileRect :: Tile -> (Int, Int)
tileRect tile = (floor w, floor h)
    where (w, h) = dimensions $ imageRect tile

-- | Perform an action, but only if the tile needs to be redrawn.
ifModified :: Tile -> IO () -> IO ()
ifModified tile f = do
    redraw <- tryTakeMVar $ shouldRedrawTile tile
    case redraw of
        Nothing -> return ()
        Just _  -> f

-- | Perform an action, but only if the tile needs to be redrawn.
-- Otherwise, perform a fallback action
ifElseModified :: Tile -> IO a -> IO a -> IO a
ifElseModified tile yes no = do
    redraw <- tryTakeMVar $ shouldRedrawTile tile
    case redraw of
        Nothing -> no
        Just _  -> yes

-- | Construct a tile from a dynamical system, and begin drawing to it.
renderTile :: Bool -- ^ Use smoothing?
           -> BlockComputeAction -- ^ The rendering action
           -> (Int, Int)   -- ^ The height and width of this tile.
           -> Rectangle (Double, Double)
              -- ^ The region of the dynamical plane corresponding
              --   to this tile.
           -> IO Tile      -- ^ An action which allocates the tile and
                           --   forks a task which draws into it.

renderTile smooth renderingAction (width, height) mRect = do

    -- Allocate an red/green/blue pixel byte for each point in the tile
    buf <- mallocForeignPtrBytes (3 * width * height)

    -- Initial fill of the image
    withForeignPtr buf $ \ptr ->
      sequence_ [ pokeColor ptr index grey | index <- [0 .. width * height - 1] ]

    let iRect = rectangle (ImagePoint (0,0))
                          (ImagePoint (fromIntegral width, fromIntegral height))

    redraw     <- newMVar ()  -- used to request a redraw
    managedBuf <- synchronized buf

    let (mRectWidth, mRectHeight) = dimensions mRect

    worker <- async $ progressively fillBlock
                    $ Block { coordToModel = convertRect iRect mRect . fromCoords
                            , compute = renderingAction
                            , logSampleRate = if smooth then 1 else 0
                            , blockBuffer = managedBuf
                            , x0 = 0
                            , y0 = 0
                            , deltaX = mRectWidth / fromIntegral width
                            , deltaY = negate (mRectHeight / fromIntegral height)
                            , xStride = width
                            , xSize = width
                            , ySize = height
                            , shouldRedraw = redraw
                            }
    link worker

    cancelled <- newEmptyMVar

    return Tile { imageRect = iRect
                , tileBuffer = managedBuf
                , tileWorker = worker
                , shouldRedrawTile = redraw
                , tileCancelled = cancelled
                }
