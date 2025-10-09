{-# options_ghc -Wno-type-defaults #-}

{- |
Module      : Task.Block
Description : Blocks encapsulate the task of rendering a picture of a
              dynamical system to one or more tiles.
-}
module Task.Block
  ( Block(..)
  , BlockComputeAction
  , fillBlock
  , progressively
  ) where

import FractalStream.Prelude

import Task.Concurrent

import Control.Concurrent.MVar
import Foreign.ForeignPtr
import Control.Concurrent
import Data.Array.IO hiding (index)
import System.Random
import Data.Time (diffUTCTime, getCurrentTime)
import Foreign (Ptr, peekByteOff, pokeByteOff, allocaArray)

type BlockComputeAction = Word32 -- ^ Block width, in subsamples
                       -> Word32 -- ^ Block height, in subsamples
                       -> Word32 -- ^ Subsamples per point length
                       -> Complex Double -- ^ Initial (x,y) values
                       -> Complex Double -- ^ (dx,dy) values
                       -> Ptr Word8 -- ^ Output array of colors
                       -> IO ()

-- | A Block carries the information required to go from a
--   runnable dynamical system to a buffer filled with the
--   resulting color data.
data Block =
  Block { coordToModel  :: (Double, Double) -> (Double, Double)
        , compute       :: BlockComputeAction
        , logSampleRate :: Int
          -- ^ The rate of over- or under-sampling to use.
          --      * logSampleRate == 0: draw one point per pixel.
          --      * logSampleRate == N < 0: draw 2^-N by 2^-N pixel
          --          blocks per model point.
          --      * logSampleRate == N > 0: subsample each pixel
          --          on a 2^N by 2^N subgrid and average the
          --          results, for a smoother picture.
        , blockBuffer   :: Synchronizable (ForeignPtr Word8)
          -- ^ The pixel buffer to write into.
        , xStride       :: Int
          -- ^ The width of the pixel buffer.
        , x0            :: Int
          -- ^ The upper-left x coordinate of this block in the pixel buffer.
        , y0            :: Int
          -- ^ The upper-left y coordinate of this block in the pixel buffer.
        , deltaX        :: Double
          -- ^ The model size of a pixel in the X direction
        , deltaY        :: Double
          -- ^ The model size of a pixel in the Y direction
        , xSize         :: Int
          -- ^ The width of the block, in subsamples.
        , ySize         :: Int
          -- ^ The height of the block, in subsamples
        , shouldRedraw  :: MVar ()
          -- ^ A variable used to signal that this block is complete
          --   and the pixel buffer should be redrawn.
        }

-- | Render the block into its output buffer.
fillBlock :: Block -> IO ()
fillBlock Block{..} = do
    let skip = if logSampleRate < 0 then 2^(negate logSampleRate) else 1
        k    = if logSampleRate > 0 then 2^logSampleRate          else 1

    let points    = [ (x,y) | y <- [0, skip .. ySize - 1]
                            , x <- [0, skip .. xSize - 1] ]
        uv_points = [ (fromIntegral (x0 + x), fromIntegral (y0 + y))
                    | (x,y) <- points ]
        indexOf (u,v) = u + v * xStride


    -- Run the computation on each subsampled point.
    allocaArray (length points * 3) $ \tmp -> do
      compute (fromIntegral (xSize `div` skip))
              (fromIntegral (ySize `div` skip))
              (floor k)
              (deltaX :+ deltaY)
              (let (u,v) = coordToModel (fromIntegral x0, fromIntegral y0)
               in u :+ v)
              tmp

      -- Resample the results
      --let rgbs = resampleBy averageColor nSubsamples results

      -- Fill target buffer with result colors.
      with blockBuffer $ \buffer -> withForeignPtr buffer $ \ptr -> do
        forM_ (zip uv_points [0..]) $ \((u,v), ix) -> do
          let index = floor $ u + v * fromIntegral xStride
          forM_ [indexOf (du,dv) | dv <- [0 .. skip - 1]
                                 , du <- [0 .. skip - 1] ] $ \offset -> do
            peekByteOff @Word8 tmp (3*ix + 0) >>= pokeByteOff ptr (3*index + offset + 0)
            peekByteOff @Word8 tmp (3*ix + 1) >>= pokeByteOff ptr (3*index + offset + 1)
            peekByteOff @Word8 tmp (3*ix + 2) >>= pokeByteOff ptr (3*index + offset + 2)

      -- Completed the block, signal for a redraw
      void (tryPutMVar shouldRedraw ())

-- | Chop up a block into sub-blocks, delegate rendering
--   tasks for sub-blocks, and blit the results back
--   into an image.
progressively :: (Block -> IO ()) -> (Block -> IO ())
progressively render block = do

    let subBlockSize = 16
        width   = xSize block
        height  = ySize block
        xBlocks = let (z,r) = width `divMod` subBlockSize
                  in zip [0 .. z - 1] (repeat subBlockSize) ++
                     [(z,r) | r > 0]
        yBlocks = let (z,r) = height `divMod` subBlockSize
                  in zip [0 .. z - 1] (repeat subBlockSize) ++
                     [(z,r) | r > 0]

    subblocks <- shuffle [(x,y) | x <- xBlocks, y <- yBlocks]

    poolSize <- subtract 1 <$> getNumCapabilities

    when False $ do
      caps <- getNumCapabilities
      putStrLn $ show caps ++ " capabilities, pool size " ++ show poolSize ++ " (w=" ++ show (xSize block) ++ ", h=" ++ show (ySize block) ++ ")"

    let rates = [logSampleRate block] -- FIXME filter (<= logSampleRate block) [-4, -2, 0, 1] --, logSampleRate block]

    let todo = [(rate, x, y) | rate <- rates, (x,y) <- subblocks]
    when False $ putStrLn $ "***** start @ rates=" ++ show rates
    start <- getCurrentTime

    forPool_ poolSize todo $ \(rate,(x,w),(y,h)) -> do
      render $ block { xSize = w, ySize = h,
                       x0 = subBlockSize * x, y0 = subBlockSize * y,
                       logSampleRate = rate }

    end <- getCurrentTime
    when False $ do
      putStrLn $ "***** " ++ show width ++ " x " ++ show height ++ " @ rates " ++ show rates
               ++ " rendered in " ++ show (diffUTCTime end start)

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- makeArray xs
        forM_ [2..n] $ \i -> do
            j <- randomRIO (1,i)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            writeArray ar i vj
        forM [1..n] (readArray ar)
  where
    n = length xs
    makeArray :: [a] -> IO (IOArray Int a)
    makeArray = newListArray (1,n)
