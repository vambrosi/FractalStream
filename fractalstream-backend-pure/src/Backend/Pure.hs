--{-# options_ghc -Wno-unused-matches #-}
module Backend.Pure
  ( interpretViewer
  ) where

import FractalStream.Prelude
import Language.Environment
import Language.Draw
import Language.Code
import Language.Code.InterpretIO
import Language.Value.Evaluator (HaskellValue)
import Actor.Viewer
import Actor.Field (ContinuationField(..), overrideFromField, reprojectIndex, markHasSeed)
import Data.Indexed.Functor
import Data.Color (colorToRGB, grey)

import Foreign hiding (void)
import Data.IORef

interpretViewer :: forall env t. MissingViewerArgs env
                => Maybe (PrepScript env)
                -> Code (ViewerEnv env)
                -> (ViewerFunction env -> IO t) -> IO t
interpretViewer mPrepScript body action = do
  let env = toIndex body
  withEnvironment env $ action $ ViewerFunction $ \ViewerArgs{..} -> do
    let (x0, y0) = vaPoint
        (dx, dy) = vaStep
        context :: Context HaskellValue (ViewerEnv env)
        context = Bind (Proxy @InternalX)  RealType  x0
                $ Bind (Proxy @InternalY)  RealType  y0
                $ Bind (Proxy @InternalDX) RealType  dx
                $ Bind (Proxy @InternalDY) RealType  dy
                $ Bind (Proxy @"color")    ColorType grey
                $ vaArgs

    forM_ (zip [0 .. vaWidth - 1] [y0, y0 - dy ..]) $ \(j, y) -> do
      forM_ (zip [0 .. vaHeight - 1] [x0, x0 + dx ..]) $ \(i, x) -> do

        iorefs :: Context IORefTypeOfBinding (ViewerEnv env) <-
          mapContextM (\_ _ -> newIORef) context

        -- If a continuation field was computed for this tile, override the
        -- published output bindings with the field's value at this pixel
        -- (found by reprojecting the pixel coordinate onto the field grid).
        case vaContinuationField of
          Nothing -> pure ()
          Just (ContinuationField outEnv arrays geom) ->
            case reprojectIndex geom (x :+ y) of
              Nothing  -> pure ()
              Just idx -> do
                overrideFromField env outEnv arrays idx iorefs
                -- Mark that a field seed is available, so a `continuing` solve
                -- seeds from it rather than the cold-start anchor.
                markHasSeed env iorefs

        (r, g, b) <- fmap colorToRGB . flip evalStateT iorefs $ do
          update bindingEvidence (Proxy @InternalX) RealType x
          update bindingEvidence (Proxy @InternalY) RealType y
          -- Run prep script first (can Set prep output variables in context)
          case mPrepScript of
            Nothing -> pure ()
            Just (PrepScript _ prepCode) -> interpretToIO noDrawing prepCode
          interpretToIO noDrawing body
          eval (Var (Proxy @"color") ColorType bindingEvidence)

        let offset = fromIntegral (3 * (j * vaWidth + i))
        pokeByteOff vaBuffer (offset + 0) r
        pokeByteOff vaBuffer (offset + 1) g
        pokeByteOff vaBuffer (offset + 2) b

    pure ()

noDrawing :: DrawHandler ScalarIORefM
noDrawing = DrawHandler (\_ -> pure ())
