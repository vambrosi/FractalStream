{-# language OverloadedStrings, UndecidableInstances, NumericUnderscores #-}
{-# options_ghc -Wno-orphans #-}

module UI.ProjectViewer
  ( viewProject
  ) where

import Language.Environment
import Language.Type
import Data.Color (Color, colorToRGB)
import Control.Concurrent.MVar
import Control.Monad.State hiding (get)

import Graphics.UI.WX hiding (pt, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WXCore.Events as WX
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.Draw
import           Graphics.UI.WXCore.WxcClassTypes
import           Graphics.UI.WXCore.WxcTypes      (rgba)
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Planar

import UI.Tile
import UI.Layout

import Data.IORef
import Data.Word

import Actor.UI
import Actor.Tool
import Actor.Viewer.Complex
import Actor.Event (Event(..))
import Language.Effect.Draw

import Data.DynamicValue
import Language.Value (Value, ValueF(..))
import Language.Code (CodeF(..))
import Task.Block (BlockComputeAction)

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits
import Data.Kind
import Data.Indexed.Functor
import Fcf (Exp, Eval)

viewProject :: (forall a. Frame a -> [Menu ()] -> IO ())
            -> UI
viewProject addMenuBar = UI
  { newEnsemble = pure ()

  , runSetup = \_ title setupUI continue -> do
      f <- frame [ text := title
                 , on resize := propagateEvent
                 ]

      addMenuBar f []
      WX.windowOnClose f (set f [ visible := False ])

      innerLayout <- generateWxLayout f setupUI
      compileButton <- button f [ text := "Go!"
                                , on command := do
                                    set f [ visible := False ]
                                    continue
                                ]
      set f [ layout := fill . margin 5 . column 5
              $ [ innerLayout, widget compileButton ]
            ]

  , makeLayout = \_ title ui -> do
      f <- frame [ text := title
                 , on resize := propagateEvent
                 ]

      WX.windowOnClose f (set f [ visible := False ])

      addMenuBar f []

      innerLayout <- generateWxLayout f ui
      set f [ layout := fill . margin 5 . column 5 $ [ innerLayout ] ]

  , makeViewer = const (makeWxComplexViewer addMenuBar)
  }

makeWxComplexViewer :: (forall a. Frame a -> [Menu ()] -> IO ())
                    -> ViewerUIProperties
                    -> ComplexViewer'
                    -> IO ()
makeWxComplexViewer
  addMenuBar
  vup@ViewerUIProperties{..}
  theViewer@ComplexViewer'{..} = do

    let clone = do
          newCV <- cloneComplexViewer theViewer
          makeWxComplexViewer addMenuBar vup newCV

    let (width, height) = (fst vpSize, snd vpSize)
    f <- frame [ text := vpTitle
               , on resize := propagateEvent
               ]
    WX.windowOnClose f (set f [ visible := False ])

    p <- panel f [ ]

    -- Viewer status bar
    status <- statusField [ text := "Pointer location" ]
    toolStatus <- statusField [text := ""]

    set f [ statusBar := [toolStatus, status]
          , layout := fill (minsize (sz 128 128) (widget p))
          , clientSize := sz width height
          ]

    -- `requestRefresh` can be used from any thread to queue up a
    -- window refresh.
    offThreadRefresh <- newEmptyMVar
    let requestRefresh = void (tryPutMVar offThreadRefresh ())

    -- Build the initial view model
    model <- variable [value := Model (0,0) (1/128,1/128)]

    -- Get the tools
    let theTools = cvTools'

    -------------------------------------------------------
    -- Main view
    -------------------------------------------------------

    -- trigger repaint
    let triggerRepaint = do
          isVisible <- get f visible
          when isVisible $ do
            repaint p
            windowRefresh p True -- True=redraw background
            windowUpdateWindowUI p

    renderId <- newIORef (0 :: Int)

    draggedTo <- variable [value := Nothing]
    lastClick <- variable [value := Nothing]
    pendingResize <- variable [value := False]

    viewerTile     <- do
      renderAction <- cvGetFunction
      renderTile' renderId renderAction (width, height) model
    currentTile    <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]
    lastTileImage  <- variable [value := Nothing]
    animate        <- variable [value := Nothing]
    currentToolIndex <- variable [value := Nothing]

    let startAnimatingFrom oldModel = do
            now <- getCurrentTime
            img <- get savedTileImage value >>= traverse imageCopy
            set lastTileImage [value := img]
            set animate [value := Just (now, oldModel, img)]

    -- Convert back and forth between viewport coordinates and coordinates
    -- in the underlying model (e.g. viewport coordinates to C-plane coordinates in
    -- the dynamical system or parameter plane)
    let viewToModel pt = do
            Size { sizeW = w, sizeH = h } <- get f clientSize
            let dim = (w,h)
                fullViewRect = rectangle (Viewport (0,0)) (Viewport dim)
            modelRect <- modelToRect @(Double,Double) dim <$> get model value
            pure (convertRect fullViewRect modelRect $ Viewport (pointX pt, pointY pt))
        modelToView mdl pt = do
          Size { sizeW = w, sizeH = h } <- get f clientSize
          let dim = (w,h)
              fullViewRect = rectangle (Viewport (0,0)) (Viewport dim)
          let modelRect = modelToRect @(Double,Double) dim mdl
          let Viewport (px, py) = convertRect modelRect fullViewRect pt
          pure (point px py)

    -- Set paint handlers
    set p [ on paintRaw := \dc r _dirty -> get animate value >>= \case
              Nothing -> do
                -- Normal paint. Draw current rendering, then layer
                -- tool imagery on top.
                viewRect <- windowGetViewRect f
                curTile <- get currentTile value
                let (w, h) = tileRect curTile
                get savedTileImage value >>= \case
                  Nothing -> pure ()
                  Just im -> drawCenteredImage im dc viewRect (w, h)

                mdl <- get model value
                get currentToolIndex value >>= \case
                  Nothing -> paintToolLayerWithDragBox (modelToView mdl) (modelPixelDim mdl) cvGetDrawCommands lastClick draggedTo dc r viewRect
                  Just{} -> paintToolLayer (modelToView mdl) (modelPixelDim mdl) cvGetDrawCommands dc

              Just (startTime, oldModel, oldImage) -> do
                -- Animated paint. Zoom and blend smoothly between
                -- the new and old images.
                now <- getCurrentTime
                let speed :: forall n. Num n => n
                    speed = 6
                    blend = min 255 (round (speed * 255 * toRational (diffUTCTime now startTime)) :: Integer)
                    t = min 1.0 (speed * fromRational (toRational (diffUTCTime now startTime)) :: Double)
                when (blend >= 255) (set animate [value := Nothing])
                curTile <- get currentTile value
                let (w, h) = tileRect curTile

                gc <- graphicsContextCreate dc
                newModel <- get model value
                let midModel = interpolateModel t oldModel newModel
                    withLayer (_opacity :: Double) action = do
                      -- FIXME: this used to do a crossfade but
                      -- graphicsContextBeginLayer seems to be gone
                      -- from wxWidgets 3.2. What is the replacement?
                      --graphicsContextBeginLayer gc opacity
                      action
                      --graphicsContextEndLayer gc
                    restoringContext action = do
                      graphicsContextPushState gc
                      action
                      graphicsContextPopState gc

                let zoom :: (Double, Double) -> (Double, Double) -> IO () -> IO ()
                    zoom (scaleX, scaleY) (cx, cy) action = do
                      restoringContext $ do
                        graphicsContextTranslate gc cx cy
                        graphicsContextScale gc (sz scaleX scaleY)
                        action

                    viewCenterX = fromIntegral w / 2
                    viewCenterY = fromIntegral h / 2
                    dx = (fst (modelCenter newModel) - fst (modelCenter oldModel))
                         / fst (modelPixelDim oldModel)
                    dy = negate (snd (modelCenter newModel) - snd (modelCenter oldModel))
                         / snd (modelPixelDim oldModel)

                -- draw the old image
                restoringContext $ do
                  let k = 1 / sqrt ( (fst (modelPixelDim oldModel) * snd (modelPixelDim oldModel))
                                   / (fst (modelPixelDim newModel) * snd (modelPixelDim newModel)))
                      t' = if (k - 1)^(2 :: Int) < 0.05 then t else (1 - k ** t) / (1 - k)
                  graphicsContextTranslate gc viewCenterX viewCenterY
                  graphicsContextScale gc (sz (fst (modelPixelDim oldModel)
                                              / fst (modelPixelDim midModel))
                                            (snd (modelPixelDim oldModel)
                                              / snd (modelPixelDim midModel)))
                  graphicsContextTranslate gc (negate viewCenterX) (negate viewCenterY)
                  graphicsContextTranslate gc (negate $ dx * t') (negate $ dy * t')

                  withLayer 1 $ restoringContext $ do
                    zoom (1.0, 1.0) (viewCenterX, viewCenterY) $
                      case oldImage of
                        Nothing -> pure ()
                        Just im -> do
                          drawImage dc im (WX.pt
                                           (round $ negate viewCenterX)
                                           (round $ negate viewCenterY))
                            []
                  -- draw the new image
                  withLayer (min 1 t) $ do
                    let zoomRatioX = fst (modelPixelDim newModel) / fst (modelPixelDim oldModel)
                        zoomRatioY = snd (modelPixelDim newModel) / snd (modelPixelDim oldModel)
                    restoringContext $ do
                      zoom (zoomRatioX, zoomRatioY)
                           (viewCenterX + dx, viewCenterY + dy)
                        $ get savedTileImage value >>= \case
                              Nothing -> pure ()
                              Just im -> do
                                drawImage dc im (WX.pt
                                              (round $ negate viewCenterX)
                                              (round $ negate viewCenterY)) []
                paintToolLayer (modelToView midModel) (modelPixelDim midModel) cvGetDrawCommands dc
          ]


    -- Set click and drag event handlers
    set p [ on mouse   := \case
              MouseLeftDown pt modifiers | isNoShiftAltControlDown modifiers -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                propagateEvent

              MouseLeftUp pt modifiers | isNoShiftAltControlDown modifiers -> do
                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing  -> get currentToolIndex value >>= \case
                      Nothing -> do
                        -- Completed a click, recenter to the clicked point.
                        Size { sizeW = w, sizeH = h } <- get f clientSize
                        oldModel <- get model value
                        newCenter <- viewToModel pt
                        set model [value := oldModel
                                    { modelCenter = toCoords newCenter }]

                        get currentTile value >>= cancelTile
                        renderAction <- cvGetFunction
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]

                        startAnimatingFrom oldModel
                        triggerRepaint
                      Just ix -> do
                        z <- viewToModel pt
                        toolEventHandler (theTools !! ix) (Click z)
                        cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint

                    Just box -> get currentToolIndex value >>= \case
                      Just ix -> do
                        get lastClick value >>= \case
                          Nothing -> pure ()
                          Just (Viewport vstart) -> do
                            zstart <- viewToModel (uncurry Point vstart)
                            z <- viewToModel pt
                            toolEventHandler (theTools !! ix) (DragDone z zstart)
                            cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                      Nothing -> do
                        -- Completed a drag. Zoom in to the dragged box, unless
                        -- the box is pathologically small; in that case, treat
                        -- the action as if it were a simple click.
                        oldModel <- get model value
                        Size { sizeW = w, sizeH = h } <- get f clientSize
                        newCenter <- viewToModel (viewportToPoint $ rectCenter box)
                        let (px, py) = modelPixelDim oldModel
                            (boxW, boxH) = dimensions box
                            oldArea = fromIntegral (w * h)
                            newArea = boxW * boxH
                            literalScale = sqrt (newArea / oldArea)
                            scale = if literalScale < 0.001 then 1 else literalScale
                        set model [value := oldModel
                                    { modelCenter = toCoords newCenter
                                    , modelPixelDim = (px * scale, py * scale)
                                    }]
                        get currentTile value >>= cancelTile
                        renderAction <- cvGetFunction
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]
                        startAnimatingFrom oldModel
                        triggerRepaint

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

              MouseLeftDrag pt modifiers | isNoShiftAltControlDown modifiers -> do
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                get currentToolIndex value >>= \case
                  Nothing -> do
                    mpt <- viewToModel pt
                    set status [text := show mpt]

                    dragBox <- getDragBox lastClick draggedTo
                    case dragBox of
                      Nothing -> return ()
                      Just _  -> triggerRepaint

                    propagateEvent

                  Just ix -> do
                     get lastClick value >>= \case
                       Nothing -> pure ()
                       Just (Viewport vstart) -> do
                         zstart <- viewToModel (uncurry Point vstart)
                         z <- viewToModel pt
                         toolEventHandler (theTools !! ix) (Drag z zstart)
                         cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                     propagateEvent


              MouseMotion pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                set status [text := show mpt]
                propagateEvent

              MouseLeftDClick pt modifiers | isNoShiftAltControlDown modifiers -> do
                get currentToolIndex value >>= \case
                   Nothing -> pure ()
                   Just ix -> do
                     z <- viewToModel pt
                     toolEventHandler (theTools !! ix) (DoubleClick z)
                     cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                     propagateEvent

              -- other mouse events
              _ -> propagateEvent
          ]

    -- Add a timer which will check for repainting requests from WX, ~10Hz
    _ <- timer f [ interval := 100
                 , enabled := True
                 , on command := do
                     curTile <- get currentTile value
                     ifModified curTile $ do
                       viewRect <- windowGetViewRect f
                       tileImage <- generateTileImage curTile viewRect
                       saved <- imageCopy tileImage
                       set savedTileImage [value := Just saved]
                       triggerRepaint
                 ]

    -- Add a timer which will check for repainting requests from off the main
    -- UI thread, ~10Hz
    _ <- timer f [ interval := 100
                 , enabled := True
                 , on command := do
                     needRefresh <- (== Just ()) <$> tryTakeMVar offThreadRefresh
                     when needRefresh $ do
                        Size { sizeW = w, sizeH = h } <- get f clientSize

                        get currentTile value >>= cancelTile
                        renderAction <- cvGetFunction
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]

                        triggerRepaint
                 ]

    -- Animation timer. At ~65Hz, check if we are animating between
    -- two views. If so, step the animation and repaint.
    _ <- timer f [ interval := 16
                 , enabled := True
                 , on command := get animate value >>= \case
                         Nothing -> pure ()
                         Just _  -> triggerRepaint
                 ]

    -- onResizeTimer is a one-shot timer that fires 100ms after the
    -- frame has been resized. If another resize event comes in during
    -- that interval, the timer is reset to 100ms. When the timer fires,
    -- we kick off a new rendering task to build the contents of the
    -- window. Using a timer lets us avoid starting hundreds of rendering
    -- tasks while the user adjusts their window size.
    onResizeTimer <- timer f [ interval := 100
                             , enabled := False ]
    set onResizeTimer [ on command := do
                              set onResizeTimer [enabled := False] -- one-shot
                              needResize <- get pendingResize value
                              when needResize $ do
                                  set pendingResize [value := False]
                                  Size { sizeW = w0, sizeH = h0 } <- get f clientSize
                                  let w = roundUp w0 16
                                      h = roundUp h0 16
                                      roundUp x n = case x `mod` n of
                                          0 -> x
                                          k -> x + (n - k)
                                  get currentTile value >>= cancelTile
                                  renderAction <- cvGetFunction
                                  newViewerTile <- renderTile' renderId renderAction (w, h) model
                                  set currentTile [value := newViewerTile]
                                  -- no animation?
                                  triggerRepaint ]


    set f [ on resize := do
                  set onResizeTimer [enabled := False]
                  set pendingResize [value := True]
                  set onResizeTimer [enabled := True]
                  propagateEvent ]

    -------------------------------------------------------
    -- Change tracking
    -------------------------------------------------------

    -- For each variable that the viewer code depends on, trigger a repaint whenever
    -- that variable changes.
    let usedVars = execState (indexedFoldM gatherUsedVarsInCode cvCode') Set.empty

        gatherUsedVarsInCode :: CodeF '[] Unit et
                             -> State (Set String) ()
        gatherUsedVarsInCode = \case
          Let _ name _ _ _ _ -> modify' (Set.delete (symbolVal name))
          Set{} -> pure ()
          Call{} -> pure ()
          Block{} -> pure ()
          Pure _ v -> gatherUsedVars v
          NoOp{} -> pure ()
          DoWhile{} -> pure ()
          IfThenElse _ tf _ _ -> gatherUsedVars tf
          Effect{} -> pure () -- FIXME, should also inspect the effects

        gatherUsedVars :: Value et -> State (Set String) ()
        gatherUsedVars = indexedFoldM @Unit gatherUsedVarsInValue

        gatherUsedVarsInValue :: ValueF Unit et -> State (Set String) ()
        gatherUsedVarsInValue = \case
          Var name _ _ -> modify' (Set.insert (symbolVal name))
          LocalLet name _ _ _ _ _ -> modify' (Set.delete (symbolVal name))
          _ -> pure ()

    fromContextM_ (\name _ v ->
                      when (symbolVal name `Set.member` usedVars)
                        (v `listenWith` (\_ _ -> requestRefresh))) cvConfig'

    -------------------------------------------------------
    -- Menus
    -------------------------------------------------------

    -- Viewer menu
    viewMenu <- menuPane [text := "&Viewer"]
    menuItem viewMenu [ text := "Reset view\t^"
                      , on command := do
                          set model [ value := Model (0,0) (1/128,1/128) ]
                          requestRefresh
                      ]
    menuItem viewMenu [ text := "Clone viewer\t2"
                      , on command := clone
                      ]

    -- Tool menu
    tools <- menuPane [text := "&Tool"]

    nav <- menuRadioItem tools [ text := "Navigate\tn"
                               , help := "Move around or whatever"
                               , on command := do
                                   set toolStatus [text := "Navigate"]
                                   set currentToolIndex [value := Nothing]
                               ]

    -- Add each tool to the tool menu
    forM_  (zip [0..] . map toolInfo $ theTools) $ \(ix, ToolInfo{..}) -> menuRadioItem tools
          [ text := tiName ++ maybe "" (\c -> "\t" ++ (c : "")) tiShortcut
          , help := tiShortHelp
          , on command := do
              -- When the menu item is selected, send the current tool
              -- a Deactivated event, and then send the selected tool
              -- an Activated event.
              get currentToolIndex value >>= \case
                Nothing -> pure ()
                Just i -> do
                  toolEventHandler (theTools !! i) Deactivated
                  cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
              set toolStatus [text := tiName]
              set currentToolIndex [value := Just ix]
              toolEventHandler (theTools !! ix) Activated
              cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
          ]

    -- Select the Navigate tool to start
    set nav [ checked := True ]

    let menuBarItems =
          [ viewMenu ] ++
          [ tools | length theTools > 0 ]

    addMenuBar f menuBarItems

-- | A model of the view, in terms of the underlying coordinate system.
data Model = Model
  { modelCenter   :: (Double, Double)
  , modelPixelDim :: (Double, Double)
  }

-- | Given the dimensions of the window, work out a rectangle describing
-- the viewport in the underlying coordinate system.
modelToRect :: Planar a => (Int,Int) -> Model -> Rectangle a
modelToRect (w,h) Model{..} = flippedRectangle (fromCoords ul) (fromCoords lr)
  where
    ul = (cx - px * fromIntegral w / 2, cy - py * fromIntegral h / 2)
    lr = (cx + px * fromIntegral w / 2, cy + py * fromIntegral h / 2)
    (cx, cy) = modelCenter
    (px, py) = modelPixelDim

-- | Smoothly interpolate between two models
interpolateModel :: Double -> Model -> Model -> Model
interpolateModel t m1 m2 = m2
    { modelCenter   = interpolate modelCenter
    , modelPixelDim = logInterpolate modelPixelDim
    }
  where
    interp p q = p + t * (q - p)
    interp2 (p1,p2) (q1,q2) = (interp p1 q1, interp p2 q2)
    interpolate f = interp2 (f m1) (f m2)
    logInterp p q = p * (q/p) ** t
    logInterp2  (p1,p2) (q1,q2) = (logInterp p1 q1, logInterp p2 q2)
    logInterpolate f = logInterp2 (f m1) (f m2)

-- | A wrapper around UI.Tile.renderTile that tags each rendering
-- action with an ID, and will stop rendering when the ID no longer
-- matches the current ID value.
renderTile' :: Valued w
            => IORef Int
            -> BlockComputeAction
            -> (Int, Int)
            -> w Model
            -> IO Tile
renderTile' renderId action dim model = do
    iD <- atomicModifyIORef' renderId (\x -> (x + 1, x + 1))
    modelRect <- modelToRect dim <$> get model value
    let action' p q r x y c = do
            curId <- readIORef renderId
            if (curId == iD) then action p q r x y c else pure ()
    renderTile action' dim modelRect

drawCenteredImage :: Image b -> DC d -> Rect -> (Int,Int) -> IO ()
drawCenteredImage img dc windowRect (width, height) = do
    let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
                     (fHeight + height) `div` 2 - height )
    drawImage dc img (WX.pt x0 y0) []

viewportToPoint :: Viewport -> Point
viewportToPoint (Viewport (x,y)) = Point { pointX = x, pointY = y }


getDragBox :: Var (Maybe Viewport)
           -> Var (Maybe Viewport)
           -> IO (Maybe (Rectangle Viewport))
getDragBox lastClick draggedTo = do
    dragSrc <- get lastClick value
    dragTgt <- get draggedTo value
    return $ case (dragSrc, dragTgt) of
        (Just p1, Just p2) -> Just $ rectangle p1 p2
        _                  -> Nothing

drawBox :: DC d
        -> WX.Color
        -> WX.Color
        -> [Point]
        -> IO ()
drawBox dc fillColor lineColor coords =
    polygon dc coords [ brush := brushSolid fillColor
                      , pen := penColored lineColor 2
                      ]

-- | Paint the state of a tile into a device context.
generateTileImage
    :: Tile    -- ^ A tile to convert to an image
    -> Rect    -- ^ The enclosing view rectangle
    -> IO (Image ())
generateTileImage viewerTile _windowRect = do
    let (width, height) = tileRect viewerTile
    withSynchedTileBuffer viewerTile (imageCreateFromData (sz width height))

-- | Read draw commands from each tool layer and paint them into the
-- given device context, and then draw the zoom drag box on top.
paintToolLayerWithDragBox :: ((Double, Double) -> IO Point)
               -> (Double, Double)
               -> IO [[DrawCommand]]
               -> Var (Maybe Viewport)
               -> Var (Maybe Viewport)
               -> DC d
               -> Rect
               -> Rect
               -> IO ()
paintToolLayerWithDragBox modelToView pxDim getDrawCommands lastClick draggedTo dc _ _ = dcEncapsulate dc $ do
    paintToolLayer modelToView pxDim getDrawCommands dc
    dragBox <- getDragBox lastClick draggedTo
    case dragBox of
        Nothing  -> return ()
        Just box -> do
            let boxPts = map viewportToPoint (rectPoints box)
            drawBox dc (rgba @Word8 0 128 255 128) white boxPts

-- | Read draw commands for each tool layer and paint them into the
-- given device context
paintToolLayer :: ((Double, Double) -> IO Point)
               -> (Double, Double)
               -> IO [[DrawCommand]]
               -> DC d
               -> IO ()
paintToolLayer modelToView pxDim getDrawCommands dc = dcEncapsulate dc $ do

    cmdss <- getDrawCommands
    let initialPenColor = rgba 255 255 255 (255 :: Word8)
        initialBrushColor = rgba 255 255 255 (255 :: Word8)
    currentPen <- newIORef initialPenColor
    currentBrush <- newIORef initialBrushColor

    let withPen fil action = do
          curPen <- (`penColored` 2) <$> readIORef currentPen
          curBrush <- (if fil then brushSolid else const brushTransparent) <$> readIORef currentBrush
          dcWithPenStyle dc curPen $ dcWithBrushStyle dc curBrush $ action []
        pxSz = sqrt(fst pxDim * snd pxDim)

    forM_ cmdss $ \cmds -> do
      -- Reset the pen and brush for the next drawing layer
      writeIORef currentPen initialPenColor
      writeIORef currentBrush initialBrushColor

      forM_ cmds $ \case

        DrawPoint _ pt -> do
          pt' <- modelToView pt
          withPen True (circle dc pt' 2)

        DrawLine _ pt1 pt2 -> do
          pt1' <- modelToView pt1
          pt2' <- modelToView pt2
          withPen False (line dc pt1' pt2')

        DrawCircle _ fil r pt -> do
          pt' <- modelToView pt
          let r' = round (r / pxSz)
          withPen fil (circle dc pt' r')

        DrawRect _ fil pt1 pt3 -> do
          let pt2 = (fst pt1, snd pt2)
              pt4 = (snd pt1, fst pt2)
          pt1' <- modelToView pt1
          pt2' <- modelToView pt2
          pt3' <- modelToView pt3
          pt4' <- modelToView pt4
          let boxpts = [pt1', pt2', pt3', pt4']
          withPen fil (polygon dc boxpts)

        Clear {} -> pure () -- clear operations should be handled upstream,
                            -- by truncating the draw command list
        SetStroke _ c -> writeIORef currentPen (fsColorToWxColor c)

        SetFill _ c -> writeIORef currentBrush (fsColorToWxColor c)

-- | Convert a FractalStream color to a WxWidgets color
fsColorToWxColor :: Color -> WX.Color
fsColorToWxColor c =
  let (r,g,b) = colorToRGB c in rgba r g b 255

data Unit :: (Environment, FSType) -> Exp Type
type instance Eval (Unit et) = ()
