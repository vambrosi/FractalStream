module UI.Layout
  ( generateWxLayout
  ) where

import FractalStream.Prelude hiding (get)

import Data.Color (colorToRGB, rgbToColor)
import Data.DynamicValue
import Actor.Layout

import Graphics.UI.WX hiding (pt, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WXCore.Events as WX
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ
import           Graphics.UI.WXCore.WxcDefs

generateWxLayout :: Dynamic dyn
                 => Window a
                 -> Layout dyn
                 -> IO WX.Layout

generateWxLayout frame0 wLayout = do
  panel0 <- panel frame0 []
  computedLayout <- go panel0 wLayout
  pure (container panel0 computedLayout)

 where

   go :: Dynamic dyn => Window a -> Layout dyn -> IO WX.Layout
   go p = \case

     Panel pTitle inner -> do
       p' <- panel p [ ]
       go p' inner
       pure (fill $ boxed pTitle (widget p'))

     Vertical parts -> do
       p' <- panel p []
       lo <- fill . column 5 <$> mapM (go p') parts
       set p' [ layout := lo ]
       pure (fill (widget p'))

     Horizontal parts -> do
       p' <- panel p []
       lo <- fill . margin 10 . row 5 <$> mapM (go p') parts
       set p' [ layout := lo ]
       pure (fill (widget p'))

     Tabbed ts -> do
       nb <- feed2 [ visible := True ] 0 $
             initialWindow $ \iD rect' ps s -> do
                   e <- notebookCreate p iD rect' s
                   set e ps
                   pure e
       forM_ ts $ \(lab, x) -> do
         c <- panel nb []
         page <- go c x
         set c [ layout := page ]
         notebookAddPage nb c lab True (-1)
       notebookSetSelection nb 0
       pure (fill $ widget nb)

     ColorPicker (Label lab) v -> do
       (r0, g0, b0) <- colorToRGB <$> getDynamic v
       picker <- feed2 [ text := lab, visible := True ] 0 $
                 initialWindow $ \iD rect' ps s -> do
                   e <- colorPickerCtrlCreate p iD (rgb r0 g0 b0) rect' s
                   set e ps
                   pure e
       let newPick = do
             c <- colorPickerCtrlGetColour picker
             let r = colorRed c
                 g = colorGreen c
                 b = colorBlue c
             void (setDynamic v (rgbToColor (r, g, b)))
       WX.windowOnEvent picker [wxEVT_COMMAND_COLOURPICKER_CHANGED] newPick (const newPick)
       pure (fill $ row 5 [ margin 3 (label lab), hfill (widget picker) ])

     CheckBox (Label lab) v -> do
       initial <- getDynamic v
       cb <- checkBox p [ text := lab
                        , checkable := True
                        , checked := initial
                        , visible := True
                        ]
       set cb [ on command := do
                  isChecked <- get cb checked
                  void (setDynamic v isChecked)
              ]
       listenWith v (\_ isChecked -> set cb [ checked := isChecked ])
       pure (widget cb)

     TextBox (Label lab) v -> do
       initial <- getDynamic v
       te <- textEntry p [ text := initial
                         , processEnter := True
                         , tooltip := ""
                         ]
       normalBG <- get te bgcolor

       errorMessage <- variable [value := Nothing]
       showErrorTimer <- timer frame0 [ interval := 100
                                      , enabled := False ]

       errorPopup <- frame
         [ visible := False
         , style := wxFRAME_TOOL_WINDOW .+. wxNO_BORDER
         , position := Point 0 0
         ]
       ep <- panel errorPopup [ bgcolor := rgb 255 200 (200 :: Int)]
       txt <- staticText ep [ text := ""
                            , font := fontFixed
                            , fontSize := 12
                            , color := black ]
       let showError err = do
             Point wx wy <- get te position
             set txt [ text := err ]
             set errorPopup [ visible := True
                            , layout := fill $ container ep $ margin 5 $ widget txt
                            , position := Point (wx + 40) (wy + 40)]

       set showErrorTimer [ on command := do
         set showErrorTimer [ enabled := False ]
         msg <- get errorMessage value
         case msg of
           Nothing -> pure ()
           Just err -> showError err
         ]

       let alert = \case
             Nothing -> do
               set te [ bgcolor := normalBG ]
               set errorMessage [ value := Nothing ]
             Just err -> do
               set te [ bgcolor := rgb 180 80 (80 :: Int)]
               showError err

       set te [ on command := do
                  newText <- get te text
                  setDynamic v newText >>= alert
              , on focus := (\case
                                True -> pure ()
                                False -> do
                                  newText <- get te text
                                  oldText <- getDynamic v
                                  when (newText /= oldText) $ setDynamic v newText >>= alert)
              , on enter := \_ -> do
                  alreadyShowing <- get errorPopup visible
                  unless alreadyShowing $ set showErrorTimer [ enabled := True ]
              , on leave := \_ -> do
                  set showErrorTimer [ enabled := False ]
                  set errorPopup [ visible := False ]
              ]
       listenWith v (\_ newText -> set te [ text := newText ])
       pure (fill $ row 5 [ margin 3 (label lab), hfill (widget te) ])
