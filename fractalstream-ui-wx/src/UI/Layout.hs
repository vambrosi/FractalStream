module UI.Layout
  ( generateWxLayout
  ) where

import FractalStream.Prelude hiding (get)

import Data.Color (colorToRGB, grey)
import Data.DynamicValue
import Language.Parser.SourceRange (spanOfSourceRange, SourceSpan(..))
import Language.Parser.Tokenizer (commentRanges)
import Actor.Layout
import Data.IORef
import Text.Printf (printf)
import qualified Data.Map as Map

import UI.CodeEditor

import Graphics.UI.WX hiding (pt, grey, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WXCore.Events as WX
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ
--import           Graphics.UI.WXCore.WxcDefs
--import Graphics.UI.WXCore.Frame (windowGetScreenPosition)

generateWxLayout :: (String -> IO ())
                 -> Window a
                 -> Layout
                 -> IO (IO (), WX.Layout)

generateWxLayout buttonPress frame0 wLayout = do
  panel0 <- panel frame0 []

  let watch :: forall f a. AsDynamic f => f a -> (a -> IO ()) -> StateT (IO ()) IO ()
      watch d action = do
        done <- watchDynamic d action
        modify (>> done)

      go :: Window a -> Layout -> StateT (IO ()) IO WX.Layout
      go p = \case

        Panel pTitle innerLayout -> do
          Label title <- getDynamic pTitle
          p' <- liftIO $ panel p [ ]
          inner <- getDynamic innerLayout
          lo <- go p' inner
          liftIO $ set p' [ layout := lo ]
          pure (hstretch $ expand $ margin 5 $ boxed title (fill $ widget p'))

        Vertical v -> do
          p' <- liftIO $ panel p []
          parts <- getDynamic v
          lo <- hstretch . expand . margin 5 . column 5 <$> mapM (go p') parts
          liftIO $ set p' [ layout := lo ]
          pure (hstretch $ expand (widget p'))

        Horizontal v -> do
          p' <- liftIO $ panel p []
          parts <- getDynamic v
          lo <- fill . margin 5 . row 5 <$> mapM (go p') parts
          liftIO $ set p' [ layout := lo ]
          pure (hstretch $ expand (widget p'))

        Tabbed dts -> do
          nb <- liftIO $ feed2 [ visible := True ] 0 $
                initialWindow $ \iD rect' ps s -> do
                   e <- notebookCreate p iD rect' s
                   set e ps
                   pure e
          ts <- getDynamic dts
          forM_ ts $ \TabItem{..} -> do
            Label lab <- getDynamic tiLabel
            c <- liftIO $ panel nb []
            page <- go c =<< getDynamic tiBody
            liftIO $ do
              set c [ layout := page ]
              notebookAddPage nb c lab True (-1)
          liftIO $ notebookSetSelection nb 0
          pure (fill $ margin 5 $ widget nb)

        PlainText tv -> liftIO $ do
          txt <- getDynamic tv
          p' <- panel p []
          lo <- margin 5 . floatCentre . widget <$> staticText p' [ text := txt ]
          set p' [layout := lo]
          pure (hstretch . expand . margin 5 $ widget p')

        Button tv -> liftIO $ do
          txt <- getDynamic tv
          p' <- panel p []
          btn <- button p' [ text := txt
                           , on command := buttonPress txt ]
          pure (hstretch . expand . container p' $ floatCentre $ margin 5 $ widget btn)

        Selection _l _ pick options -> do
          --Label lab <- getDynamic l
          ix <- getDynamic pick
          opts <- getDynamic options
          p' <- liftIO $ panel p []
          c <- liftIO $ choice p' [ selection := fromIntegral ix, items := opts ]
          liftIO $ set c [ on select := do
                             newIx <- fromIntegral <$> get c selection
                             setValue pick newIx
                         ]
          watch pick $ \newIx -> set c [ selection := fromIntegral newIx ]
          pure (hstretch . expand . container p' $ fill $ widget c)

        ColorPicker l _ col -> do
          Label lab <- getDynamic l
          (r0, g0, b0) <- colorToRGB . fromRight grey <$> getDynamic col
          picker <- liftIO $ do
            picker <- feed2 [ text := lab, visible := True ] 0 $
              initialWindow $ \iD rect' ps s -> do
                e <- colorPickerCtrlCreate p iD (rgb r0 g0 b0) rect' s
                set e ps
                pure e
            let newPick = do
                  c <- colorPickerCtrlGetColour picker
                  let r = fromIntegral (colorRed c   :: Word8) / 255.0 :: Double
                      g = fromIntegral (colorGreen c :: Word8) / 255.0 :: Double
                      b = fromIntegral (colorBlue c  :: Word8) / 255.0 :: Double
                  setValue (source col) (printf "rgb(%0.3f, %0.3f, %0.3f)" r g b :: String)

            -- TODO: update picker if color is changed by a script

            WX.windowOnEvent picker [wxEVT_COMMAND_COLOURPICKER_CHANGED] newPick (const newPick)
            pure picker

          pure (hstretch $ expand $ margin 5 $
                row 5 [ margin 3 (label lab), hfill (widget picker) ])

        CheckBox l _ b -> do
          Label lab <- getDynamic l
          initial <- getDynamic b
          cb <- liftIO $ do
            cb <- checkBox p [ text := lab
                             , checkable := True
                             , checked := initial
                             , visible := True
                             ]
            set cb [ on command := do
                       isChecked <- get cb checked
                       void (setValue b isChecked)
                   ]
            pure cb
          watch b (\isChecked -> set cb [ checked := isChecked ])
          pure (hstretch . expand . margin 5 $ widget cb)

        ScriptBox v -> do
          p' <- liftIO $ panel p []
          ce <- liftIO $ codeEditor p' (scriptCode v)
          txt <- unCodeString <$> getDynamic (source $ scriptCode v)
          lastText <- liftIO $ newIORef txt
          errorText <- liftIO $ staticText p' [ text := "" ]

          let doSyntaxColoring = do
                code <- get ce text
                if null code
                  then pure Map.empty
                  else do
                    let m = editorOffsetMap code
                    styledTextCtrlStartStyling ce 0 0
                    styledTextCtrlSetStyling ce ((m Map.! (length code - 1)) + 1) 0
                    forM_ (commentRanges code) $ \(s, e) -> do
                      styledTextCtrlStartStyling ce (m Map.! s) 0
                      styledTextCtrlSetStyling ce (m Map.! (e + 1 - s)) 2
                    pure m

          liftIO $ set ce $
            [ on focus := \tf -> do
                case tf of
                  True -> styledTextCtrlGetText ce >>= writeIORef lastText
                  False -> do
                    new <- styledTextCtrlGetText ce
                    old <- readIORef lastText
                    when (new /= old) $ do
                      writeIORef lastText new
                      void (setValue (source $ scriptCode v) (CodeString new))
                propagateEvent
            ]

          -- Check for script changes periodically. TODO: make this event-driven instead
          liftIO $ do
            t <- timer frame0 [ interval := 500, enabled := True
                              , on command := do
                                  new <- styledTextCtrlGetText ce
                                  old <- readIORef lastText
                                  when (new /= old) $ do
                                    writeIORef lastText new
                                    setValue (source $ scriptCode v) (CodeString new) ]
            set frame0 [ on closing :~ (timerStop t >>) ]
          watch (source $ scriptCode v) $ \(CodeString newText) -> do
            set ce [ text := newText ]
          isError <- liftIO ((isLeft <$> getDynamic (scriptCode v)) >>= \startError ->
             variable [ value := startError ])
          watch (scriptCode v) $ \case
            Left (loc, msg) -> do
              set isError [ value := True ]
              set errorText [ text := "⚠️ " ++ msg, visible := True ]
              windowReLayout p'
              code <- styledTextCtrlGetText ce
              case convertSourceSpan code <$> spanOfSourceRange loc of
                Nothing -> void doSyntaxColoring
                Just (s, e) -> do
                  m <- doSyntaxColoring
                  styledTextCtrlStartStyling ce (m Map.! s) 0
                  styledTextCtrlSetStyling ce (m Map.! (e + 1 - s)) 1
            Right _ -> do
              wasError <- get isError value
              void doSyntaxColoring
              when wasError $ do
                set isError [ value := False ]
                set errorText [ text := "", visible := False ]
                windowReLayout p'

          void $ liftIO doSyntaxColoring
          pure (container p' $ fill $ column 5 [ fill $ widget ce, hstretch $ expand $ widget errorText ])

        TextBox l v -> do
          Label lab <- getDynamic l
          errorMessage <- liftIO $ staticText p [ text := "" ] -- , font := fontFixed, fontSize := 12, color := black ]
          te <- liftIO $ do
            initial <- getDynamic (source $ exprValue v)
            te <- textEntry p [ text := initial
                              , processEnter := True
                              , tooltip := ""
                              ]
            normalBG <- get te bgcolor

            -- TODO: only refresh if the value has changed
            let setErrorMessage = \case
                  Nothing -> do
                    set te [ bgcolor := normalBG ]
                    set errorMessage [ text := "" ]
                    windowReLayout panel0
                  Just err -> do
                    set te [ bgcolor := rgb 180 80 (80 :: Int)]
                    set errorMessage [ text :=  "⚠️", tooltip := err
                                     , on click := \_ -> do
                                         tw <- tipWindowCreate frame0 "" 1000
                                         textCol <- frameIsLight tw <&> \case
                                           True  -> black
                                           False -> white
                                         _ <- staticText tw [ text := err
                                                            , font := fontFixed
                                                            , fontSize := 12
                                                            , color := textCol ]
                                         windowReLayout tw
                                         windowRefresh tw True
                                     ]
                    windowReLayout panel0

            set te [ on command := do
                       newText <- get te text
                       setValue (source $ exprValue v) newText
                       getDynamic (exprValue v) >>= \case
                         Left msg -> setErrorMessage (Just msg)
                         _        -> setErrorMessage Nothing
                , on focus := \case
                    True -> propagateEvent
                    False -> do
                      newText <- get te text
                      oldText <- getDynamic (source $ exprValue v)
                      when (newText /= oldText) $ do
                        setValue (source $ exprValue v) newText
                        getDynamic (exprValue v) >>= \case
                          Left msg -> setErrorMessage (Just msg)
                          _        -> setErrorMessage Nothing
                      propagateEvent
                   ]
            pure te
          watch (source $ exprValue v) (\newText -> set te [ text := newText ])
          pure (hstretch $ expand $ margin 5 $ row 5 [ margin 3 (label lab)
                                                     , hfill (widget te)
                                                     , widget errorMessage ])

  (computedLayout, done) <- runStateT (go panel0 wLayout) (pure ())
  pure (done, container panel0 $ fill  computedLayout)

convertSourceSpan :: String -> SourceSpan -> (Int, Int)
convertSourceSpan input = \case
  InLine linum s e -> let lo = lineOffset input linum + s - 1
                          hi = e - s + lo
                      in (lo, hi)
  InRows s e -> let lo = lineOffset input
                in (lo s, lo (e + 1))

lineOffset :: String -> Int -> Int
lineOffset input = let m = Map.fromList
                         . ((0,0):)
                         . zip [1..]
                         . map fst
                         . filter ((== '\n') . snd)
                         . zip [1..] $ input
                       li = length input
                   in \lo -> 1 + Map.findWithDefault li lo m
