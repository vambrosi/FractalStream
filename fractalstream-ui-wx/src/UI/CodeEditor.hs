module UI.CodeEditor
  ( codeEditor
  ) where

import FractalStream.Prelude hiding (get)

import Graphics.UI.WX hiding (Vertical, Horizontal)
import Graphics.UI.WXCore.WxcClasses

codeEditor :: Frame () -> Window a -> String -> IO (StyledTextCtrl ())
codeEditor ce cep code = do
    stc <- styledTextCtrl cep [ clientSize := sz 100 100 ]
    styledTextCtrlSetMarginWidth stc 0 30
    styledTextCtrlSetMarginWidth stc 1 0
    styledTextCtrlSetMarginWidth stc 2 0
    -- see Style Definition at https://www.scintilla.org/ScintillaDoc.html#Styling
    lum <- do
      col <- get ce bgcolor
      let rc :: Float = colorRed col / 255
          gc = colorGreen col / 255
          bc = colorBlue col / 255
      pure (0.299 * rc + 0.587 * gc + 0.114 * bc)
    if lum > 0.5
      then do
        styledTextCtrlStyleSetSpec stc  0 "fore:#000000,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc 32 "fore:#000000,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc 33 "fore:#808080,back:#f0f060"
        styledTextCtrlSetCaretLineBackground stc (rgb 240 240 (255 :: Word8))
      else do
        styledTextCtrlStyleSetSpec stc  0 "fore:#dbdbdb,back:#14191e"
        styledTextCtrlStyleSetSpec stc 32 "fore:#dbdbdb,back:#14191e"
        styledTextCtrlStyleSetSpec stc 33 "fore:#a0a0a0,back:#101040"
        styledTextCtrlSetCaretLineBackground stc (rgb 0x20 0x30 (0x38 :: Word8))

    styledTextCtrlStyleSetFaceName stc 0 "Monaco"
    -- Set the minimum size, or else Scintilla will default to 2000 pixels(!)
    styledTextCtrlSetScrollWidth stc 100
    styledTextCtrlSetCaretLineVisible stc True

    styledTextCtrlSetUseTabs stc False
    styledTextCtrlSetTabWidth stc 4
    styledTextCtrlSetIndent stc 4
    styledTextCtrlSetTabIndents stc True
    styledTextCtrlSetBackSpaceUnIndents stc True
    styledTextCtrlSetIndentationGuides stc True
    styledTextCtrlSetViewWhiteSpace stc 3

    styledTextCtrlSetText stc code
    pure stc
