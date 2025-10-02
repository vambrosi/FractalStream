module UI.Welcome
  ( welcome
  ) where

import Graphics.UI.WX

import UI.ProjectActions
import UI.Menu

welcome :: ProjectActions -> IO ()
welcome ProjectActions{..} = do

  f <- frame [ text := "Welcome to FractalStream!"
             , on resize := propagateEvent ]

  makeMenuBar ProjectActions{..} f []

  p <- panel f []

  let getProject verb action = do
        mprj <- fileOpenDialog objectNull True True
                (verb ++ " a FractalStream project")
                [ ("FractalStream project files", ["*.yaml"])
                , ("All files", ["*.*"])
                ] "" ""
        case mprj of
          Nothing -> pure ()
          Just prj -> do
            --set f [ visible := False ]
            action prj

  -- TODO: verify that the code for each viewer, tool, etc works properly
  --       with the splices declared by the setup config. e.g. all code
  --       typechecks with the splice's types, each splice's environment
  --       is contained in the actual code environment at each use, etc.
  --
  --       If the ensemble passes this verification, then the end-user
  --       should not be able to cause a compilation failure via the
  --       UI.
  new  <- button p [ text := "New project"
                   , on command := projectNew ]

  open <- button p [ text := "Open project"
                   , on command := getProject "Open" projectOpen
                   ]
  edit <- button p [ text := "Edit project"
                   , on command := getProject "Edit" projectEdit
                   ]
  set f [ layout := margin 10 $ stretch $ container p $ floatCenter $ column 10
                    [ widget new
                    , widget open
                    , widget edit
                    ]
        , clientSize := sz 300 120
        ]
