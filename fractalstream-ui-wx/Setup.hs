import Distribution.MacOSX
import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags(..))
import Distribution.PackageDescription hiding (updatePackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
       { postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       , confHook = wxConfHook
       }

guiApps :: [MacApp]
guiApps = [MacApp "FractalStream"

                  -- Icon file
                  (Just "FS.icns")

                  -- Build a default Info.plist
                  Nothing

                  -- Other resource files
                  []

                  -- Other binary files
                  []

                  -- Starting in Big Sur, MacOS caches certain
                  -- system libraries. They act like they are present
                  -- when using dlopen but do not actually exist on
                  -- disk at the stated locations! This throws off
                  -- `ChaseWithDefaults` when it uses otool -L to find
                  -- dylib dependencies. We'll work around it by exlcuding
                  -- /usr/lib, where these libraries claim to be installed.
                  --
                  -- See: https://developer.apple.com/forums/thread/655588
                  --
                  DoNotChase --(ChaseWith $ defaultExclusions ++ ["/usr/lib"])
          ]

wxConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
wxConfHook (pkg_descr, hooked_bi) flags = do
  -- Get wx-config --libs output
  wxConfig <- getWxConfig flags
  wxLibs0 <- words <$> wxConfig ["--libs"]
  -- from ls /opt/homebrew/lib/libwx*-3.2.dylib | sed 's/\/opt\/homebrew\/lib\/lib/"-l/g'  | sed 's/-3.2.dylib/"
  let wxLibs = wxLibs0 ++ ["-framework", "AppKit",
                           "-lwx_baseu_net-3.2",
                           "-lwx_baseu_xml-3.2",
                           "-lwx_baseu-3.2",
                           "-lwx_osx_cocoau_adv-3.2",
                           "-lwx_osx_cocoau_aui-3.2",
                           "-lwx_osx_cocoau_core-3.2",
                           "-lwx_osx_cocoau_gl-3.2",
                           "-lwx_osx_cocoau_html-3.2",
                           "-lwx_osx_cocoau_media-3.2",
                           "-lwx_osx_cocoau_propgrid-3.2",
                           "-lwx_osx_cocoau_qa-3.2",
                           "-lwx_osx_cocoau_ribbon-3.2",
                           "-lwx_osx_cocoau_richtext-3.2",
                           "-lwx_osx_cocoau_stc-3.2",
                           "-lwx_osx_cocoau_webview-3.2",
                           "-lwx_osx_cocoau_xrc-3.2"
                          ]
  let pkg_descr' = updatePackageDescription wxLibs pkg_descr
  confHook simpleUserHooks (pkg_descr', hooked_bi) flags

getWxConfig :: ConfigFlags -> IO ([String] -> IO String)
getWxConfig confFlags = do
  let verbosity = fromFlag $ configVerbosity confFlags
  program <- fst <$> requireProgram verbosity (simpleProgram "wx-config-3.2")
                                   (configPrograms confFlags)
  return $ getProgramOutput verbosity program

updatePackageDescription :: [String] -> GenericPackageDescription -> GenericPackageDescription
updatePackageDescription wxLibs gpd =
  gpd { condExecutables = map (updateExecutable wxLibs) (condExecutables gpd)
      , condTestSuites = map (updateTestSuite wxLibs) (condTestSuites gpd)
      , condBenchmarks = map (updateBenchmark wxLibs) (condBenchmarks gpd)
      }

updateExecutable wxLibs (name, condTree) =
  (name, fmap (\exe -> exe { buildInfo = (buildInfo exe) { ldOptions = ldOptions (buildInfo exe) ++ wxLibs }}) condTree)

updateTestSuite wxLibs (name, condTree) =
  (name, fmap (\test -> test { testBuildInfo = updateBuildInfo wxLibs (testBuildInfo test) }) condTree)

updateBenchmark wxLibs (name, condTree) =
  (name, fmap (\bench -> bench { benchmarkBuildInfo = updateBuildInfo wxLibs (benchmarkBuildInfo bench) }) condTree)

updateBuildInfo wxLibs bi = bi { extraLibs = extraLibs bi ++ wxLibs }
