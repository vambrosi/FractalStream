import Distribution.MacOSX
import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags(..))
import Distribution.PackageDescription hiding (updatePackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)
import qualified System.Info as System
import Data.Either (partitionEithers)
import Data.List (isSuffixOf)

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
                  (ChaseWith $ defaultExclusions ++ ["/usr/lib"])
          ]


wxConfHook :: (GenericPackageDescription, HookedBuildInfo)
           -> ConfigFlags
           -> IO LocalBuildInfo
wxConfHook (pkg_descr, hooked_bi) flags = do
  -- Get wx-config --libs output
  wxConfig <- getWxConfig flags
  output0 <- words <$> wxConfig ["--libs"]
  (includes, cxxOpts) <- partitionEithers . map splitCxxOptions . words
                         <$> wxConfig ["--cxxflags"]
  let output = if System.os == "darwin"
               then output0 ++ ["-framework", "AppKit"]
               else output0
      (wxLibs, wxLdDirsOrOpts) = partitionEithers (concatMap splitLdOptions output)
      (wxLdDirs, wxLdOpts) = partitionEithers wxLdDirsOrOpts
      pkg_descr' = updatePackageDescription wxLibs wxLdOpts wxLdDirs includes cxxOpts pkg_descr
  confHook simpleUserHooks (pkg_descr', hooked_bi) flags

splitLdOptions :: String -> [Either String (Either String String)]
splitLdOptions opt = case opt of
  '-' : 'l' : libname -> [Left libname]
  '-' : 'L' : dirname -> [Right (Left dirname)]
  -- This is a hack to work around a bug(?) in wx-config where some frameworks
  -- are given as a full path, which confuses the linker.
  _ | ".framework" `isSuffixOf` opt
                      -> [ Right (Right "-framework")
                         , Right . Right
                         . takeWhile (/= ".") . reverse
                         . takeWhile (/= "/") . reverse
                         $ opt
                         ]
    | otherwise       -> [Right (Right opt)]

splitCxxOptions :: String -> Either String String
splitCxxOptions opt = case opt of
  '-' : 'I' : include -> Left include
  _                   -> Right opt

getWxConfig :: ConfigFlags -> IO ([String] -> IO String)
getWxConfig confFlags = do
  let verbosity = fromFlag $ configVerbosity confFlags
  program <- fst <$> requireProgram verbosity (simpleProgram "wx-config-3.2")
                                   (configPrograms confFlags)
  return $ getProgramOutput verbosity program

updatePackageDescription :: [String]
                         -> [String]
                         -> [String]
                         -> [String]
                         -> [String]
                         -> GenericPackageDescription
                         -> GenericPackageDescription
updatePackageDescription wxLibs wxLdOpts wxLdDirs wxIncludes wxCxxOpts gpd =
    gpd { condExecutables = map updateExecutable (condExecutables gpd) }
  where
    updateExecutable (name, condTree) =
      (name, fmap (\exe -> exe {
                      buildInfo = (buildInfo exe)
                        { ldOptions = ldOptions (buildInfo exe) ++ wxLdOpts
                        , extraLibs = extraLibs (buildInfo exe) ++ wxLibs
                        , extraLibDirs = extraLibDirs (buildInfo exe) ++ wxLdDirs
                        , cxxOptions = cxxOptions (buildInfo exe) ++ wxCxxOpts
                        , ccOptions  = ccOptions (buildInfo exe) ++ wxCxxOpts
                        , includeDirs = includeDirs (buildInfo exe) ++ wxIncludes
                        }
                      }) condTree)
