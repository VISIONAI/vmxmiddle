-- Example Setup.hs for the wxHello app.

import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = do
         defaultMainWithHooks $ simpleUserHooks {
           postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
         }

guiApps :: [MacApp]
guiApps = [MacApp "middle"
                  (Just "resources/WxHello.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  [] -- No other resources.
                  [] -- No other binaries.
                  (ChaseWith defaultExclusions) -- Try changing to ChaseWithDefaults
          ]
