module Actor.Viewer.Types
  ( InternalX
  , InternalY
  , InternalDX
  , InternalDY
  , InternalViewerEnv
  , ViewerEnv
  , MissingViewerArgs
  -- ** Re-exports
  , InternalIterations
  , InternalStuck
  , InternalIterationLimit
  , InternalEscapeRadius
  , InternalVanishingRadius
  ) where

import Language.Type
import Language.Environment
import Language.Value.Typecheck

type InternalX   = "[internal] x"
type InternalY   = "[internal] y"
type InternalDX  = "[internal] dx"
type InternalDY  = "[internal] dy"

type InternalViewerEnv env =
    ( '(InternalIterations, 'IntegerT) ':
      '(InternalStuck, 'BooleanT) ':
      '(InternalIterationLimit, 'IntegerT) ':
      '(InternalEscapeRadius, 'RealT) ':
      '(InternalVanishingRadius, 'RealT) ':
      ViewerEnv env)

type MissingViewerArgs env =
  ( NotPresent InternalX  env, NotPresent InternalY  env
  , NotPresent InternalDX env, NotPresent InternalDY env
  , NotPresent "color" env )

type ViewerEnv env =
  ( '(InternalX,  'RealT) ': '(InternalY,  'RealT) ':
    '(InternalDX, 'RealT) ': '(InternalDY, 'RealT) ':
    '("color", 'ColorT) ': env )
