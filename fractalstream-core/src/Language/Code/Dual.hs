{-# language AllowAmbiguousTypes #-}

-- | Building blocks for differentiating a value through a /tracked/
-- variable's shadow, rather than a single fixed target.
--
-- A tracked variable named @v@'s derivative lives in another, genuinely
-- separate runtime variable, @shadowName v@, in the same scope (not a
-- symbolic expression -- the dual-number transform over looped 'Code'
-- (forthcoming) needs this, since the loop body that updates @v@ runs an
-- unknown number of times). 'dValue' differentiates a single (non-looped)
-- 'Value' under this convention.
module Language.Code.Dual
  ( shadowName
  , Tracked
  , dValue
  , dualizeCode
  ) where

import FractalStream.Prelude
import Language.Value
import Language.Value.Derivative (derivativeWith)
import Language.Value.Reindex (reindexValue)
import Language.Code
import Language.Code.Reindex (reindexCode)
import Language.Code.Typecheck (letBind)
import Language.Typecheck
import Language.Parser.SourceRange
import qualified Data.Set as Set

-- | The names of variables currently tracked for differentiation.
type Tracked = Set String

-- | The (bracketed, collision-proof) name of a tracked variable's shadow.
-- User identifiers can't contain brackets or spaces, so this can never
-- collide with a user-written name.
shadowName :: String -> String
shadowName v = "[dual] " ++ v

-- | Differentiate @v@ with respect to the seed direction, using @tracked@'s
-- shadow convention: a tracked variable's derivative is read from its
-- shadow; anything else (an untracked variable, a constant, ...) is
-- locally constant (0). @blame@ only fills the "with respect to ..." slot
-- of a 'DiffNotImplemented' error (e.g. when @v@ contains a loop or an
-- unsupported node).
dValue :: String -> Tracked -> SourceRange -> Value et -> TC (Value et)
dValue blame tracked sr v = derivativeWith blame shadowOf sr v
  where
    shadowOf :: forall et'. Value et' -> Maybe (Value et')
    shadowOf (Var name ty _)
      | symbolVal name `Set.member` tracked
      = case someSymbolVal (shadowName (symbolVal name)) of
          SomeSymbol sname -> case lookupEnv sname ty (envProxy Proxy) of
            Found pf' -> Just (Var sname ty pf')
            _         -> Nothing
    shadowOf _ = Nothing

-- | Real or Complex: the two types 'dValue'/'dualizeCode' can differentiate
-- and track a shadow for. Everything else (Integer loop counters, Boolean
-- flags, ...) has no meaningful derivative and is left completely alone.
isDifferentiable :: TypeProxy ty -> Bool
isDifferentiable = \case
  ComplexType -> True
  RealType    -> True
  _           -> False

-- | Transform a 'Code' so that every Real/Complex @Let@/@Set@-bound
-- variable named in @tracked@ (extended as new @Let@s are discovered) gets
-- a shadow (see 'shadowName') holding its running derivative with respect
-- to the seed direction. A tracked variable must already have its shadow
-- declared (with its initial derivative) in the environment @code@ starts
-- in; @dualizeCode@ only introduces shadows for variables @code@ itself
-- declares via @Let@.
--
-- @blame@ only fills the "with respect to ..." slot of a
-- 'DiffNotImplemented' error. Only @Let@/@Set@/@Block@/@IfThenElse@/
-- @DoWhile@/@NoOp@ are supported; @ForEach@/@Lookup@/@DrawCommand@ are
-- rejected with a clear error (not used by potential-style numeric loop
-- bodies).
dualizeCode :: SourceRange -> String -> Tracked -> Code env -> TC (Code env)
dualizeCode sr blame tracked code0 = case code0 of

  Let pf name v body
    | isDifferentiable (typeOfValue v) -> do
        let ty  = typeOfValue v
            env = envProxy Proxy
        dv <- dValue blame tracked sr v
        letBind sr (symbolVal name) ty v env $ \env1 ->
          letBind sr (shadowName (symbolVal name)) ty (reindexValue env1 dv) env1 $ \env2 ->
            dualizeCode sr blame (Set.insert (symbolVal name) tracked) (reindexCode env2 body)
    | otherwise -> Let pf name v <$> dualizeCode sr blame tracked body

  Set pf name v
    | symbolVal name `Set.member` tracked, isDifferentiable (typeOfValue v) -> do
        let ty = typeOfValue v
        dv <- dValue blame tracked sr v
        case someSymbolVal (shadowName (symbolVal name)) of
          SomeSymbol sname -> case lookupEnv sname ty (envProxy Proxy) of
            -- `dv` (and `v`, if `v` reads `name` itself, e.g. `w <- w * x`)
            -- is evaluated against `name`'s OLD value, so the shadow must
            -- be updated before `name` itself is overwritten below.
            Found spf -> pure (Block [ Set spf sname dv, Set pf name v ])
            _ -> throwError (Advice sr
                   ("dualizeCode: internal error, `" ++ symbolVal name
                     ++ "` is tracked but its shadow is missing."))
    | otherwise -> pure (Set pf name v)

  Block stmts -> Block <$> traverse (dualizeCode sr blame tracked) stmts

  NoOp -> pure NoOp

  DoWhile cond body -> DoWhile cond <$> dualizeCode sr blame tracked body

  IfThenElse cond yes no ->
    IfThenElse cond <$> dualizeCode sr blame tracked yes <*> dualizeCode sr blame tracked no

  DrawCommand{} -> throwError (Advice sr "dualizeCode: draw commands are not supported.")
  Lookup{}      -> throwError (Advice sr "dualizeCode: list operations are not supported.")
  ForEach{}     -> throwError (Advice sr "dualizeCode: list operations are not supported.")
