-- | Re-index a 'Code' statement from one environment into another.
--
-- Mirrors 'Language.Value.Reindex.reindexValue', extended to 'Code's only
-- binding construct ('Let'): every variable the code references or assigns
-- must be present in the target environment at the same type (typically
-- because the target is an /extension/ of the source, e.g. with extra
-- bindings spliced in elsewhere). Embedded 'Value's (conditions, assigned
-- expressions) are re-indexed directly via 'reindexValue'.
module Language.Code.Reindex
  ( reindexCode
  ) where

import Language.Code
import Language.Value.Reindex (reindexValue)

-- | Rebuild a 'Code' so that it is indexed by @tgt@ instead of its own
-- (existing) source environment — pure environment weakening, no
-- substitution. Errors (at runtime) if a referenced or assigned variable is
-- not present in the target environment, or if a 'Let'-bound name collides
-- with the target environment; callers are responsible for ensuring
-- neither happens (e.g. by only ever inserting fresh, bracketed names into
-- the target environment beyond what the source already has).
reindexCode :: forall src tgt. EnvironmentProxy tgt -> Code src -> Code tgt
reindexCode = go
  where
    go :: forall s t. EnvironmentProxy t -> Code s -> Code t
    go t code0 = withEnvironment t $ case code0 of

      Let _ name v body ->
        let ty = typeOfValue v
        in case lookupEnv' name t of
             Absent' pf' -> recallIsAbsent pf' $
               Let bindingEvidence name (reindexValue t v) (go (BindingProxy name ty t) body)
             _ -> error "reindexCode: let-bound name collides with the target environment"

      Set _ name v ->
        let ty = typeOfValue v
        in case lookupEnv name ty t of
             Found pf' -> Set pf' name (reindexValue t v)
             _ -> error "reindexCode: assigned variable is not present in the target environment"

      Block stmts -> Block (map (go t) stmts)

      NoOp -> NoOp

      DoWhile cond body -> DoWhile (reindexValue t cond) (go t body)

      IfThenElse cond yes no -> IfThenElse (reindexValue t cond) (go t yes) (go t no)

      DrawCommand{} -> error "reindexCode: draw commands are not supported"
      Lookup{}      -> error "reindexCode: list operations are not supported"
      ForEach{}     -> error "reindexCode: list operations are not supported"
