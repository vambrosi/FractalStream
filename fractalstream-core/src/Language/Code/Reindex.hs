-- | Re-index a 'Code' statement from one environment into another.
module Language.Code.Reindex
  ( reindexCode
  ) where

import Language.Code
import Language.Value.Reindex (reindexValue)

-- | Rebuild a 'Code' so it is indexed by @tgt@ instead of its source
-- environment (weakening only, no substitution).
--
-- Precondition, checked with 'error':
--
-- * every variable the code reads or assigns is in @tgt@ at the same type;
-- * no 'Let'-bound name is already in @tgt@.
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
