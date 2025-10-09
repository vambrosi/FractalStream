module Language.Code.Simulator
  ( simulate
  , eval
  , HaskellTypeM
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Code
import Language.Value.Evaluator

import Data.Indexed.Functor

data HaskellTypeM :: Type -> Environment -> Exp Type
type instance Eval (HaskellTypeM s env) =
  State (Context HaskellTypeOfBinding env, s) ()

-- | Update a variable in the current environment
update :: forall name t env s
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> TypeProxy t
       -> HaskellType t
       -> State (Context HaskellTypeOfBinding env, s) ()
update pf _name t v = withKnownType t (modify' (\(x,y) -> (setBinding pf v x, y)))

-- | Evaluate a value in the current environment
eval :: forall t env s
      . Value '(env, t)
     -> State (Context HaskellTypeOfBinding env, s) (HaskellType t)
eval v = do
  ctx <- fst <$> get
  pure (evaluate v ctx)

-- | Run some 'Code' by interpreting it into a state monad.
-- The 's' parameter allows for extra state that may be used
-- by the effects handlers.
simulate :: forall effs env s
          . Handlers effs (HaskellTypeM s)
         -> Code effs env
         -> State (Context HaskellTypeOfBinding env, s) ()
simulate handlers = indexedFold @(HaskellTypeM s) $ \case

  Let pf name vc body -> recallIsAbsent (absentInTail pf) $ do
    (ctx, s) <- get
    value <- eval vc
    let ctx' = Bind name (typeOfValue vc) value ctx
        (result, (Bind _ _ _ ctx'', s'')) = runState body (ctx', s)
    put (ctx'', s'')
    pure result

  Set pf name vc -> do
    result <- eval vc
    update pf name (typeOfValue vc) result

  Block stmts -> sequence_ stmts

  NoOp -> pure ()

  DoWhile cond body -> loop
    where loop = do
            body
            tf <- eval cond
            if tf then loop else pure ()

  IfThenElse test t f -> do
    tf <- eval test
    if tf then t else f

  Effect effectType env eff ->
    case getHandler effectType handlers of
      Handle _ handle -> handle (envProxy env) eff
