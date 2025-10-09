{-# language AllowAmbiguousTypes #-}
module Language.Code.InterpretIO
  ( interpretToIO
  , interpretToIO_
  , eval
  , eval'
  , update
  , update'
  , IORefTypeOfBinding
  , ScalarIORefM
  , ScalarIORefMWith
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Code
import Language.Value.Evaluator

import Data.Indexed.Functor

import Data.IORef

data ScalarIORefM :: Environment -> Exp Type
type instance Eval (ScalarIORefM env) =
  StateT (Context IORefTypeOfBinding env) IO ()

data ScalarIORefMWith :: Type -> Environment -> Exp Type
type instance Eval (ScalarIORefMWith s env) =
  StateT (Context IORefTypeOfBinding env, s) IO ()

data IORefTypeOfBinding :: Symbol -> FSType -> Exp Type
type instance Eval (IORefTypeOfBinding name t) = IORef (HaskellType t)

-- | Evaluate a value in the current environment
eval :: forall t env s
      . Value '(env, t)
     -> StateT (Context IORefTypeOfBinding env, s) IO (HaskellType t)
eval v = do
  ctxRef <- fst <$> get
  ctx <- mapContextM (\_ _ -> lift . readIORef) ctxRef
  pure (evaluate v ctx)

-- | Evaluate a value in the current environment
eval' :: forall t env
      . Value '(env, t)
     -> StateT (Context IORefTypeOfBinding env) IO (HaskellType t)
eval' v = do
  ctxRef <- get
  ctx <- mapContextM (\_ _ -> lift . readIORef) ctxRef
  pure (evaluate v ctx)

-- | Update a variable in the current environment
update :: forall name t env s
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> TypeProxy t
       -> HaskellType t
       -> StateT (Context IORefTypeOfBinding env, s) IO ()
update pf _name ty v = withKnownType ty $ do
  ctx <- fst <$> get
  let valueRef = getBinding ctx pf
  lift (writeIORef valueRef v)

-- | Update a variable in the current environment
update' :: forall name t env
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> TypeProxy t
       -> HaskellType t
       -> StateT (Context IORefTypeOfBinding env) IO ()
update' pf _name ty v = withKnownType ty $ do
  ctx <- get
  let valueRef = getBinding ctx pf
  lift (writeIORef valueRef v)

interpretToIO :: forall effs env0
               . Handlers effs ScalarIORefM
              -> Code effs env0
              -> StateT (Context IORefTypeOfBinding env0) IO ()
interpretToIO handlers = fro (Proxy @env0)
                       . interpretToIO_ (mapHandlers to fro handlers)
  where
    to  :: forall env pxy
         . pxy env
        -> StateT (Context IORefTypeOfBinding env) IO ()
        -> StateT (Context IORefTypeOfBinding env, ()) IO ()
    to _ s = do
      (ctx, _) <- get
      (result, ctx') <- lift (runStateT s ctx)
      put (ctx', ())
      pure result

    fro :: forall env pxy
         . pxy env
        -> StateT (Context IORefTypeOfBinding env, ()) IO ()
        -> StateT (Context IORefTypeOfBinding env) IO ()
    fro _ s = do
      ctx <- get
      (result, (ctx', _)) <- lift (runStateT s (ctx, ()))
      put ctx'
      pure result

interpretToIO_ :: forall effs env s
                . Handlers effs (ScalarIORefMWith s)
               -> Code effs env
               -> StateT (Context IORefTypeOfBinding env, s) IO ()
interpretToIO_ handlers =
  indexedFold @(ScalarIORefMWith s) $ \case

    Let pf name ve body -> recallIsAbsent (absentInTail pf) $ do
      (ctxRef, _) <- get
      value <- eval ve
      valueRef <- lift (newIORef value)
      let ctxRef' = Bind name (typeOfValue ve) valueRef ctxRef
      s <- snd <$> get
      lift (evalStateT body (ctxRef', s))

    Set pf name ve -> do
      value <- eval ve
      update pf name (typeOfValue ve) value

    Block stmts -> sequence_ stmts

    NoOp -> pure ()

    DoWhile cond body -> loop
      where loop = do
              body
              tf <- eval cond
              if tf then loop else pure ()

    IfThenElse test yes no -> do
      tf <- eval test
      if tf then yes else no

    Effect effectType env eff ->
      case getHandler effectType handlers of
        Handle _ handle -> handle (envProxy env) eff
