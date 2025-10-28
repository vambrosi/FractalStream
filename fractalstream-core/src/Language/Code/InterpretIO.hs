{-# language AllowAmbiguousTypes #-}
module Language.Code.InterpretIO
  ( interpretToIO
  , eval
  , update
  , IORefTypeOfBinding
  , ScalarIORefM
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Code
import Language.Value.Evaluator
import Language.Draw

import Data.Indexed.Functor

import Data.IORef

data ScalarIORefM :: Environment -> Exp Type
type instance Eval (ScalarIORefM env) =
  StateT (Context IORefTypeOfBinding env) IO ()

data IORefTypeOfBinding :: Symbol -> FSType -> Exp Type
type instance Eval (IORefTypeOfBinding name t) = IORef (HaskellType t)

-- | Evaluate a value in the current environment
eval :: forall t env
      . Value '(env, t)
     -> StateT (Context IORefTypeOfBinding env) IO (HaskellType t)
eval v = do
  ctxRef <- get
  ctx <- mapContextM (\_ _ -> lift . readIORef) ctxRef
  pure (evaluate v ctx)


-- | Update a variable in the current environment
update :: forall name t env
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> TypeProxy t
       -> HaskellType t
       -> StateT (Context IORefTypeOfBinding env) IO ()
update pf _name ty v = withKnownType ty $ do
  ctx <- get
  let valueRef = getBinding ctx pf
  lift (writeIORef valueRef v)

interpretToIO :: forall env
               . DrawHandler ScalarIORefM
              -> Code env
              -> StateT (Context IORefTypeOfBinding env) IO ()
interpretToIO draw = indexedFold @ScalarIORefM phi
  where
    phi :: forall env'
         . CodeF ScalarIORefM env'
        -> StateT (Context IORefTypeOfBinding env') IO ()

    phi = \case

      Let pf name ve body -> recallIsAbsent (absentInTail pf) $ do
        ctxRef <- get
        value <- eval ve
        valueRef <- lift (newIORef value)
        let ctxRef' = Bind name (typeOfValue ve) valueRef ctxRef
        lift (evalStateT body ctxRef')

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

      DrawCommand d -> runDrawHandler draw d

      Lookup pfList listName listTy@(ListType itemTy) itemName pfNoItem _ env predicate action fallback ->
        recallIsAbsent pfNoItem $ do
        ctxRef <- get
        let go = \case
              [] -> fromMaybe (pure ()) fallback
              (item : items) -> do
                itemRef <- lift (newIORef item)
                let ctxRef' = Bind itemName itemTy itemRef ctxRef
                matches <- lift (evalStateT (eval predicate) ctxRef')
                case matches of
                  True -> lift (evalStateT action ctxRef')
                  False -> go items
        go =<< eval (withEnvironment env $ Var listName listTy pfList)

      ForEach pfList listName listTy@(ListType itemTy) itemName pfNoItem env _ body ->
        recallIsAbsent pfNoItem $ do
        ctxRef <- get
        let go = \case
              [] -> pure ()
              (item : items) -> do
                itemRef <- lift (newIORef item)
                let ctxRef' = Bind itemName itemTy itemRef ctxRef
                lift (evalStateT body ctxRef')
                go items
        go =<< eval (withEnvironment env $ Var listName listTy pfList)
