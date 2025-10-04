{-# language UndecidableInstances #-}
module Language.Untyped.Code
  ( type Code
  , CodeF(..)
  , attachScope
  , promoteSetToLet
  ) where

import Language.Untyped.Value
import Data.Recursive
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad
import Fcf (Eval, Exp, Pure)
import Data.Kind

import Debug.Trace

type Code = Eval (FIX (CodeF Value))

data CodeF (value :: Type) (code :: Exp Type)
  = Let String value (Eval code)
  | LetBind String (Eval code) (Eval code)
  | Set String value
  | SetBind String (Eval code)
  | Call (Eval code)
  | Block [(Eval code)]
  | Pure value
  | DoWhile (Eval code)
  | IfThenElse value (Eval code) (Eval code)
  -- Render effect
  | Render String String value value value (Eval code)
  | HaltRender value
  | Blit value value value value
  | ClearTo value
  -- Draw effect
  | DrawPoint value
  | DrawCircle Bool value value
  | DrawLine value value
  | DrawRect Bool value value
  | SetStroke value
  | SetFill value
  | Clear
  -- List effect
  | Insert String value
  | Lookup String String value (Eval code) (Eval code)
  | ClearList String
  | Remove String String value
  | ForEach String String (Eval code)
  -- Output effect
  | Output String value
  -- Provide effect
  | Provide (Set String) (Eval code)

deriving instance (Eq   value,   Eq (Eval code)) => Eq   (CodeF value code)
deriving instance (Ord  value,  Ord (Eval code)) => Ord  (CodeF value code)
deriving instance (Show value, Show (Eval code)) => Show (CodeF value code)

vmap :: (a -> b) -> CodeF a c -> CodeF b c
vmap f = \case
  Let n v x -> Let n (f v) x
  LetBind n x y -> LetBind n x y
  Set n v -> Set n (f v)
  SetBind n c -> SetBind n c
  Call c -> Call c
  Block cs -> Block cs
  Pure v -> Pure (f v)
  DoWhile c -> DoWhile c
  IfThenElse c y n -> IfThenElse (f c) y n
  Render x y a b c w -> Render x y (f a) (f b) (f c) w
  HaltRender x -> HaltRender (f x)
  Blit a b c d -> Blit (f a) (f b) (f c) (f d)
  ClearTo a -> ClearTo (f a)
  DrawPoint x -> DrawPoint (f x)
  DrawCircle a b c -> DrawCircle a (f b) (f c)
  DrawLine a b -> DrawLine (f a) (f b)
  DrawRect a b c -> DrawRect a (f b) (f c)
  SetStroke a -> SetStroke (f a)
  SetFill a -> SetFill (f a)
  Clear -> Clear
  Insert s v -> Insert s (f v)
  Lookup a b c x y -> Lookup a b (f c) x y
  ClearList a -> ClearList a
  Remove a b c -> Remove a b (f c)
  ForEach a b c -> ForEach a b c
  Output a b -> Output a (f b)
  Provide a c -> Provide a c

instance EFunctor (CodeF v) where
  emap f = \case
    Let n v x -> Let n v (f x)
    LetBind n x y -> LetBind n (f x) (f y)
    Set n v -> Set n v
    SetBind n c -> SetBind n (f c)
    Call c -> Call (f c)
    Block cs -> Block (map f cs)
    Pure v -> Pure v
    DoWhile c -> DoWhile (f c)
    IfThenElse c y n -> IfThenElse c (f y) (f n)
    Render x y a b c w -> Render x y a b c (f w)
    HaltRender x -> HaltRender x
    Blit a b c d -> Blit a b c d
    ClearTo a -> ClearTo a
    DrawPoint x -> DrawPoint x
    DrawCircle a b c -> DrawCircle a b c
    DrawLine a b -> DrawLine a b
    DrawRect a b c -> DrawRect a b c
    SetStroke a -> SetStroke a
    SetFill a -> SetFill a
    Clear -> Clear
    Insert s v -> Insert s v
    Lookup a b c x y -> Lookup a b c (f x) (f y)
    ClearList a -> ClearList a
    Remove a b c -> Remove a b c
    ForEach a b c -> ForEach a b (f c)
    Output a b -> Output a b
    Provide a c -> Provide a (f c)

instance ETraversable (CodeF v) where
  etraverse f = \case
    Let n v x -> Let n v <$> f x
    LetBind n x y -> LetBind n <$> f x <*> f y
    Set n v -> pure $ Set n v
    SetBind n c -> SetBind n <$> f c
    Call c -> Call <$> f c
    Block cs -> Block <$> traverse f cs
    Pure v -> pure $ Pure v
    DoWhile c -> DoWhile <$> f c
    IfThenElse c y n -> IfThenElse c <$> f y <*> f n
    Render x y a b c w -> Render x y a b c <$> f w
    HaltRender x -> pure $ HaltRender x
    Blit a b c d -> pure $ Blit a b c d
    ClearTo a -> pure $ ClearTo a
    DrawPoint x -> pure $ DrawPoint x
    DrawCircle a b c -> pure $ DrawCircle a b c
    DrawLine a b -> pure $ DrawLine a b
    DrawRect a b c -> pure $ DrawRect a b c
    SetStroke a -> pure $ SetStroke a
    SetFill a -> pure $ SetFill a
    Clear -> pure $ Clear
    Insert s v -> pure $ Insert s v
    Lookup a b c x y -> Lookup a b c <$> f x <*> f y
    ClearList a -> pure $ ClearList a
    Remove a b c -> pure $ Remove a b c
    ForEach a b c -> ForEach a b <$> f c
    Output a b -> pure $ Output a b
    Provide a c -> Provide a <$> f c

data BindingError
  = Shadowing String
  | Unbound String
  deriving Show

type CodeWithEnv = Ann (CodeF (Set String, Value)) (Set String)
type CodeWithEnvF t = AnnF (CodeF (Set String, Value)) (Set String) (Pure t)

-- | Annotate each 'Code' AST node and @value@ with its environment.
withBindings :: (Set String, Code)
             -> Either BindingError (CodeWithEnvF (Set String, Code))
withBindings (env, code) = Ann env <$> case code of
  Let n v body -> do
    when (Set.member n env) (throwError (Shadowing n))
    pure (Let n (env, v) (Set.insert n env, body))

  LetBind n c body -> do
    when (Set.member n env) (throwError (Shadowing n))
    pure (LetBind n (env, c) (Set.insert n env, body))

  Set n v -> do
    unless (Set.member n env) (throwError (Unbound n))
    pure (Set n (env, v))

  SetBind n c -> do
    unless (Set.member n env) (throwError (Unbound n))
    pure (SetBind n (env, c))

  Render x y dim pos delta body -> do
    when (Set.member x env) (throwError (Shadowing x))
    when (Set.member y env) (throwError (Shadowing y))
    let env' = Set.insert x (Set.insert y env)
    pure (Render x y (env, dim) (env, pos) (env, delta) (env', body))

  Lookup lst n p yes no -> do
    when (Set.member n env) (throwError (Shadowing n))
    let env' = Set.insert n env
    pure (Lookup lst n (env', p) (env', yes) (env, no))

  Remove lst n p -> do
    when (Set.member n env) (throwError (Shadowing n))
    pure (Remove lst n (Set.insert n env, p))

  ForEach lst n body -> do
    when (Set.member n env) (throwError (Shadowing n))
    pure (ForEach lst n (Set.insert n env, body))

  Provide ns c -> do
    case Set.toList (Set.intersection ns env) of
      (n:_) -> throwError (Shadowing n)
      []    -> pure (Provide ns (env `Set.union` ns, c))

  etc -> pure (vmap (env,) $ emap @_ (env,) etc) -- (bimap (env,) (env,) etc)

attachScope :: Set String
            -> Code
            -> Either BindingError CodeWithEnv
attachScope = curry (unfoldM withBindings)

-- | Find Set/SetBind instructions for unbound variables,
-- and promote them to Let/LetBind instructions that introduce
-- the variable.
promoteSetToLet :: Set String -> Code -> Code
promoteSetToLet = curry (unfold phi)
  where
    phi :: (Set String, Code) -> CodeF Value (Pure (Set String, Code))
    phi (scope, ast) = trace (show ast) $ case ast of
      -- In a block, find any Set or SetBind instructions that
      -- are setting an unbound variable. Convert these to
      -- Let / LetBind, scoping over the remainder of the block.
      Block instrs ->
        let setsUnboundVar = \case
              Set v _     -> not (v `Set.member` scope)
              SetBind v _ -> not (v `Set.member` scope)
              _           -> False

            toBlock = \case { [x] -> x; xs -> Block xs }

            toLet = \case
              -- Set -> Let promotion
              (Set var value : more) ->
                [(Set.insert var scope,
                  Let var value (toBlock more))]

              -- SetBind -> LetBind promotion
              (SetBind var code : more) ->
                [(Set.insert var scope,
                  LetBind var code (toBlock more))]

              []  -> []
              etc -> error ("unreachable case in promoteSetToLet: " ++ show etc)

            (body, rest) = break setsUnboundVar instrs
        in case map (scope,) body ++ toLet rest of
          [oneInstr] -> phi oneInstr
          manyInstrs -> Block manyInstrs

      -- Instructions that modify the scope
      Let var value body ->
        Let var value (Set.insert var scope, body)
      LetBind var code body ->
        LetBind var (scope, code) (Set.insert var scope, body)
      Render x y dim pos delta body ->
        Render x y dim pos delta (Set.insert x (Set.insert y scope), body)
      Lookup lst n p yes no ->
        Lookup lst n p (Set.insert n scope, yes) (scope, no)
      ForEach lst n body ->
        ForEach lst n (Set.insert n scope, body)
      Provide names body ->
        Provide names (names `Set.union` scope, body)

      -- Default case: recurse without modifying the scope
      _ -> emap (scope,) ast
