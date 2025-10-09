{-# language AllowAmbiguousTypes #-}

module Language.Code
  ( module Language.Value
  , module Language.Effect
  , Code
  , CodeF(..)
  , SomeCode(..)
  , transformValues
  , set
  , let_
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Effect
import Data.Indexed.Functor

data SomeCode where
  SomeCode :: forall effs env. Code effs env -> SomeCode

---------------------------------------------------------------------------------
-- Code
---------------------------------------------------------------------------------

type Code effs = CodeF effs (FIX (CodeF effs))

instance Show (Code effs env) where show _ = "<code>"

-- | The Code type is used recursively at different Type parameters,
-- and also at different Environments (in a Let binding). That means
-- we need to use both the environment *and* the type as indices
-- in order to make an indexed functor.
data CodeF (effs :: [Effect])
           (code :: Environment -> Exp Type)
           (env :: Environment) where

  -- | Define a new variable scoped to the given Code.
  Let :: forall name ty env effs code
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty ( '(name, ty) ': env)
      -> Proxy (name :: Symbol)
      -> Value '(env, ty)
      -> Eval (code ( '(name, ty) ': env ))
      -> CodeF effs code env

  -- | Update the value of a variable
  Set :: forall name ty env effs code
        . (KnownSymbol name, KnownEnvironment env)
       => NameIsPresent name ty env
       -> Proxy name
       -> Value '(env, ty)
       -> CodeF effs code env

  -- | A block of statements with VoidT type, followed by a
  -- statement with any type. The type of the block is the
  -- type of the final statement.
  Block :: forall effs env code
         . KnownEnvironment env
        => [Eval (code env)]
        -> CodeF effs code env

  -- | Do nothing
  NoOp :: forall env effs code
        . KnownEnvironment env
       => CodeF effs code env

  -- | Do-while loop
  DoWhile :: forall env effs code
           . KnownEnvironment env
          => Value '(env, 'BooleanT)
          -> Eval (code env)
          -> CodeF effs code env

  -- | If/else statement
  IfThenElse :: forall env effs code
       . KnownEnvironment env
      => Value '(env, 'BooleanT)
      -> Eval (code env)
      -> Eval (code env)
      -> CodeF effs code env

  -- | Embedded effect sub-language
  Effect :: forall env effs eff code
          . (HasEffect eff effs, KnownEnvironment env)
         => Proxy eff
         -> Proxy env
         -> eff code env
         -> CodeF effs code env

---------------------------------------------------------------------------------
-- Indexed functor instance for Code
---------------------------------------------------------------------------------

instance IFunctor (CodeF eff) where

  type IndexProxy (CodeF eff) = EnvironmentProxy

  toIndex = \case
    Let {} -> envProxy Proxy
    Set {} -> envProxy Proxy
    Block {} -> envProxy Proxy
    NoOp -> envProxy Proxy
    DoWhile {} -> envProxy Proxy
    IfThenElse {} -> envProxy Proxy
    Effect _ pxy _ -> envProxy pxy

  imap :: forall a b env
        . (forall env'. EnvironmentProxy env' -> Eval (a env') -> Eval (b env'))
       -> CodeF eff a env
       -> CodeF eff b env
  imap f code =
    let env = toIndex code
    in case code of
      Let pf (n :: Proxy name) v e -> recallIsAbsent (removeName @name pf) $
        Let pf n v (f (BindingProxy n (typeOfValue v) env) e)
      Set pf n v -> Set pf n v
      Block body -> Block (map (f env) body)
      NoOp -> NoOp
      DoWhile cond body -> DoWhile cond (f env body)
      IfThenElse cond yes no -> IfThenElse cond (f env yes) (f env no)
      Effect eff envp e -> Effect eff envp (imap f e)

{-
    let env :: EnvironmentProxy (Env et)
        env = case toIndex x of { EnvType _ -> envProxy (Proxy @(Env et)) }
        index :: forall i. TypeProxy i -> EnvTypeProxy '(Env et, i)
        index i = withEnvironment env (EnvType i)
    in case x of
      Let pf (n :: Proxy name) (vt :: TypeProxy vt) cv t b ->
        recallIsAbsent (removeName @name pf) $
          let env' :: EnvironmentProxy ( '(name, vt) ': Env et)
              env' = withKnownType vt
                     $ withEnvironment env
                     $ BindingProxy n vt (envProxy (Proxy @(Env et)))
              index' :: forall i r
                      . TypeProxy i
                     -> TypeProxy r
                     -> EnvTypeProxy '( '(name, i) ': Env et, r)
              index' i r = withKnownType i
                         $ withEnvironment env' (EnvType @( '(name, i) ': Env et) r)
          in withEnvironment env' (Let pf n vt (f (index vt) cv) t (f (index' vt t) b))
      Set pf n ty c -> Set pf n ty (f (index ty) c)
      Block t cs c -> Block t (map (f (index VoidType)) cs) (f (index t) c)
      NoOp -> NoOp
      While c b -> While c (f (index VoidType) b)
      IfThenElse t v yes no -> IfThenElse t v (f (index t) yes) (f (index t) no)
      Effect e en t c -> Effect e en t (imap f c)
-}

---------------------------------------------------------------------------------
-- Apply an operation to the @Value@s in a @Code@
---------------------------------------------------------------------------------

transformValues :: forall eff env
                . (forall et. Value et -> Value et)
               -> Code eff env
               -> Code eff env
transformValues f = indexedFold @(FIX (CodeF eff)) $ \case
  Let pf n v e -> Let pf n (f v) e
  Set pf n v -> Set pf n (f v)
  DoWhile cond body -> DoWhile (f cond) body
  IfThenElse cond yes no -> IfThenElse (f cond) yes no
  -- FIXME this needs a case that recurses into effects too
  other    -> other

---------------------------------------------------------------------------------
-- Indexed traversable instance
---------------------------------------------------------------------------------

instance ITraversable (CodeF effs) where
  isequence = \case
    Let pf n v c -> Let pf n v <$> c
    Set pf n v -> pure (Set pf n v)
    Block block -> Block  <$> sequenceA block
    NoOp -> pure NoOp
    DoWhile c body -> DoWhile c <$> body
    IfThenElse v yes no -> IfThenElse v <$> yes <*> no
    Effect eff env e -> Effect eff env <$> isequence e

---------------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------------

-- | Bind a name to a value.
-- This is the same as using the 'Let' constructor directly,
-- except that it saves you the syntactic noise of using
-- a 'Proxy', by using type applications instead.
--
--     let_ @"foo" value code
--
--     Let bindingEvidence (Proxy @"foo") value code
--
let_ :: forall name env effs ty
      . (NotPresent name env, KnownSymbol name, KnownEnvironment env)
     => Value '(env, ty)
     -> Code effs ('(name, ty) ': env)
     -> Code effs env
let_ = Let bindingEvidence (Proxy @name)


-- | Set the value of a variable.
-- This is the same as using the 'Set' constructor directly,
-- except that it saves you the syntactic noise of using a
-- 'Proxy', by using type applications instead.
--
--     set @"foo" value
--
-- vs
--
--     Set bindingEvidence (Proxy @"foo") value
--
set :: forall name env effs ty
     . ( Required name env ~ ty, NotPresent name (env `Without` name)
       , KnownSymbol name, KnownEnvironment env)
    => Value '(env, ty)
    -> Code effs env
set = Set bindingEvidence (Proxy @name)
