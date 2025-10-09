{-# language QuasiQuotes #-}

module Language.Effect.ListSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Value.Evaluator
import Language.Code.Parser
import Language.Code.Simulator
import Language.Effect.List
import Language.Effect

import qualified Data.Map as Map
import Text.RawString.QQ

runWithXY :: forall xt yt
           . Scalar xt
          -> Scalar yt
          -> [Double]
          -> String
          -> Either String [Double]
runWithXY (Scalar xt x) (Scalar yt y) list0 input = withKnownType xt $ withKnownType yt $
  let ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @"y") yt y
          $ EmptyContext
      getList v = do
        (_, xs) <- get
        pure (v, xs)
  in fmap (snd . (`evalState` (ctx, list0)) . (>>= getList) . simulate (Handler listHandler NoHandler))
   $ parseCode (EP (ParseEff listEffectParser NoEffs)) (envProxy Proxy) Map.empty input

-- | Mix in list manipulation effects
listHandler :: EffectHandler (List "test" 'RealT) (HaskellTypeM [Double])
listHandler = Handle Proxy handle
  where
    handle :: forall env
            . EnvironmentProxy env
           -> List "test" 'RealT (HaskellTypeM [Double]) env
           -> State (Context HaskellTypeOfBinding env, [Double]) ()
    handle _ = \case

      Insert _ _ soe _ v -> do
        x <- eval v
        case soe of
          Start -> modify' (\(ctx, s) -> (ctx, x : s))
          End   -> modify' (\(ctx, s) -> (ctx, s ++ [x]))

      Lookup _ _ _ pf _ _ test match miss -> recallIsAbsent pf $ do
        let test' item = do
              (ctx, _) <- get
              let ctx' = Bind Proxy RealType item ctx
              pure (evaluate test ctx')

            go [] = case miss of
                Nothing     -> pure ()
                Just action -> action
            go (item:items) = test' item >>= \case
              False -> go items
              True  -> do -- evaluate continuation with "item" bound
                (ctx, s) <- get
                let ctx' = Bind Proxy RealType item ctx
                    (result, (Bind _ _ _ ctx'', s'')) = runState match (ctx', s)
                put (ctx'', s'')
                pure result
        (_, items) <- get
        go items

      ClearList _ _ -> modify' (\(ctx, _) -> (ctx, []))

      Remove _ _ _ pf _ test -> recallIsAbsent pf $ do
        (ctx, items) <- get
        let reject item = evaluate test (Bind Proxy RealType item ctx)
        put (ctx, filter (not . reject) items)

      ForEach _ _ _ pf _ _ body -> recallIsAbsent pf $ do
        items <- snd <$> get
        forM_ items $ \item -> do
          (ctx, s) <- get
          let ctx' = Bind Proxy RealType item ctx
              (Bind _ _ _ ctx'', s'') = execState body (ctx', s)
          put (ctx'', s'')

spec :: Spec
spec = do

  describe "when using list effects in code" $ do

    it "can parse embedded list effects" $ do

      let p1 = [r|
x <- 0
for each item in test do
  x <- x + item
y <- x
insert y at end of test|]
      runWithXY (Scalar RealType 5) (Scalar RealType 7) [1,2,3,4,5] p1
        `shouldBe` Right [1,2,3,4,5,15]

      let p2 = [r|
x <- 3
remove each item matching item <= x from test|]
      runWithXY (Scalar RealType 5) (Scalar RealType 7) [1,2,3,4,5] p2
        `shouldBe` Right [4,5]

      let p3 = [r|
x <- 3
if |x - y| < 2 then
  remove all items from test|]

      runWithXY (Scalar RealType 1) (Scalar RealType 10) [1,2,3,4,5] p3
        `shouldBe` Right [1,2,3,4,5]
      runWithXY (Scalar RealType 1) (Scalar RealType 4) [1,2,3,4,5] p3
        `shouldBe` Right []

{-
spec :: Spec
spec = do
  pure () -- todo
-}
