module Test.Main where

import Prelude

import Control.Category (identity)
import Control.MonadPlus (guard)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.Record.Fill
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, singleton, unfoldr)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Effect (Effect)
import Effect.Aff (Aff(..))
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

test1 :: _
test1 =
  traverseRecord (Just :: Int -> Maybe Int) {
    a: 1
  , b: 2
  , c: 3
  }


main :: Effect Unit
main = runTest do
  suite "sequencePropsOf" do
    test "with Maybes" do
      testNothing :: Maybe _ <- pure $ sequencePropsOf {
        a: Just "Hello"
      , b: Nothing
      , c: 42
      }
      Assert.equal
        (Nothing :: Maybe {a :: String, b :: Maybe Int, c :: Int})
        testNothing
      testJust :: Maybe _ <- pure $ sequencePropsOf {
        a: Just "Hello"
      , c: 42
      }
      Assert.equal (Just {a: "Hello", c : 42}) testJust
      test2xMay :: Maybe _ <- pure $ sequencePropsOf {
        a: Just $ Just "Hello"
      , b: Just $ Nothing
      , c: 42
      }
      Assert.equal
        ((Just {a: Just "Hello", b: Nothing, c: 42})
         :: Maybe {a :: Maybe String, b :: Maybe Int, c :: Int})
        test2xMay
  suite "traverseProp" do
    test "homogeneous record" do
      testHomog1 :: Maybe {a :: Int} <- pure $
        traverseRecord (Just :: Int -> Maybe Int) {a : 1}
      Assert.equal (Just {a : 1}) testHomog1
      testHomog2 :: Maybe {a :: Int} <- pure $
        traverseRecord ((\x -> Nothing) :: Int -> Maybe Int) {a : 1}
      Assert.equal Nothing testHomog2
      testHomog3 :: Maybe {a :: Int, b :: Int} <- pure $
        traverseRecord
          ((\x -> if x < 0 then Nothing else Just x) :: Int -> Maybe Int)
          {a : 1, b : -1}
      Assert.equal Nothing testHomog3
      testHomog4 :: Maybe {a :: Int, b :: Int} <- pure $
        traverseRecord
          ((\x -> if x < 0 then Nothing else Just x) :: Int -> Maybe Int)
          {a : 1, b : 2}
      Assert.equal (Just {a: 1, b: 2}) testHomog4

      -- testHomog2 :: Maybe {a :: Int} <- pure $
      --   traverseRecord (identity :: Maybe Int -> Maybe Int) {a : 1}
      pure unit

      --tlog $ "add some tests"

tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow
