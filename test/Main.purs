module Test.Main where

import Prelude

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

      --tlog $ "add some tests"

tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow
