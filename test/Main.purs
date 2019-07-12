module Test.Main where

import Prelude

import Prelude

import Control.MonadPlus (guard)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, singleton, unfoldr)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Partial.Unsafe (unsafePartial)


test :: Maybe _
test =
  sequencePropsOf
    { a: Just "Hello"
    , b: Nothing
    , c: 42
}

main :: Effect Unit
main = runTest do
  suite "suite 1" do
    test "test 1" do
      tlog $ "add some tests"

tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow
