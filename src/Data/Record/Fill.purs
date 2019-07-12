-- TODO: explicit exports
module Data.Record.Fill where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldl, class HFoldlWithIndex, hfoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex, mapping)
import Prim.Row as Row
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder


-- SequencePropOf and related code taked from purescript-heterogeneous
-- test code written by Nate Faubion

data SequencePropOf (f :: Type -> Type) = SequencePropOf

instance sequencePropOf_1 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym a rb rc
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    (f a)
    (f (Builder { | ra } { | rc }))
  where
  foldingWithIndex _ prop rin a =
    (>>>) <$> rin <*> (Builder.insert prop <$> a)
else
instance sequencePropOf_2 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    x
    (f (Builder { | ra } { | rc }))
  where
  foldingWithIndex _ prop rin x =
    (_ >>> Builder.insert prop x) <$> rin

sequencePropsOf :: forall f rin rout.
  Applicative f =>
  HFoldlWithIndex (SequencePropOf f) (f (Builder {} {})) { | rin } (f (Builder {} { | rout })) =>
  { | rin } ->
  f { | rout }
sequencePropsOf =
  map (flip Builder.build {})
<<< hfoldlWithIndex (SequencePropOf :: SequencePropOf f) (pure identity :: f (Builder {} {}))

