-- TODO: explicit exports
module Data.Record.Fill where

import Prelude

-- import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex, mapping)
import Prim.Row as Row
import Record.Builder (Builder)
import Record.Builder as Builder

-- TODO: need something to go in the opposite direction of SequencePropOf
--     : to generate an "Optionalized (empty) Record", given a type of
--     : the input record.

-- SequencePropOf, TraverseProp, and related code taked from purescript-heterogeneous
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



data TraverseProp (f :: Type -> Type) k = TraverseProp k

instance traverseProp ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym b rb rc
  , Mapping k a (f b)
  ) =>
  FoldingWithIndex
    (TraverseProp f k)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    a
    (f (Builder { | ra } { | rc }))
  where
  foldingWithIndex (TraverseProp k) prop rin a =
    (>>>) <$> rin <*> (Builder.insert prop <$> mapping k a)

traverseRecord :: forall f k rin rout.
  Applicative f =>
  HFoldlWithIndex (TraverseProp f k) (f (Builder {} {})) { | rin } (f (Builder {} { | rout })) =>
  k ->
  { | rin } ->
  f { | rout }
traverseRecord k =
  map (flip Builder.build {})
    <<< hfoldlWithIndex (TraverseProp k :: TraverseProp f k) (pure identity :: f (Builder {} {}))

