-- |
-- Module:      Language.Archetype.Core
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Language.Archetype.Core
    (
      Expression(..)
    , Binding(..)
    , untag
    )
  where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Data.Text (Text)
import qualified Dhall.Core as Dhall


data Binding ann a = Binding
    { name :: Text
    , typeAnnotation :: ann
    , value :: a
    , comment :: Text
    -- ^ Comment that is right before the type definition.
    }
  deriving
    ( Data
    , Eq
    , Foldable
    , Functor
    , Generic
    , Show
    , Traversable
    )

instance Bifunctor Binding where
    bimap f g = \binding@Binding{typeAnnotation, value} -> binding
        { typeAnnotation = f typeAnnotation
        , value = g value
        }

data Expression s a
    = TypeDeclaration (Binding (Maybe (Dhall.Expr s a)) (Dhall.Expr s a))
    -- ^
    -- @
    -- type <name> [: <type-annotation] = <value>
    -- @

    | PrimitiveTypeDeclaration (Binding (Dhall.Expr s a) (Maybe (Dhall.Expr s a)))
    -- ^
    -- @
    -- prim type <name> : <type-annotation> [= <value>]
    -- @

    | Tag s (Expression s a)
    -- ^ Allows us to annotate Archetype AST with e.g. source code location.

    | Import a
    -- ^ Import Archetype expressions.

    | Expression (NonEmpty (Expression s a))
    -- ^
    -- Represents file with more than one declaration\/import.  Similar as
    -- having top-level Let expression where the body is derived for us.
  deriving stock
    ( Data
    , Eq
--  , Foldable
--  , Functor
    , Generic
    , Show
--  , Traversable
    )
-- TODO: Applicative, Bifunctor, Monad, IsString?

untag :: Expression s a -> Expression t a
untag = \case
    TypeDeclaration b ->
        TypeDeclaration (bimap (fmap Dhall.denote) Dhall.denote b)

    PrimitiveTypeDeclaration b ->
        PrimitiveTypeDeclaration (bimap Dhall.denote (fmap Dhall.denote) b)

    Tag _ e ->
        untag e

    Import a ->
        Import a

    Expression es ->
        Expression (untag <$> es)
