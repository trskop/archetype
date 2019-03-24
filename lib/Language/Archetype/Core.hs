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
    )
  where

import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty)
import Data.Data (Data)
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
-- TODO: Bifunctor

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
