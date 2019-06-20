-- |
-- Module:      Language.Archetype.IntermediateRepresentation
-- Description: Intermediate Representation (IR) is a simplified representation
--              of Archetype types suitable as code generator input.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Intermediate Representation ('IR') is a simplified representation of
-- Archetype types suitable as code generator input.
module Language.Archetype.IntermediateRepresentation
    ( IR(..)
    , Position(..)
    , Location(..)

    -- * Primitive Types
    , PrimitiveType(..)
    , PrimitiveTypeAnnotation(..)

    -- * Type Definition
    , TypeDefinition(..)
    , TypeDefinitionBody(..)
    )
  where

--import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Data.Text (Text)
import qualified Dhall (Inject, Interpret)


data Position = Position Natural Natural
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data Location = Location
    { sourceName :: Text
    , text :: Text
    , startPosition :: Position
    , endPosition :: Position
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data PrimitiveType = PrimitiveType
    { name :: Text
    -- ^ Name of the primitive type.
    , location :: Location
    -- ^ Location where the primitive type was declared.
    , annotation :: PrimitiveTypeAnnotation
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data PrimitiveTypeAnnotation
    = PrimPi Text PrimitiveTypeAnnotation
    -- ^ @∀(a : Type) → <rest>@
    | PrimApp Text Text [Text]
    -- ^ @<type> <type-var> [...]@
    | PrimType
    -- ^ @Type@
  deriving stock (Generic, Show)
  -- TODO: Derived instances won't work correctly.
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data TypeDefinition = TypeDefinition
    { name :: Text
    -- ^ Name of the type.
    , location :: Location
    -- ^ Location where the type was defined.
    , definition :: TypeDefinitionBody
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data TypeDefinitionBody
    = App Text Text [Text]
    | Product [(Text, Text)] -- TODO: Better representation.
    | Sum Text (Maybe Text)
  deriving stock (Generic, Show)
  -- TODO: Derived instances won't work correctly.
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data IR = IR
    { primitives :: [PrimitiveType]
    -- ^ Primitives that the code generator is expected to understand.
    , types :: [TypeDefinition]
    -- ^ Type definitions that are the actual input for code generation.
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)
