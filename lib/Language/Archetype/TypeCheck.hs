-- |
-- Module:      Language.Archetype.TypeCheck
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Language.Archetype.TypeCheck
    (
      Typer
    , typeWith
    , typeWithA
    , typeOf
    , checkContext

    , TypeError(..)
    , TypeMessage(..)
    )
  where

import Prelude (undefined)

import Data.Either (Either)
import Data.Eq (Eq)
import Data.Text (Text)
import Text.Show (Show)

import qualified Language.Archetype.Core as Core
import Dhall.Context (Context)
import qualified Dhall.Context as Context
import Dhall.TypeCheck (X, absurd)


type Typer a = forall s. a -> Core.Expression s a

-- | A structured type error that includes context
data TypeError s a = TypeError
    { context     :: Context (Core.Expression s a)
    , current     :: Core.Expression s a
    , typeMessage :: TypeMessage s a
    }

-- | The specific type error
data TypeMessage s a
    = UnboundVariable Text
    -- TODO: Implement other cases.
  deriving (Show)

typeWith
    :: Context (Core.Expression s X)
    -> Core.Expression s X
    -> Either (TypeError s X) (Core.Expression s X)
typeWith ctx expr = do
    checkContext ctx
    typeWithA absurd ctx expr

typeOf
    :: Core.Expression s X
    -> Either (TypeError s X) (Core.Expression s X)
typeOf = typeWith Context.empty

typeWithA
    :: Eq a
    => Typer a
    -> Context (Core.Expression s a)
    -> Core.Expression s a
    -> Either (TypeError s a) (Core.Expression s a)
typeWithA _ = undefined

checkContext
    :: Context (Core.Expression s X)
    -> Either (TypeError s X) ()
checkContext = undefined
