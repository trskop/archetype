-- |
-- Module:      Language.Archetype.BackEnd
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Language.Archetype.BackEnd
--  (
--  )
  where

import Data.Void (Void)
import GHC.Generics (Generic)

import Data.Text (Text)
import qualified Dhall (Interpret)
import qualified Dhall.Core as Dhall


data BackEndError = BackendError
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

data File = File
    { filename :: FilePath
    , content :: Text
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

data Response
    = Error BackEndError
    | Result [File]
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

data BackEnd = BackEnd
    { command :: FilePath
    , arguments :: [String]
    }

runBackEnd
    :: BackEnd
    -> Dhall.Expr Void Void
    -> m Response
runBackEnd = runBackEnd
