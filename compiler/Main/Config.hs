{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Main.Config
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main.Config
    ( Params(..)
    , defParams

    , EnvironmentVariable(..)
    , Backend(..)

    , Config(..)
    , emptyConfig
    , applyConfig
    )
  where

import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import Data.Monoid.Endo (Endo(Endo))
import Data.Output.Colour (ColourOutput(Auto))
import Data.Verbosity (Verbosity(Normal))
import qualified Dhall (Interpret)


data Params = Params
    { verbosity :: Verbosity
    , colourOutput :: ColourOutput
    }
  deriving (Generic, Show)
  deriving anyclass (Dhall.Interpret)

defParams :: Params
defParams = Params
    { verbosity = Normal
    , colourOutput = Auto
    }

data EnvironmentVariable = EnvironmentVariable
    { name :: String
    , value :: String
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

data Backend = Backend
    { command :: FilePath
    , arguments :: [String]
    , environment :: [EnvironmentVariable]
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

data Config = Config
    { verbosity :: Maybe Verbosity
    , colourOutput :: Maybe ColourOutput
    }
  deriving (Generic, Show)
  deriving anyclass (Dhall.Interpret)

emptyConfig :: Config
emptyConfig = Config
    { verbosity = Nothing
    , colourOutput = Nothing
    }

applyConfig :: Config -> Endo Params
applyConfig Config{colourOutput, verbosity} =
    Endo \Params{colourOutput = defColourOutput, verbosity = defVerbosity} ->
        Params
            { verbosity = fromMaybe defVerbosity verbosity
            , colourOutput = fromMaybe defColourOutput colourOutput
            }
