-- |
-- Module:      Language.Archetype.FrontEnd
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Language.Archetype.FrontEnd
--  (
--  )
  where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Void (Void)

import Control.Monad.Except (MonadError, throwError)
import Data.Text (Text)
import qualified Dhall.Core as Dhall
import Dhall.Parser (Src)
import qualified Dhall.Parser as Dhall (ParseError(..), Parser(unParser))
import qualified Text.Megaparsec as Megaparsec (parse)

import qualified Language.Archetype.Core as Archetype
import qualified Language.Archetype.Parser as Archetype


type Import = Archetype.Import -- TODO

type ParsingError = Dhall.ParseError

parser
    :: String
    -- ^ User-friendly name describing the input expression, used in parsing
    -- error messages.
    -> Text
    -> Either ParsingError (Archetype.Expression Src Import)
parser name input = first toParsingError (parse name input)
  where
    toParsingError e = Dhall.ParseError{Dhall.unwrap = e, Dhall.input}
    parse = Megaparsec.parse (Dhall.unParser Archetype.expression)

class Monad m => ImportResolutionMonad m

data ImportResolutionError

resolveImports
    :: Archetype.Expression Src Import
    -> m (Either ImportResolutionError (Archetype.Expression Src Void))
resolveImports = resolveImports

data AnalysisError

-- | Transform Archetype expression into intermediate representation (Dhall
-- Expression).
analyser
    :: Archetype.Expression src Void
    -> Either AnalysisError (Dhall.Expr src Void)
analyser = analyser

data FrontEndError
    = ParsingError ParsingError
    | ImportResolutionError ImportResolutionError
    | AnalysisError AnalysisError

frontEnd
    ::  ( ImportResolutionMonad m
        , MonadError FrontEndError m
        )
    => String
    -- ^ User-friendly name describing the input expression, used in parsing
    -- error messages.
    -> Text
    -> m (Dhall.Expr Src Void)
frontEnd description source =
    throwLeftAs ParsingError (parser description source)
    >>= (resolveImports >=> throwLeftAs ImportResolutionError)
    >>= throwLeftAs AnalysisError . analyser

throwLeftAs :: MonadError e' m => (e -> e') -> Either e a -> m a
throwLeftAs f = either (throwError . f) pure
