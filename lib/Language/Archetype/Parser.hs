-- |
-- Module:      Language.Archetype.Parser
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Language.Archetype.Parser
    ( expression
    , expressionA
    , Import

    , lineComment
    , blockComment
    , whitespace
    , nonemptyWhitespace
    )
  where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>), optional)
import Control.Monad (guard)
import Data.Functor (void)
import Data.List.NonEmpty (some1)
import Data.Monoid (mconcat)

import qualified Dhall.Parser as Dhall
import Data.Text (Text)
import qualified Data.Text as Text (cons, singleton)
import qualified Dhall.Core as Dhall (Expr)
import Dhall.Parser (Src)
import qualified Dhall.Parser as Dhall (exprA)
import qualified Text.Megaparsec as Megaparsec
import Text.Parser.Char (char, satisfy, text)
import Text.Parser.Combinators ((<?>), choice, skipMany, skipSome, try)

import qualified Language.Archetype.Core as Archetype

-- TODO: Imports, and move this data type somewhere more reasonable.
data Import = Import
  deriving (Show)

expression :: Dhall.Parser (Archetype.Expression Src Import)
expression = expressionA (fail "Imports not supported") -- TODO: Imports

expressionA
    :: Dhall.Parser a
    -> Dhall.Parser (Archetype.Expression Src a)
expressionA importParser = Archetype.Expression
    <$> some1
            (   whitespace
            *>  (   typeDeclaration importParser
                <|> primitiveTypeDeclaration importParser
                )
            )
    <* whitespaceOrComment
    <* Megaparsec.eof
  where
    whitespaceOrComment = Megaparsec.many
        ( choice [whitespaceChunk, void lineComment, void blockComment]
        <?> "whitespace or comment"
        )

typeDeclaration
    :: Dhall.Parser a
    -> Dhall.Parser (Archetype.Expression Src a)
typeDeclaration importParser = Archetype.TypeDeclaration
    <$> ( text "type"
        *> binding (optional (annotation importParser)) (body importParser)
        )

primitiveTypeDeclaration
    :: Dhall.Parser a
    -> Dhall.Parser (Archetype.Expression Src a)
primitiveTypeDeclaration importParser = Archetype.PrimitiveTypeDeclaration
    <$> ( text "prim"
        *> whitespace
        *> text "type"
        *> binding (annotation importParser) (optional (body importParser))
        )

binding
    :: Dhall.Parser ann
    -> Dhall.Parser a
    -> Dhall.Parser (Archetype.Binding ann a)
binding annotationParser valueParser = do
    comment <- whitespace
        *>  (    mconcat
            <$> Megaparsec.many
                (lineComment <|> blockComment <|> whitespaceChunk')
            )
        <* whitespace

    name <- label
    whitespace
    typeAnnotation <- annotationParser
    whitespace
    value <- valueParser
    whitespace

    pure Archetype.Binding
        { name
        , typeAnnotation
        , value
        , comment
        }
  where
    whitespaceChunk' = "" <$ whitespaceChunk

-- TODO: This doesn't work! Dhall expression parser will consume anything that
-- resemples Dhall.  We need to provide modified Dhall parser.
annotation :: Dhall.Parser a -> Dhall.Parser (Dhall.Expr Src a)
annotation importParser =
    char ':' *> Dhall.exprA importParser

-- TODO: This doesn't work! Dhall expression parser will consume anything that
-- resemples Dhall.  We need to provide modified Dhall parser.
body :: Dhall.Parser a -> Dhall.Parser (Dhall.Expr Src a)
body importParser =
    char '=' *> Dhall.exprA importParser

label :: Dhall.Parser Text
label = try do
    l <- Text.cons
        <$> satisfy (\c -> alpha c || c == '_')
        <*> takeWhile \c ->
                alpha c || digit c || c == '_' || c == '-' || c == '/'
    l <$ guard (l /= "prim" && l /= "type")
  where
    alpha :: Char -> Bool
    alpha c = ('\x41' <= c && c <= '\x5A') || ('\x61' <= c && c <= '\x7A')

    digit :: Char -> Bool
    digit c = '\x30' <= c && c <= '\x39'

lineComment :: Dhall.Parser Text
lineComment = do
    _ <- text "--"
    s <- takeWhile \c -> ('\x20' <= c && c <= '\x10FFFF') || c == '\t'
    (s <>) <$> endOfLine
  where
    endOfLine = (Text.singleton <$> char '\n') <|> text "\r\n"

blockComment :: Dhall.Parser Text
blockComment = text "{-" *> blockCommentContinue
  where
    blockCommentChunk :: Dhall.Parser Text
    blockCommentChunk = choice
        [ blockComment  -- Nested block comment
        , characters
        , character
        , endOfLine
        ]
      where
        characters = takeWhile1 \c ->
            '\x20' <= c && c <= '\x10FFFF' && c /= '-' && c /= '{'
            || c == '\n' || c == '\t'

        character = Text.singleton <$> satisfy \c ->
            '\x20' <= c && c <= '\x10FFFF'
            || c == '\n' || c == '\t'

        endOfLine = text "\r\n"

    blockCommentContinue :: Dhall.Parser Text
    blockCommentContinue = endOfComment <|> continue
      where
        endOfComment = "" <$ text "-}"

        continue = (<>)
            <$> blockCommentChunk
            <*> blockCommentContinue

takeWhile :: (Char -> Bool) -> Dhall.Parser Text
takeWhile predicate = Dhall.Parser (Megaparsec.takeWhileP Nothing predicate)

takeWhile1 :: (Char -> Bool) -> Dhall.Parser Text
takeWhile1 predicate = Dhall.Parser (Megaparsec.takeWhile1P Nothing predicate)

whitespace :: Dhall.Parser ()
whitespace = skipMany whitespaceChunk

nonemptyWhitespace :: Dhall.Parser ()
nonemptyWhitespace = skipSome whitespaceChunk

whitespaceChunk :: Dhall.Parser ()
whitespaceChunk = whitespaceChunk' <?> "whitespace"
  where
    whitespaceChunk' =
       void (takeWhile1 \c -> c == ' ' || c == '\t' || c == '\n')
       <|> void (text "\r\n")
