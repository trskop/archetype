{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Archetype.Parser.Dhall.Combinators
    ( laxSrcEq
    , count
    , range
    , option
    , star
    , plus
    , satisfy
    , takeWhile
    , takeWhile1
    , noDuplicates
    , toMap
    , Src(..)
    , Parser(..)
    )
  where

import           Control.Applicative        (Alternative (..))
import           Data.Semigroup             (Semigroup (..))
import           Data.Sequence              (ViewL (..))
import           Data.Text                  (Text)
import           Dhall.Map                  (Map)
import           Dhall.Set                  (Set)
import           Prelude                    hiding (const, pi, takeWhile)

import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Text.Megaparsec
#if !MIN_VERSION_megaparsec(7, 0, 0)
import qualified Text.Megaparsec.Char as Text.Megaparsec (satisfy)
#endif
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators

import Dhall.Parser (Parser(..), Src(..))

-- | Doesn't force the 'Text' part
laxSrcEq :: Src -> Src -> Bool
laxSrcEq (Src p q _) (Src p' q' _) = eq p  p' && eq q q'
  where
    -- Don't compare filename (which is FilePath = String)
    eq  :: Text.Megaparsec.SourcePos -> Text.Megaparsec.SourcePos -> Bool
    eq (Text.Megaparsec.SourcePos _ a b) (Text.Megaparsec.SourcePos _ a' b') =
        a == a' && b == b'
{-# INLINE laxSrcEq #-}

count :: (Semigroup a, Monoid a) => Int -> Parser a -> Parser a
count n parser = mconcat (replicate n parser)

range :: (Semigroup a, Monoid a) => Int -> Int -> Parser a -> Parser a
range minimumBound maximumMatches parser =
    count minimumBound parser <> loop maximumMatches
  where
    loop 0 = mempty
    loop n = (parser <> loop (n - 1)) <|> mempty

option :: (Alternative f, Monoid a) => f a -> f a
option p = p <|> pure mempty

star :: (Alternative f, Monoid a) => f a -> f a
star p = plus p <|> pure mempty

plus :: (Alternative f, Monoid a) => f a -> f a
plus p = mappend <$> p <*> star p

satisfy :: (Char -> Bool) -> Parser Text
satisfy = fmap Data.Text.singleton . Text.Parser.Char.satisfy

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile predicate = Parser (Text.Megaparsec.takeWhileP Nothing predicate)

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 predicate = Parser (Text.Megaparsec.takeWhile1P Nothing predicate)

noDuplicates :: Ord a => [a] -> Parser (Set a)
noDuplicates = go Dhall.Set.empty
  where
    go found    []  = return found
    go found (x:xs) =
        if Data.Set.member x (Dhall.Set.toSet found)
        then fail "Duplicate key"
        else go (Dhall.Set.append x found) xs

toMap :: [(Text, a)] -> Parser (Map Text a)
toMap kvs = do
    let adapt (k, v) = (k, pure v)
    let m = fromListWith (<|>) (fmap adapt kvs)
    let action k vs = case Data.Sequence.viewl vs of
            EmptyL  -> empty
            v :< vs' ->
                if null vs'
                then pure v
                else
                    Text.Parser.Combinators.unexpected
                        ("duplicate field: " ++ Data.Text.unpack k)
    Dhall.Map.traverseWithKey action m
  where
    fromListWith combine = foldr cons mempty
      where
        cons (k, v) = Dhall.Map.insertWith combine k v
