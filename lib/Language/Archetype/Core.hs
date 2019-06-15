-- |
-- Module:      Language.Archetype.Core
-- Description: Archetype AST.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Archetype AST.
module Language.Archetype.Core
    (
      Expression(..)
    , Binding(..)
    , untag

    -- * Embedding in Dhall
    --
    -- | Archetype can be considered as
    , DhallEmbeddingError(..)
    , embedIntoDhall
    , embedIntoDhall'
    , embedPrimType
    , embedType
    )
  where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Data (Data)
import Data.Foldable (fold, for_)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Data.Text (Text)
import qualified Dhall.Core as Dhall
import qualified Dhall.Map as Dhall (Map)
import qualified Dhall.Map (fromList, singleton)


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

data DhallEmbeddingError s a
    = UnexpectedImportStatement (Expression s a)
    | DhallEmbeddingError Text (Dhall.Expr s a)
  deriving Show

embedIntoDhall
    :: Expression s a
    -> Either (DhallEmbeddingError s a) (Dhall.Expr s a)
embedIntoDhall = embedIntoDhall

embedIntoDhall'
    :: Expression s a
    -> Either (DhallEmbeddingError s a)
        ( Dhall.Map Text (Dhall.Expr s a)
        , Dhall.Map Text (Dhall.Expr s a)
        )
embedIntoDhall' = \case
    (Tag loc (TypeDeclaration binding)) -> do
        (name, ctx, r) <- embedType (Just loc) binding
        pure (Dhall.Map.singleton name ctx, Dhall.Map.singleton name r)

    TypeDeclaration binding -> do
        (name, ctx, r) <- embedType Nothing binding
        pure (Dhall.Map.singleton name ctx, Dhall.Map.singleton name r)

    (Tag loc (PrimitiveTypeDeclaration binding)) -> do
        (name, ctx, r) <- embedPrimType (Just loc) binding
        pure (Dhall.Map.singleton name ctx, Dhall.Map.singleton name r)

    PrimitiveTypeDeclaration binding -> do
        (name, ctx, r) <- embedPrimType Nothing binding
        pure (Dhall.Map.singleton name ctx, Dhall.Map.singleton name r)

    expr@(Tag _ (Import _)) ->
        Left (UnexpectedImportStatement expr)

    expr@(Import _) ->
        Left (UnexpectedImportStatement expr)

    Tag _ (Expression exprs) ->
        fold <$> mapM embedIntoDhall' exprs

    Expression exprs ->
        fold <$> mapM embedIntoDhall' exprs

    Tag _ expr@(Tag _ _) ->
        embedIntoDhall' expr

-- | Embed primitive type declaration into Dhall.
embedPrimType
    :: Maybe s
    -> Binding (Dhall.Expr s a) (Maybe (Dhall.Expr s a))
    -> Either (DhallEmbeddingError s a) (Text, Dhall.Expr s a, Dhall.Expr s a)
    -- ^ Success result consists of primitive type name, it's value
    -- representation, and type annotation.
embedPrimType loc Binding{name, typeAnnotation, value, comment = _} = do
    for_ value \v ->
        Left $ DhallEmbeddingError
            "Prim type definition bodies are unsupported at the moment."
            (maybe id Dhall.Note loc v)

    -- TODO: We need to type check and normalise typeAnnotation before passing
    -- it to `typeToFunction`.
    context <- typeToFunction typeAnnotation

    pure (name, context, typeAnnotation)
  where
    foldPi
        :: Text
        -> [Text]
        -> Text
        -> Dhall.Expr s a
        -> Dhall.Expr s a
        -> Either (DhallEmbeddingError s a) (Dhall.Map Text (Dhall.Expr s a))
    foldPi typeName vars n ann rest = do
        case ann of
            e@(Dhall.Note _ (Dhall.Const c)) -> case c of
                Dhall.Type -> pure ()
                _          -> unsupported e

            e@(Dhall.Const c) -> case c of
                Dhall.Type -> pure ()
                _          -> unsupported e

            e -> unsupported e

        case rest of
            Dhall.Note _ (Dhall.Pi n' e1 e2) ->
                foldPi typeName (vars <> [n]) n' e1 e2

            Dhall.Pi n' e1 e2 ->
                foldPi typeName (vars <> [n]) n' e1 e2

            Dhall.Note _ e ->
                foldApp typeName vars [] e

            e ->
                foldApp typeName vars [] e

    foldApp
        :: Text
        -> [Text]
        -> [Text]
        -> Dhall.Expr s a
        -> Either (DhallEmbeddingError s a) (Dhall.Map Text (Dhall.Expr s a))
    foldApp typeName piVars appVars = \case
        Dhall.App e1 e2 -> do
            n <- case e1 of
                    Dhall.Var (Dhall.V n _) -> pure n
                    Dhall.Note _ (Dhall.Var (Dhall.V n _)) -> pure n
                    _ -> unsupported e1

            case e2 of
                e@(Dhall.Var (Dhall.V n' _))
                  | sort piVars /= sort (appVars <> [n']) -> unsupported e
                  | otherwise ->
                      pure . Dhall.Map.fromList $ (, Dhall.Const Dhall.Type) <$> piVars

                e@(Dhall.Note _ (Dhall.Var (Dhall.V n' _)))
                  | sort piVars /= sort (appVars <> [n']) -> unsupported e
                  | otherwise ->
                      pure . Dhall.Map.fromList $ (, Dhall.Const Dhall.Type) <$> piVars

                _ -> foldApp typeName piVars (appVars <> [n]) e2

        e ->
            unsupported e

    typeToFunction
        :: Dhall.Expr s a
        -> Either (DhallEmbeddingError s a) (Dhall.Expr s a)
    typeToFunction = \case
        Dhall.Note _ b -> typeToFunction b

        Dhall.Const Dhall.Type -> Right (Dhall.Union (Dhall.Map.singleton name Nothing))
        e@Dhall.Const{}        -> unsupported e

        e@(Dhall.Pi "_" _ _) -> unsupported e
        Dhall.Pi a b c       ->
            Dhall.Union . Dhall.Map.singleton name . Just . Dhall.Record
                <$> foldPi name [] a b c

        e@Dhall.Var{}            -> unsupported e
        e@Dhall.Lam{}            -> unsupported e
        e@Dhall.App{}            -> unsupported e
        e@Dhall.Let{}            -> unsupported e
        e@Dhall.Annot{}          -> unsupported e
        e@Dhall.Bool             -> unsupported e
        e@Dhall.BoolLit{}        -> unsupported e
        e@Dhall.BoolAnd{}        -> unsupported e
        e@Dhall.BoolOr{}         -> unsupported e
        e@Dhall.BoolEQ{}         -> unsupported e
        e@Dhall.BoolNE{}         -> unsupported e
        e@Dhall.BoolIf{}         -> unsupported e
        e@Dhall.Natural          -> unsupported e
        e@Dhall.NaturalLit{}     -> unsupported e
        e@Dhall.NaturalFold      -> unsupported e
        e@Dhall.NaturalBuild     -> unsupported e
        e@Dhall.NaturalIsZero    -> unsupported e
        e@Dhall.NaturalEven      -> unsupported e
        e@Dhall.NaturalOdd       -> unsupported e
        e@Dhall.NaturalToInteger -> unsupported e
        e@Dhall.NaturalShow      -> unsupported e
        e@Dhall.NaturalPlus{}    -> unsupported e
        e@Dhall.NaturalTimes{}   -> unsupported e
        e@Dhall.Integer          -> unsupported e
        e@Dhall.IntegerLit{}     -> unsupported e
        e@Dhall.IntegerShow      -> unsupported e
        e@Dhall.IntegerToDouble  -> unsupported e
        e@Dhall.Double           -> unsupported e
        e@Dhall.DoubleLit{}      -> unsupported e
        e@Dhall.DoubleShow       -> unsupported e
        e@Dhall.Text             -> unsupported e
        e@Dhall.TextLit{}        -> unsupported e
        e@Dhall.TextAppend{}     -> unsupported e
        e@Dhall.TextShow         -> unsupported e
        e@Dhall.List             -> unsupported e
        e@Dhall.ListLit{}        -> unsupported e
        e@Dhall.ListAppend{}     -> unsupported e
        e@Dhall.ListBuild        -> unsupported e
        e@Dhall.ListFold         -> unsupported e
        e@Dhall.ListLength       -> unsupported e
        e@Dhall.ListHead         -> unsupported e
        e@Dhall.ListLast         -> unsupported e
        e@Dhall.ListIndexed      -> unsupported e
        e@Dhall.ListReverse      -> unsupported e
        e@Dhall.Optional         -> unsupported e
        e@Dhall.OptionalLit{}    -> unsupported e
        e@Dhall.Some{}           -> unsupported e
        e@Dhall.None             -> unsupported e
        e@Dhall.OptionalFold     -> unsupported e
        e@Dhall.OptionalBuild    -> unsupported e
        e@Dhall.Record{}         -> unsupported e
        e@Dhall.RecordLit{}      -> unsupported e
        e@Dhall.Union{}          -> unsupported e
        e@Dhall.UnionLit{}       -> unsupported e
        e@Dhall.Combine{}        -> unsupported e
        e@Dhall.CombineTypes{}   -> unsupported e
        e@Dhall.Prefer{}         -> unsupported e
        e@Dhall.Merge{}          -> unsupported e
        e@Dhall.Field{}          -> unsupported e
        e@Dhall.Project{}        -> unsupported e
        e@Dhall.ImportAlt{}      -> unsupported e
        e@Dhall.Embed{}          -> unsupported e

    -- TODO: Source code location should be part of the error.
    unsupported e =
        Left (DhallEmbeddingError "Unsupported in prim type annotation." e)

-- | Embed type definition into Dhall.
embedType
    :: Maybe s
    -> Binding (Maybe (Dhall.Expr s a)) (Dhall.Expr s a)
    -> Either (DhallEmbeddingError s a) (Text, Dhall.Expr s a, Dhall.Expr s a)
embedType = embedType  -- TODO: Implement
