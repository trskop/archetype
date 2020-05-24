{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Either
import Test.Hspec.Expectations.Pretty
import Test.Hspec (hspec, describe, it, specify, before, xit, context)
import qualified Data.Text.IO as Tx

import           Text.Megaparsec.Pos (SourcePos(..), mkPos)
import           Dhall.Src
import           Language.Archetype.Core
import qualified Dhall.Core as Dhall
import           Language.Archetype.FrontEnd (parser)

main :: IO ()
main = hspec do
  describe "Golden Tests" $
    before (Tx.readFile "example.archetype") $
      xit "can parse the example file" $ \txt ->
        parser "Example File" txt `shouldSatisfy` isRight

  describe "Unit Tests" do
    context "Example file" do
      it "parses section 1" $
        parser mempty "-- Non-empty list\nprim type NonEmpty : ∀(a : Type) → NonEmpty a"
          `shouldSatisfy` isRight

      it "parses section 2" $
        parser mempty "-- [Universally unique identifier](https://en.wikipedia.org/wiki/Universally_unique_identifier)\nprim type UUID : Type"
          `shouldSatisfy` isRight

      specify "REST and RPC calls are modeled as primitive types as well." $
        parser mempty "prim type RpcCall\n: ∀(request : Type)\n→ ∀(response : Type)\n→ RpcCall request response"
          `shouldSatisfy` isRight

      it "parses section 2+4" $
        parser mempty "prim type UUID : Type\ntype UserId = UUID"
          `shouldSatisfy` isRight

      specify "Every user account can be identified by a unique UUID." $
        parser mempty "type UserId = UUID"
          `shouldSatisfy` isRight

      specify "Enums are supported as a special case of sum types." $
        parser mempty "type UserRole = < Admin | NormalUser >"
          `shouldSatisfy` isRight

      specify "Enums are supported as a special case of sum types." $
        parser mempty "type UserRole = < Admin | NormalUser >"
          `shouldSatisfy` isRight

      specify "A user, what a surprise." $
        parser mempty "type User =\n { id : UserId\n , full-name : Text\n , emails : NonEmpty Text\n , verified : Bool\n , role : UserRole\n }"
          `shouldSatisfy` isRight

      it "parses section 7" $
        parser mempty "type GetUserError = < NoSuchUser | AccountDisabled | OtherError : Text >"
          `shouldSatisfy` isRight

      it "parses section 8" $
        parser mempty "type Response = λ(error : Type) → λ(r : Type) → < Error : error | Response : r >"
          `shouldSatisfy` isRight

      it "parses section 9" $
        parser mempty "type rpc-get-user = RpcCall UserId (Response GetUserError User)"
          `shouldSatisfy` isRight

    context "tests parsing result 'isRight'" do
      it "can parse 'prim type's with comment" $
        parser mempty "-- test\ntype Melee : Type = < Barbarian | Fighter | Paladin | Monk > -- test end\n"
          `shouldSatisfy` isRight

      it "can parse 'prim type's with comment" $
        parser mempty "-- test\nprim type UUID : Text -- test end\n"
          `shouldSatisfy` isRight
      it "can parse record types with comment" $
        parser mempty "-- test\ntype Character : Type = { name : Text, str : Natural, dex : Natural, inspiration : Bool } -- test end\n"
          `shouldSatisfy` isRight


    context "tests with explicit result types" do
      it "can parse 'type's" $
        rights [parser mempty "type Mage = < Wizard | Sorcerer | Bard | Cleric | Warlock >"]
          `shouldBe`
            [ Expression
              [Tag (Src { srcStart = SourcePos { sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                        , srcEnd   = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 60}
                        , srcText = "type Mage = < Wizard | Sorcerer | Bard | Cleric | Warlock >"})
                  (TypeDeclaration
                    (Binding { name = "Mage"
                              , typeAnnotation = Nothing
                              , value = Dhall.Note
                                (Src { srcStart = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 13}
                                    , srcEnd = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 60}
                                    , srcText = "< Wizard | Sorcerer | Bard | Cleric | Warlock >"})
                                (Dhall.Union [("Wizard",Nothing) ,("Sorcerer",Nothing),("Bard",Nothing),("Cleric",Nothing),("Warlock",Nothing)])
                              , comment = ""}))
              ]
            ]

      it "can parse 'prim type's" $
        rights [parser mempty "prim type UUID : Text"]
          `shouldBe`
            [ Expression
              [ Tag (Src { srcStart = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                        , srcEnd = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 22}
                        , srcText = "prim type UUID : Text"})
                    (PrimitiveTypeDeclaration
                      (Binding { name = "UUID"
                              , typeAnnotation = Dhall.Note
                                (Src { srcStart = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 18}
                                      , srcEnd = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 22}
                                      , srcText = "Text"}) Dhall.Text
                              , value = Nothing
                              , comment = ""}))
              ]
            ]
