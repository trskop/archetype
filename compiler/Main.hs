-- |
-- Module:      Main
-- Description: Archetype IDL compiler.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Archetype IDL compiler.
module Main (main)
  where

import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStr, hPutStrLn, stderr)

import Data.Monoid.Endo (Endo(Endo))
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (putStrLn)
import qualified Dhall (Interpret, auto, input, inputFile)
import Mainplate (runAppWith)
import System.Environment.Parser (ParseEnvError(..), parseEnvIO, optionalVar)
import System.Process
    ( CreateProcess(env)
    , proc
    , readCreateProcessWithExitCode
    )

import Language.Archetype.FrontEnd as Archetype (parser)
import Language.Archetype.BackEnd
    ( File(File, content, filename)
    , Response(Error, Result)
    )

import Main.Config
    ( Backend(Backend, arguments, command, environment)
    , Params
    , applyConfig
    , defParams
    , emptyConfig
    )


-- TODO:
--
-- ark codegen --backend=EXECUTABLE [--backend-argument=ARGUMENT ...] [FILE ...]
-- ark lint [FILE ...]
-- ark diff FILE FILE

data Action
    = Help
    | Version
    | GenerateCode Backend
--  | Lint
--  | Diff
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

data Mode a = Mode
    { action :: Action
    , configFile :: Maybe FilePath
    , params :: a
    }
  deriving stock (Functor, Generic, Show)
  deriving anyclass (Dhall.Interpret)

main :: IO ()
main = runAppWith parseOptions readConfig readAndApplyGlobalConfig \Mode{..} ->
    case action of
        Help -> printHelp params
        Version -> printVersion params
        GenerateCode backend -> generateCode backend params

-- TODO: Proper options parsing.
parseOptions :: IO (Endo (Mode Params))
parseOptions = getArgs <&> \case
    [] ->
        mempty

    command : arguments ->
        Endo \mode -> mode
            { action = GenerateCode Backend
                { arguments
                , command
                , environment = []
                }
            }

readConfig :: Mode a -> IO (Either String (Endo Params))
readConfig Mode{configFile} =
    Right . applyConfig <$> maybe (pure emptyConfig) readConfigFile configFile
  where
    readConfigFile = Dhall.inputFile Dhall.auto

readAndApplyGlobalConfig :: Endo (Mode Params) -> IO (Mode Params)
readAndApplyGlobalConfig (Endo f) = do
    configFile <- parseEnvIO () parseEnvError do
        fmap Text.unpack <$> optionalVar "ARK_CONFIG"

    pure $ f Mode
        { action = Help
        , configFile
        , params = defParams
        }
  where
    parseEnvError err = do
        hPutStrLn stderr $ "Error: " <> case err of
            ParseEnvError n msg ->
                show n <> ": Failed to parse environment variable: " <> msg

            MissingEnvVarError n ->
                show n <> ": Missing required environment variable."

            ErrorMessage msg ->
                msg

            UnknownError ->
                "Unable to parse environment variables."

        exitWith (ExitFailure 1)

printHelp :: Params -> IO ()
printHelp _ = putStr $ unlines
    [ "Archetype IDL compiler toolkit."
    , ""
    , "Usage: "
    , ""
    , "  ark [EXECUTABLE [ARGUMENTS]]"
    ]

printVersion :: Params -> IO ()
printVersion _ = pure ()

generateCode :: Backend -> Params -> IO ()
generateCode Backend{..} params = do
    print params

    input <- getContents

    expression <- case Archetype.parser "(stdin)" (fromString input) of
        Left e -> do
            hPutStrLn stderr (show e)
            exitWith (ExitFailure 1)

        Right expr ->
            pure expr

    print expression

    (exitCode, output, err) <- readCreateProcessWithExitCode process input
    if exitCode == ExitSuccess
        then
            Dhall.input Dhall.auto (fromString output) >>= \case
                Error e -> do
                    hPutStrLn stderr ("Error: Code generator: " <> show e)
                    exitWith (ExitFailure 1)

                Result files -> for_ files \File{filename, content} -> do
                    putStrLn ("--- Begin " <> filename <> " ---")
                    Text.putStrLn content
                    putStrLn ("--- End " <> filename <> " ---")
        else do
            hPutStrLn stderr
                ( "Error: Code generator terminated unexpectedly with "
                <> show exitCode
                <> ":"
                )
            hPutStr stderr err
            exitWith (ExitFailure 1)
  where
    process = (proc command arguments)
        { env = Nothing -- TODO
        }
