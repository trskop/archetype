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

import Control.Applicative (many, optional)
import Data.Foldable (asum, for_)
import Data.Functor ((<&>))
import Data.Monoid (mconcat)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStr, hPutStrLn, stderr)

import Data.Monoid.Endo (Endo(Endo, appEndo))
--import Data.Monoid.Endo.Fold (dualFoldEndo)
import Data.Output.Colour (noColorEnvVar)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (getContents, putStrLn, readFile)
import qualified Dhall (Interpret, auto, input, inputFile)
import Mainplate (applySimpleDefaults, runAppWith)
import qualified Options.Applicative as Options
    ( CommandFields
    , Mod
    , Parser
    , ParserInfo
    , command
    , defaultPrefs
    , flag'
    , info
    , long
    , metavar
    , short
    , strArgument
    , strOption
    , subparser
    )
import qualified Options.Applicative.Standard as Options (version)
import System.Environment.Parser (ParseEnvError(..), parseEnvIO, optionalVar)
import System.Process
    ( CreateProcess(env)
    , proc
    , readCreateProcessWithExitCode
    )

import Language.Archetype.Core as Archetype (untag)
import Language.Archetype.FrontEnd as Archetype (parser)
import Language.Archetype.BackEnd
    ( File(File, content, filename)
    , Response(Error, Result)
    )

import Main.Config
    ( Backend(Backend, arguments, command, environment)
    , Config(colourOutput)
    , Params
    , applyConfig
    , defParams
    , emptyConfig
    )
import qualified Main.Options as Options (parse)

import Paths_archetype () -- TODO: Take version information from here.


data Action
    = Help
    | Version
    | GenerateCode Backend (Maybe FilePath)
    | Lint (Maybe FilePath)
    | Diff FilePath FilePath
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
main = do
    mode@Mode{params = params'} <- readGlobalConfig
    runAppWith (parseOptions params') readConfig (applySimpleDefaults mode)
        \Mode{..} -> case action of
            Help -> printHelp params
            Version -> printVersion params
            GenerateCode backend input -> generateCode params backend input
            Lint _input -> pure ()  -- TODO: Implement
            Diff _input1 _input2 -> pure () -- TODO: Implement

parseOptions :: Params -> IO (Endo (Mode Params))
parseOptions params = do
    name <- getProgName
    getArgs >>= Options.parse name params Options.defaultPrefs parserInfo
  where
    parserInfo :: Options.ParserInfo (Endo (Mode Params))
    parserInfo = Options.info options mempty

options :: Options.Parser (Endo (Mode Params))
options = asum
    [ helpOption
    , versionOption
    , Options.subparser $ mconcat
        [ subcommand "codegen" codegenOptions
        , subcommand "lint"    lintOptions
        , subcommand "diff"    diffOptions
        , subcommand "help"    (pure switchToHelpMode)
        , subcommand "version" (pure switchToVersionMode)
        ]
    ]
  where
    subcommand
        :: String
        -> Options.Parser (Endo (Mode Params))
        -> Options.Mod Options.CommandFields (Endo (Mode Params))
    subcommand n p = Options.command n (Options.info p mempty)

    helpOption :: Options.Parser (Endo (Mode Params))
    helpOption = Options.flag' switchToHelpMode $ mconcat
        [ Options.short 'h'
        , Options.long "help"
        ]

    versionOption :: Options.Parser (Endo (Mode Params))
    versionOption = Options.flag' switchToVersionMode (Options.version True)

    codegenOptions :: Options.Parser (Endo (Mode Params))
    codegenOptions =
        switchToCodeGenMode
            <$> backendOption
            <*> many backendArgumentOption
            <*> optional inputFileArgument
      where
        backendOption = Options.strOption $ mconcat
            [ Options.long "backend"
            , Options.short 'b'
            , Options.metavar "EXECUTABLE"
            ]

        backendArgumentOption = Options.strOption
            (Options.long "backend-argument" <> Options.metavar "ARGUMENT")

    lintOptions :: Options.Parser (Endo (Mode Params))
    lintOptions = switchToLintMode <$> optional inputFileArgument

    diffOptions :: Options.Parser (Endo (Mode Params))
    diffOptions = switchToDiffMode <$> inputFileArgument <*> inputFileArgument

    inputFileArgument :: Options.Parser FilePath
    inputFileArgument = Options.strArgument (Options.metavar "FILE")

    switchToHelpMode :: Endo (Mode Params)
    switchToHelpMode = Endo \m -> m{action = Help}

    switchToVersionMode :: Endo (Mode Params)
    switchToVersionMode = Endo \m -> m{action = Version}

    switchToCodeGenMode
        :: FilePath
        -> [String]
        -> Maybe FilePath
        -> Endo (Mode Params)
    switchToCodeGenMode command arguments input = Endo \mode -> mode
        { action = GenerateCode
            Backend
                { arguments
                , command
                , environment = []
                }
            input
        }

    switchToLintMode :: Maybe FilePath -> Endo (Mode Params)
    switchToLintMode input = Endo \m -> m{action = Lint input}

    switchToDiffMode :: FilePath -> FilePath -> Endo (Mode Params)
    switchToDiffMode file1 file2 = Endo \m -> m{action = Diff file1 file2}

readConfig :: Mode a -> IO (Either String (Endo Params))
readConfig Mode{configFile} =
    Right . applyConfig <$> maybe (pure emptyConfig) readConfigFile configFile
  where
    readConfigFile = Dhall.inputFile Dhall.auto

readGlobalConfig :: IO (Mode Params)
readGlobalConfig = do
    (configFile, g) <- parseEnvIO () parseEnvError do
        (,) <$> optionalVar' "ARK_CONFIG"
            <*> noColorEnvVar'

    pure $ g <$> Mode
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

    optionalVar' n = fmap Text.unpack <$> optionalVar n

    noColorEnvVar' = noColorEnvVar <&> \colourOutput ->
        appEndo $ applyConfig emptyConfig{colourOutput}

printHelp :: Params -> IO ()
printHelp _ = putStr $ unlines
    [ "Archetype IDL compiler toolkit."
    , ""
    , "Usage: "
    , ""
    , "  ark codegen --backend=EXECUTABLE [--backend-argument=ARGUMENT ...]\
        \ [FILE]"
    , "  ark lint [FILE]"
    , "  ark diff FILE FILE"
    , "  ark help"
    , "  ark version"
    , "  ark {--help|-h}"
    , "  ark {--version|-V}"
    , ""
    , "Commands:"
    , ""
    , "  codegen  Generate code from Archetype IDL using provided code\
        \ generator."
    , "  lint     Suggests improvements to Archetype IDL files."
    , "  diff     Shows difference between two Archetype IDLs."
    , "  help     Displays this help message."
    , "  version  Displays detailed version information."
    ]

printVersion :: Params -> IO ()
printVersion _ = putStr $ unlines
    [ "Archetype language version:       0.1.0"
    , "Archetype compiler (ark) version: 0.1.0.0" -- TODO: Get from Paths_archetype
    , "Haskell Dhall library version:    1.23.0" -- TODO: Get via CPP
    , "Dhall standard version:           7.0.0"
    ]

generateCode :: Params -> Backend -> Maybe FilePath -> IO ()
generateCode params Backend{..} inputFile = do
    print params

    input <- maybe Text.getContents Text.readFile inputFile
    expression <- case Archetype.parser "(stdin)" input of
        Left e -> do
            hPutStrLn stderr (show e)
            exitWith (ExitFailure 1)

        Right expr ->
            pure (Archetype.untag @_ @_ @() expr)

    print expression

    let ir = show expression -- TODO: Proper intermediate representation
    (exitCode, output, err) <- readCreateProcessWithExitCode process ir
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
