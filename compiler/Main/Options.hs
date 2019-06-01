-- |
-- Module:      Main.Options
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main.Options
    ( parse
    )
  where

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Monoid (Endo)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (Handle, hIsTerminalDevice, hPutStr, hPutStrLn, stderr)

--import Data.Text (Text)
--import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import Data.Verbosity (Verbosity(Silent))
import Data.Output.Colour
    ( ColourOutput(..)
    , terminalSupportsColours
    , useColoursWhen
    )
import qualified Options.Applicative as Options
    ( ParserFailure(ParserFailure, execFailure)
    , ParserInfo
    , ParserPrefs
    , ParserResult(CompletionInvoked, Failure, Success)
    , parserFailure
    )
import qualified Options.Applicative.Common as Options (runParserInfo)
import qualified Options.Applicative.Help as Options
    ( Chunk(..)
    , ParserHelp(ParserHelp, helpError)
    , colon
    , displayS
    , hsep
    , renderPretty
    , text
    )
import qualified Options.Applicative.Internal as Options (runP)
import qualified System.Console.ANSI as Terminal
    ( Color(Red)
    , ColorIntensity(Vivid)
    , ConsoleLayer(Foreground)
    , SGR(Reset, SetColor)
    , setSGRCode
    )
import qualified System.Console.Terminal.Size as Terminal
    ( Window(width)
    , hSize
    )
import System.Console.Terminfo (setupTermFromEnv)

import Main.Config (Params(..))


parse
    :: String
    -> Params
    -> Options.ParserPrefs
    -> Options.ParserInfo (Endo (mode config))
    -> [String]
    -- ^ Command line arguments.  Usually obtained by 'getArgs'.
    -> IO (Endo (mode config))
parse name params parserPrefs parserInfo =
    handleParseResult' params . execParserPure'
  where
    execParserPure' = execParserPure parserPrefs parserInfo

    handleParseResult' Params{colourOutput, verbosity} =
        handleParseResult name verbosity colourOutput

-- | Variant of 'Options.Applicative.execParserPure' that doesn't provide shell
-- completion.
execParserPure
    :: Options.ParserPrefs
    -- ^ Global preferences for this parser
    -> Options.ParserInfo a
    -- ^ Description of the program to run
    -> [String]
    -- ^ Program arguments
    -> Options.ParserResult a
execParserPure pprefs pinfo args =
  case Options.runP (Options.runParserInfo pinfo args) pprefs of
    (Right r, _) ->
        Options.Success r

    (Left err, ctx) ->
        Options.Failure (Options.parserFailure pprefs pinfo err ctx)

handleParseResult
    :: String
    -> Verbosity
    -> ColourOutput
    -> Options.ParserResult a
    -> IO a
handleParseResult command verbosity colour = \case
    Options.Success a ->
        pure a

    Options.Failure Options.ParserFailure{Options.execFailure} ->
        let (err, _, _) = execFailure command
        in dieFailedToParseOptions command verbosity colour stderr err

    Options.CompletionInvoked _ ->
        exitWith (ExitFailure 1) -- TODO: This is imposible case.

dieFailedToParseOptions
    :: String
    -> Verbosity
    -> ColourOutput
    -> Handle
    -> Options.ParserHelp
    -> IO a
dieFailedToParseOptions name verbosity colour h
  Options.ParserHelp{Options.helpError} = do
    when (verbosity > Silent) $ do
        useColours <- shouldUseColours h colour
        withColour useColours vividRed $ \h' -> do
            width <- maybe 80 Terminal.width <$> Terminal.hSize h'
            hPutStrLn h' (renderError width)

    exitWith (ExitFailure 1)
  where
    renderError width =
        (`Options.displayS` "") . Options.renderPretty 1.0 width $ Options.hsep
            [ Options.text name <> Options.colon
            , "Error:"
            , fromChunk helpError <> Options.colon
            , Options.text "See"
            , Options.text ("'" <> name <> " --help'")
            , Options.text "for more details."
            ]

    fromChunk (Options.Chunk possiblyDoc) =
        fromMaybe "Parsing command line arguments failed." possiblyDoc

    withColour :: Bool -> Terminal.SGR -> (Handle -> IO ()) -> IO ()
    withColour useColours colourSgr action =
        if useColours
            then hSetSgrCode h colourSgr `bracket_` hReset h $ action h
            else action h

    vividRed = Terminal.SetColor
        Terminal.Foreground
        Terminal.Vivid
        Terminal.Red

hSetSgrCode :: Handle -> Terminal.SGR -> IO ()
hSetSgrCode h code = hPutStr h $ Terminal.setSGRCode [code]

hReset :: Handle -> IO ()
hReset h = hSetSgrCode h Terminal.Reset

-- | Check if we should use colours for the specified output handle.  This
-- function doesn't look for @NO_COLOR@ environment variable.  That has to be
-- done manually before calling this function.  It would be impossible to
-- override @NO_COLOR@ by options\/configuration otherwise.
--
-- See also 'noColorEnvVar' for more details.
--
-- Simple usage example:
--
-- @
-- printMessage :: Config -> 'Handle' -> Doc Style -> IO ()
-- printMessage Config{colourOutput} handle msg = do
--     useColours <- 'shouldUseColours' handle colourOutput
--     if useColours
--         then printColourisedMessage handle msg
--         else printPlainMessage handle msg
-- @
shouldUseColours
    :: Handle
    -- ^ A handle to which we want to print colourised output.  In case of
    -- 'Auto' we are checking if this is connected to a terminal or not, and
    -- if it is then we are checking that the terminal supports colours.
    -> ColourOutput
    -- ^ User preferences.  Usually combination of configuration, environment
    -- ('noColorEnvVar'), and command line options that culminate in this value.
    -> IO Bool
    -- ^ Return values:
    --
    -- * 'False' - Don't use colours when printing to specified 'Handle'.
    -- * 'True' - You are free to use colours when printing to specified
    --   'Handle'.
shouldUseColours handle = useColoursWhen $ do
    otputIsTerminal <- hIsTerminalDevice handle
    if otputIsTerminal
        then terminalSupportsColours <$> setupTermFromEnv
        else pure False
