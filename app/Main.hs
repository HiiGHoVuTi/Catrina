{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Functor
import Data.Text hiding (unlines)
import Debug.Pretty.Simple
import Interpreter.Program
import Options.Applicative
import Syntax.Program
import System.IO
import Text.Parsec

{-
main :: IO ()
main = openFile "samples" ReadMode
   >>= hGetContents
   <&> parse program "" . pack
   <&> pTraceShowId
   <&> fmap interpretProgram 
   >>= pTraceShowM
-}

-- NOTE(Maxime): newtype is only here because linter is mad at me

newtype Options = Options
  { optCommand :: Command
  }

newtype Command
  = InterpretCommand
    { filename :: String
    }


doTheThing :: Options -> IO ()
-- FIXME(Maxime)
doTheThing Options {..} = 
   openFile (filename optCommand) ReadMode
   >>= hGetContents
   <&> parse program "" . pack
   <&> fmap interpretProgram 
   >>= pTraceShowM


main :: IO ()
main = execParser opts >>= doTheThing
  where 
    -- opts = flip info (progDesc "The essential Cat-Rina compilers and toolkits") $ 
    opts = info (optsParser <**> helper)
         $ fullDesc 
        <> progDesc (unlines
            [ 
            ])
        <> header "Rina -- the essential catrina compilers and toolkits"

    optsParser =
      Options <$> 
        subparser 
           (command "interpret" (info interpretCommand (progDesc "interprets a file")))

    interpretCommand = InterpretCommand <$> strArgument
      (  metavar "INPUT"
      <> help "The file to interpret"
      )

-- evaluateExpr :: String -> Either ParseError Value
-- evaluateExpr = fmap (flip (evalExpr []) VUnit) . parse expr "" . pack
