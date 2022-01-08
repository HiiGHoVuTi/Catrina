{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where

import Control.Monad
import Data.Functor
import Data.Map (empty)
import Data.Text hiding (unlines, empty, foldl, head)
import Interpreter
import Options.Applicative hiding (empty)
import Syntax
import System.IO
import Text.Parsec
import Text.Pretty.Simple

-- NOTE(Maxime): newtype is only here because linter is mad at me

newtype Options = Options
  { optCommand :: Command
  }

data Command
  = InterpretCommand
    { interpretedFilename :: String
    }
  | ReplCommand
    { replFilename :: Maybe String
    }

doTheThing :: Options -> IO ()

-- FIXME(Maxime)
doTheThing Options {optCommand = InterpretCommand{..}} = do {-
   openFile interpretedFilename ReadMode
   >>= hGetContents
   <&> parse program "" . pack
   <&> fmap interpretProgram 
   <&> fmap pShowValue
   >>= fmap putStrLn
   -}
   fileContents <- openFile interpretedFilename ReadMode >>= hGetContents
   let 
    parsed = parse program "main" $ pack fileContents
    in case parsed of
         Left  err -> pPrint err
         Right pog -> putStrLn . pShowValue 
                   . interpretProgram 
                   $ pog

doTheThing Options {optCommand = ReplCommand{..}} =  do
  let
    load maybeName = do
      fileName <- maybeName
      let 
        fileContents = openFile fileName ReadMode >>= hGetContents
        program'     = parse program fileName . pack <$> fileContents
        in Just $ do
          program'' <- program'
          pure $ foldl interpretDecl start . programDeclarations <$> program''

    repl env = do
      input <- putStr "Rina> " >> hFlush stdout >> getLine
      when (input /= ":q") $ do
        let parsed = parse expr "repl" $ pack input
            res    = flip (evalExpr env) VUnit <$> parsed
        case res of
          Left  err -> pPrint err >> repl env
          Right out -> putStrLn (pShowValue out) >> repl env

      when (input == ":q") $ putStrLn "Thanks for using Rina ❤️"
    in case load replFilename of
         Nothing -> repl empty
         Just x  -> x >>= \case
                      Left  err -> pPrint err
                      Right env -> repl env

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
           (  command "interpret" (info interpretCommand (progDesc "interprets a file"))
           <> command "repl"      (info replCommand      (progDesc "opens a repl")))

    interpretCommand = InterpretCommand <$> strArgument
      (  metavar "INPUT"
      <> help "The file to interpret"
      )

    replCommand = ReplCommand . Just <$> strOption
      (  long "load"
      <> metavar "INPUT"
      <> help "A file to load into the REPL"
      )

-- evaluateExpr :: String -> Either ParseError Value
-- evaluateExpr = fmap (flip (evalExpr []) VUnit) . parse expr "" . pack
