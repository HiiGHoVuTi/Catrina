{-# LANGUAGE RecordWildCards, LambdaCase, BangPatterns #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Text hiding (unlines, empty, foldl, head)
import Errors
import Interpreter
import Options.Applicative hiding (ParseError, empty)
import Semantics
import Syntax
import System.Console.Haskeline
import System.IO
import Text.Parsec hiding (try, optional)
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
doTheThing Options {optCommand = InterpretCommand{..}} = do
   fileContents <- openFile interpretedFilename ReadMode >>= hGetContents
   let 
    parsed = mapBoth ParserError id 
          $ parse program "main" 
          $ pack fileContents
    tests  = join $ runAllChecks <$> (createContext <$> parsed) <*> parsed
    in case parsed <* tests of
         Left  err -> pPrint err
         Right pog -> void
                   . interpretProgram 
                   $ pog

doTheThing Options {optCommand = ReplCommand{..}} = do
   let
    load maybeName = do
      fileName <- maybeName
      let 
        fileContents = openFile fileName ReadMode >>= hGetContents
        program'     = mapBoth ParserError id 
                    . parse program fileName 
                    . pack <$> fileContents
        in Just $ do
          program'' <- program' 
          let context = createContext <$> program''
          (pure.unifyTuple) 
              ( foldl interpretDecl start . programDeclarations <$> program'' 
                  <* join (runAllChecks <$> context <*> program'')
              , context )

    repl (env, ctx) = do
      input <- getInputLine $ "Rina" #Operator <> "> " #Parens
      case input of
        Nothing   -> pure ()
        Just ":q" -> lift $ putStrLn "Thanks for using Rina ❤️"
        Just i    -> do
          let
            -- FIXME(Maxime): semantic analysis on programs too
            parsed  = mapBoth ParserError id $ parse (expr <* eof) "repl" $ pack i
          -- FIXME(Maxime)
          res <- lift $ 
            sequenceA (flip (evalExpr env) VUnit <$> parsed)
            `catch` (\x -> (const.pure.Right $ VPlaceholder) (x :: ErrorCall))
          case res of
            Left  err -> lift (print err) >> repl (env, ctx)
            Right out -> lift (putStrLn . pShowValue $ out) >> repl (env, ctx)

    in case load replFilename of
         Nothing -> runInputT defaultSettings (repl (start, createContext (Program [])))
         Just x  -> x >>= \case
                      Left  err -> print err
                      Right env -> runInputT defaultSettings (repl env)

main :: IO ()
main = execParser opts >>= doTheThing
  where 
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

    replCommand = ReplCommand <$> (optional . strOption)
      (  long "load"
      <> metavar "INPUT"
      <> help "A file to load into the REPL"
      )

