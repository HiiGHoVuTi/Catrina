{-# LANGUAGE RecordWildCards, LambdaCase, BangPatterns, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Text hiding (foldl1, map, filter, unlines, empty, foldl, head)
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

loadProgram :: String -> IO (Either CatrinaError (Context, Program))
loadProgram path = do
  fileContents <- openFile path ReadMode >>= hGetContents
  let 
    parsed  = mapBoth (ParserError .pack.show) id 
        $ parse program path
        $ pack fileContents
  -- FIXME(Maxime): monad gymnastics
  loaded       <- fmap join . sequence $ loadDeps <$> parsed
  let
    context = createContext <$> loaded
    tests   = join $ runAllChecks <$> context <*> loaded
  pure $ tests *> unifyTuple (context, loaded)
    where
      getName :: Declaration -> Text
      getName (ArrowDeclaration _ n _ _) = n
      getName (ObjectDeclaration _ n _ ) = n

      filterDecls :: Program -> Program
      filterDecls (Program i exports' decls) 
        = Program i exports' 
        $ filter (flip elem exports' . getName) decls

      concatDecls :: Program -> Program -> Program
      concatDecls (Program i e xs) (Program _ _ ys) = Program i e (xs++ys)

      combinePrograms :: [Program] -> Program
      combinePrograms = foldl1 concatDecls -- . map filterDecls

      loadDeps :: Program -> IO (Either CatrinaError Program)
      loadDeps (Program paths exp' decls)
        = fmap (fmap (combinePrograms.(Program[]exp' decls:)) . sequence) 
        $ forM paths $ (fmap.fmap) snd . loadProgram . unpack . (<> ".rina")

doTheThing Options {optCommand = InterpretCommand{..}} =
  loadProgram interpretedFilename >>= \case
     Left  err -> print err
     Right pog -> void
               . interpretProgram
               $ snd pog

doTheThing Options {optCommand = ReplCommand{..}} = do
   let
    makeEnv = foldl interpretDecl start . programDeclarations
    load maybeName = (fmap.fmap.fmap) makeEnv . loadProgram <$> maybeName

    repl (ctx, env) = do
      input <- getInputLine $ "Rina" #Operator <> "> " #Parens
      case input of
        Nothing   -> pure ()
        Just ":q" -> lift $ putStrLn "Thanks for using Rina 💖"
        Just i    -> do
          let
            -- FIXME(Maxime): semantic analysis on programs too
            parsed  = mapBoth (ParserError .pack.show) id 
              $ parse (expr <* eof) "repl" $ pack i
          -- FIXME(Maxime)
          res <- lift $ 
            (sequenceA (flip (evalExpr env) VUnit <$> parsed) >>= evaluate.force)
            `catch` (\x -> (const.pure.Right $ VPlaceholder) (x :: SomeException))
          case res of
            Left  err -> lift (print err) >> repl (ctx, env)
            Right out -> lift (putStrLn . pShowValue $ out) >> repl (ctx, env)

    in case load replFilename of
         Nothing -> runInputT defaultSettings (repl (createContext (Program [][][]), start))
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

