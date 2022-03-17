{-# LANGUAGE RecordWildCards, LambdaCase, BangPatterns, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Data.Text hiding ( foldl1, map, filter, unlines, empty, foldl, head
                        , takeWhile, drop, length, reverse, dropWhile
                        )
import Errors
import Interpreter
import Javascript.Generate
import Options.Applicative hiding (ParseError, empty)
import Semantics
import Syntax
import System.Console.Haskeline
import System.Directory
import System.IO
import Text.Parsec hiding (try, optional, Error)
import Types.Checker

import Debug.Pretty.Simple

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
  | BuildCommand
    { targetLanguage :: String
    , entrypoint     :: Maybe String
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
    typeCh  = typecheckProgram =<< loaded
  pure $ tests *> typeCh *> unifyTuple (context, loaded)
    where
      {-
      getName :: Declaration -> Text
      getName (ArrowDeclaration _ n _ _) = n
      getName (ObjectDeclaration _ n _ ) = n

      filterDecls :: Program -> Program
      filterDecls (Program i exports' decls) 
        = Program i exports' 
        $ filter (flip elem exports' . getName) decls
      -}

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

doTheThing Options {optCommand = BuildCommand t e}
  | t `elem` ["js", "javascript", "web"] = do
    let 
      entry = fromMaybe "./index.rina" e
      root' = takeWhile (/= '/') entry
    tryCreateDirectory "js-dist"
    rootExists <- doesDirectoryExist root'
    unless rootExists $ print $ "Root directory doesn't exist." #Error
    
    root <- getCurrentDirectory
    setCurrentDirectory root'
    loaded <- loadProgram (drop (length root' + 1) entry)
    case loaded of
      Left err -> print err
      Right _ -> do
        traverseTree (tryCreateDirectory.((root<>"/js-dist/")<>)) 
                     (genJsFile         .((root<>"/js-dist/")<>)) "."

  | otherwise = do
    let tgts = ["js"]
    putStrLn $ "Unknown target." #Error
    putStrLn $ "Possibilities: " <> foldl1 ((<>) . (<> ", ")) (map (#Field) tgts)

  where
    tryCreateDirectory :: String -> IO ()
    tryCreateDirectory path = do
      isDir <- doesDirectoryExist path
      unless isDir $ createDirectory path

    genJsFile :: String -> String -> IO ()
    genJsFile newp strInput = do
      let Right parsed = parse program "" (pack strInput)
          path = (<>"js") $ reverse $ dropWhile (/='.') $ reverse newp
      writeFile path (generateJs parsed)

traverseTree :: (String -> IO ()) -> (String -> String -> IO ()) -> String -> IO ()
traverseTree fdir ffile path = do
  dirs <- listDirectory path
  forM_ dirs $ \ext -> do
    let path' = path <> "/" <> ext
    isDir <- doesDirectoryExist path'
    if isDir
       then fdir  path  >> traverseTree fdir ffile ext
       else ffile path' =<< readFile path'

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
           <> command "repl"      (info replCommand      (progDesc "opens a repl"))
           <> command "build"     (info buildCommand     (progDesc "builds the project"))
           )

    interpretCommand = InterpretCommand <$> strArgument
      (  metavar "INPUT"
      <> help "The file to interpret"
      )

    replCommand = ReplCommand <$> (optional . strOption)
      (  long "load"
      <> metavar "INPUT"
      <> help "A file to load into the REPL"
      )

    buildCommand = BuildCommand 
      <$> strOption
        (  long "target"
        <> short 't'
        <> metavar "TARGET"
        <> help "the target language"
        )
      <*> (optional.strOption)
        (  long "entry"
        <> short 'e'
        <> metavar "ENTRY"
        <> help "the entry point file"
        )


