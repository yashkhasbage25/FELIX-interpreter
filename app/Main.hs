module Main where

import FelixEnvironment
import FelixEvaluate
import FelixParser
import FelixPredefined
import FelixError

import System.Environment
import System.IO
import Control.Exception (catch)
import qualified Data.Text.IO as Text

-- | The main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["repl"] -> startREPL
        ["help"] -> showHelp
        [filePath] -> executeCode filePath
        ["-P", filePath] -> printParseTree filePath
        _ -> showHelp

-- | showHelp prints how to run this app when run
-- | $ cabal run --verbose=0 -- help
showHelp = putStrLn "Incorrect argument format.\nUse: cabal run ARG\nwhere ARG can be \n\trepl, \n\thelp, \n\t<filepath> or \n\t-P <filepath>."

-- | startREPL runs the app in repl mode.
-- | repl mode does not support multiline statements.
startREPL :: IO ()
startREPL = do
    -- one initial environment to begin
    env <- initialEnv
    loop env
    where
        loop env = do
            putStr ">>> "
            hFlush stdout
            -- get single line from stdin
            text <- Text.getLine
            case parseText text of
                -- check for parsing errors
                Left err -> print err >> loop env
                Right prog -> do
                    -- check for errors while running program.
                    catch (run prog env >> return ()) $ \excpt -> do
                        putStrLn $ show (excpt :: FelixError)
                    loop env

executeCode :: FilePath -> IO ()
executeCode filePath = do
    -- read the file
    text <- Text.readFile filePath
    case parseText text of
        -- check for errors like, file does not exists
        Left err -> print err
        Right ast -> do
            env <- initialEnv
            -- the returned AST
            run ast env
            return ()

printParseTree :: FilePath -> IO ()
printParseTree filePath = do
    -- read the file
    text <- Text.readFile filePath
    case parseText text of
    -- check for errors like, file does not exists
        Left err -> print err
        Right ast -> do
            -- print AST
            putStrLn $ show ast
            return ()
