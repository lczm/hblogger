module Main where
import Html
import Markup
import OptParse
import Convert (convert)

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO

main :: IO ()
main = do
    options <- OptParse.parse
    case options of
        ConvertDir input output -> 
            convertDirectory input output
        
        ConvertSingle input output -> do
            (title, inputHandle) <-
                case input of
                    Stdin ->
                        pure ("", stdin)
                    InputFile file ->
                        (,) file <$> openFile file ReadMode
            outputHandle <-
                case output of
                    Stdout -> pure stdout
                    OutputFile file -> do
                        exists <- doesFileExist file
                        shouldOpenFile <-
                            if exists
                                then confirm
                                else pure True
                        if shouldOpenFile
                            then
                                openFile file WriteMode
                            else
                                exitFailure
            convertSingle title inputHandle outputHandle
            hClose inputHandle
            hClose outputHandle

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
    content <- hGetContents input -- handle get contents
    hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

-- Parse a document to markup, convert to HTML then render HTML to string
process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm = do
    putStrLn "Are you sure? (y/n)"
    answer <- getLine
    case answer of 
        "y" -> pure True
        "n" -> pure False
        _ -> do 
            putStrLn "Invalid response. use y or n"
            confirm
