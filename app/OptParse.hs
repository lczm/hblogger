-- Command line options parsing
module OptParse
    ( Options(..)
    , SingleInput(..)
    , SingleOutput(..)
    , parse
    )
    where

import Data.Maybe (fromMaybe)
import Options.Applicative

-- Let the user choose whether they want ot convert from a directory
-- or convert from a single file
data Options
    = ConvertSingle SingleInput SingleOutput
    | ConvertDir FilePath FilePath
    deriving Show

data SingleInput
    = Stdin
    | InputFile FilePath
    deriving Show

data SingleOutput
    = Stdout
    | OutputFile FilePath
    deriving Show

-- parse command-line options
parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
    info (pOptions <**> helper)
    ( fullDesc
      <> header "hsblogger - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )

-- parser for all options
pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")
      )
      <> command
      "convert-dir"
      ( info
        (helper <*> pConvertDir)
        (progDesc "Convert a directory of markup files to html")
      )
    )

-- single source to sink conversion parser
pConvertSingle :: Parser Options
pConvertSingle =
    ConvertSingle <$> pSingleInput <*> pSingleOutput

-- parser for single input source
pSingleInput :: Parser SingleInput
pSingleInput =
    fromMaybe Stdin <$> optional pInputFile

-- parser for single output sink
pSingleOutput :: Parser SingleOutput
pSingleOutput =
    fromMaybe Stdout <$> optional pOutputFile
    
-- input file parser
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
    where
        parser =
            strOption
                ( long "input"
                  <> short 'i'
                  <> metavar "FILE"
                  <> help "Input file"
                )

-- output file parser
pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
    where
        parser = 
            strOption
                ( long "output"
                  <> short 'o'
                  <> metavar "FILE"
                  <> help "Output file"
                )

-- a directory conversion parser
pConvertDir :: Parser Options
pConvertDir = 
    ConvertDir <$> pInputDir <*> pOutputDir

-- parser for input directory
pInputDir :: Parser FilePath
pInputDir = 
    strOption
        ( long "input"
          <> short 'i'
          <> metavar "DIRECTORY"
          <> help "Input directory"
        )

-- parser for output directory
pOutputDir :: Parser FilePath
pOutputDir =
    strOption
        ( long "output"
          <> short 'o'
          <> metavar "DIRECTORY"
          <> help "Outputdirectory"
        )
