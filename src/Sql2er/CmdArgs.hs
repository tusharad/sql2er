{-# LANGUAGE DeriveGeneric #-}

module Sql2er.CmdArgs (
    Options(..)
  , parseCmdArgs
) where

import Options.Applicative
import GHC.Generics (Generic)

data Options = Options
    { sqlFile     :: String  -- Required SQL file name
    , outputFile  :: String  -- Optional output file name with a default
    , height      :: Double -- Optional height, if provided
    , width       :: Double -- Optional width, if provided
    } deriving (Show, Generic)

optionsParser :: Parser Options
optionsParser = Options
    <$> argument str
        ( metavar "SQLFILE"
       <> help "Input SQL file name" )
    <*> strOption
        ( long "output"
       <> short 'o'
       <> metavar "OUTPUTFILE"
       <> help "Output file name (default: 'output.jpeg')"
       <> value "output.jpeg" -- Default output file
       <> showDefault )
    <*> option auto
        ( long "height"
       <> metavar "HEIGHT"
       <> help "Optional height for output in pixels (default: 500px)" 
       <> value 500
       )
    <*> option auto
        ( long "width"
       <> metavar "WIDTH"
       <> help "Optional width for output in pixels (default: 500px)" 
       <> value 500
       )

parseCmdArgs :: IO (Options)
parseCmdArgs = execParser opts
 where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Generate ER diagram from SQLFILE and save to OUTPUTFILE"
     <> header "SQL2ER - ER Diagram Generator" )
