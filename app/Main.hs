{-# LANGUAGE OverloadedStrings #-}
module Main where
import JsonScheme
import Data.Aeson.Decoding (decode)
import qualified Data.Aeson.Types as JSONTypes
import Data.List
import qualified Data.Aeson.KeyMap as KeyMap

import qualified Data.Set as Set
import Options.Applicative
import qualified System.Exit as Exit
import qualified Data.ByteString.Lazy as ByteString
import System.IO

data Input
  = FileInput FilePath
  | StdInput

runProgramArgs :: JSONSchemeArgs -> IO ()
runProgramArgs (JSONSchemeArgs { prettify=p, input=input }) = do
    inputString <- (case input of 
        StdInput -> ByteString.getContents 
        FileInput path -> ByteString.readFile path)

    let decoded :: Maybe JSONTypes.Value
        decoded = decode inputString

    case decoded of 
        Nothing -> fail "Couldn't parse JSON!"
        Just x -> (putStrLn . (if p then prettyPrint 0 else inlinePrint) . fromJSON) x

data JSONSchemeArgs = JSONSchemeArgs
          { prettify :: Bool, input :: Input }

jsonSchemeArgsParser :: Parser JSONSchemeArgs
jsonSchemeArgsParser = JSONSchemeArgs
    <$> switch
      ( long "prettify"
      <> short 'p'
      <> help "enabling prettier output" ) <*>
    (
     FileInput <$> strOption
      (  long "file"
      <> short 'f'
      <> metavar "FILENAME"
      <> help "Input file" ) <|>
      flag' StdInput
      (  long "stdin"
      <> help "Read from stdin" )
    )


main :: IO ()
main = runProgramArgs =<< execParser opts
  where
    opts = info (jsonSchemeArgsParser <**> helper)
      ( fullDesc
     <> progDesc "Returns the JSON Scheme of a JSON input"
     <> header "JSON Scheme Generator" )
