{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Base64.URL
import qualified Data.Text.IO as T
import Network.HTTP.Req
import Sql2er.CmdArgs (Options (..), parseCmdArgs)
import Sql2er.Mermaid (generateMermaidERD)
import Sql2er.Parser (parseSqlScript, postParsingSetup)
import System.Exit (exitFailure)
import Text.Megaparsec

saveImage :: BS.ByteString -> String -> IO ()
saveImage bs outputFileName = do
  BS.writeFile outputFileName bs
  putStrLn $ "Image saved as " ++ outputFileName

getMermaidImage :: Text -> String -> IO ()
getMermaidImage base64ErdCode outputFileName = do
  imageContent <- runReq defaultHttpConfig $ do
    bs <-
      req
        GET
        (https "mermaid.ink" /: "img" /: base64ErdCode)
        NoReqBody
        bsResponse
        mempty
    return (responseBody bs)
  saveImage imageContent outputFileName 

main :: IO ()
main = do
  opts <- parseCmdArgs
  content <-
    T.toLower
      <$> T.readFile (sqlFile opts)
        `catch` ( \e -> do
                    print (e :: SomeException)
                    exitFailure
                )
  case parse parseSqlScript "sql parser" content of
    Left _ -> putStrLn "parsing failed :("
    Right r -> do
      let tableList = postParsingSetup r []
      let base64Code = encodeBase64 $ generateMermaidERD tableList
      getMermaidImage base64Code (outputFile opts)
        `catch` (\e -> print ("An error occured: " :: String, e :: HttpException))
