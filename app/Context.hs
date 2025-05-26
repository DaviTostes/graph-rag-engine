{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Context where

import Control.Monad (filterM)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Network.HTTP.Req
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))

readChunks :: IO [(FilePath, String)]
readChunks = do
  let dir = "assets/chunks"
  files <- listDirectory dir
  let paths = map (dir </>) files
      fileNames = map takeFileName paths

  filePaths <- filterM doesFileExist paths

  contents <- mapM readFile filePaths
  return (zip fileNames contents)

newtype EmbeddingResponse = EmbeddingResponse
  { embeddings :: [[Float]]
  }
  deriving (Show, Generic)

instance FromJSON EmbeddingResponse where
  parseJSON = withObject "EmbeddingResponse" $ \o -> do
    emb <- o .: "embeddings"
    return (EmbeddingResponse emb)

postEmbedding :: (MonadIO m, FromJSON value) => String -> m value
postEmbedding txt = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "model" .= ("nomic-embed-text" :: String),
            "input" .= (txt :: String)
          ]

  let url = http "localhost" /: "api" /: "embed"
  let options = port 11434

  response <-
    req
      POST
      url
      (ReqBodyJson payload)
      jsonResponse
      options
  return $ responseBody response

data Context = Context
  { cId :: Int,
    title :: String,
    text :: String,
    titleVec :: [Float],
    textVec :: [Float]
  }
  deriving (Show, Generic)

instance FromJSON Context

instance ToJSON Context

createContexts :: IO [Context]
createContexts = do
  chunks <- readChunks
  embs <-
    mapM (\(content, _) -> postEmbedding content :: IO EmbeddingResponse) chunks
  textEmbs <-
    mapM (\(_, content) -> postEmbedding content :: IO EmbeddingResponse) chunks

  let indexed = zip [0 ..] (zip chunks (zip embs textEmbs))

  return
    [ Context idx titlex textx (head (embeddings embx)) (head (embeddings textEmbx))
      | (idx, ((titlex, textx), (embx, textEmbx))) <- indexed
    ]

createContextsFile :: IO ()
createContextsFile = do
  contexts <- createContexts
  let jsonData = encode contexts
  BL.writeFile "assets/contexts.json" jsonData
