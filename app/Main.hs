{-# LANGUAGE OverloadedStrings #-}

module Main where

import Context (Context (..), EmbeddingResponse (..), postEmbedding)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Foldable (maximumBy)
import Data.List (find, sortBy)
import Data.Ord
import System.Environment (getArgs)

edgeTreshold :: Float
edgeTreshold = 0.85

queryThreshold :: Float
queryThreshold = 0.7

cosineSim :: [Float] -> [Float] -> Float
cosineSim a b
  | null a || null b = 0
  | length a /= length b = 0
  | otherwise = dot / (sqrt normA * sqrt normB)
  where
    ab = zip a b
    dot = sum [x * y | (x, y) <- ab]
    normA = sum [x * x | (x, _) <- ab]
    normB = sum [y * y | (_, y) <- ab]

data Edge = Edge
  { from :: Int,
    to :: Int,
    score :: Float
  }
  deriving (Show)

searchContext :: [Float] -> [Context] -> [Edge] -> String
searchContext qV cts edgs = do
  let bestNode = maximumBy (comparing similarity) cts
      similarityScore = similarity bestNode

  if similarityScore < queryThreshold
    then ""
    else do
      let cText = text bestNode
          edgesFromBest = [e | e <- edgs, from e == cId bestNode]

      case edgesFromBest of
        [] -> cText
        _ -> do
          let bestEdge = maximumBy (comparing score) edgesFromBest
              nextNode = find (\e -> cId e == to bestEdge) cts

          case nextNode of
            Nothing -> cText
            Just _ -> do
              let unvisitedEdges = filter (\e -> to bestEdge /= to e) edgs
              let unvisitedNodes = filter (\n -> cId bestNode /= cId n) cts
                  rest = searchContext qV unvisitedNodes unvisitedEdges
              cText ++ "\n" ++ rest
  where
    similarity e = cosineSim qV (vector e)

createGraphFile :: [Context] -> [Edge] -> IO ()
createGraphFile cs es = do
  let txt = unlines $ map (\c -> printNode c ++ unlines (printNodeEdges c)) cs
      bs = BC.pack txt
  BL.writeFile "assets/graph.txt" bs
  where
    printNode c = "[" ++ title c ++ "]\n"
    printEdge c e = " [" ++ title c ++ "] -> score: " ++ show (score e)
    printNodeEdges c =
      let nodeEdges = filter (\e -> cId c == from e) es
          nodes = [(c', e) | c' <- cs, e <- nodeEdges, cId c' == to e]
       in map (uncurry printEdge) (sortBy (flip $ \(_, e1) (_, e2) -> compare (score e1) (score e2)) nodes)

main :: IO ()
main = do
  content <- BL.readFile "assets/contexts.json"
  case eitherDecode content :: Either String [Context] of
    Left err -> putStrLn $ "Error parsing JSON" ++ err
    Right list -> do
      args <- getArgs
      case args of
        [message] -> do
          response <- postEmbedding message :: IO EmbeddingResponse

          let allEdges =
                [ Edge
                    (cId c1)
                    (cId c2)
                    (cosineSim (vector c1) (vector c2))
                  | c1 <- list,
                    c2 <- list,
                    cId c1 /= cId c2,
                    cosineSim (vector c1) (vector c2) >= edgeTreshold
                ]

          putStrLn $ searchContext (head (embeddings response)) list allEdges
        _ -> putStrLn "Usage ./r"
