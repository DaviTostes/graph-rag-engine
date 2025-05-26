{-# LANGUAGE OverloadedStrings #-}

module Main where

import Context (Context (..), EmbeddingResponse (..), postEmbedding)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Foldable (maximumBy)
import Data.List (sort, sortBy)
import Data.Ord
import System.Environment (getArgs)

edgeTreshold :: Float
edgeTreshold = 0.85

queryThreshold :: Float -> Float
queryThreshold t
  | t < 0.6 = t
  | otherwise = 0.7

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

percentile :: Float -> [Float] -> Float
percentile p xs
  | null xs = error "Empty list"
  | p < 0 || p > 100 = error "Percentile must be between 0 and 100"
  | otherwise =
      let sorted = sort xs
          n = fromIntegral (length xs)
          rank = p / 100 * (n - 1)
          i = floor rank
          frac = rank - fromIntegral i
          x1 = sorted !! i
          x2 = sorted !! min (i + 1) (length sorted - 1)
       in x1 + frac * (x2 - x1)

createGraphFile :: [Context.Context] -> [Edge] -> IO ()
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

searchContext :: [Float] -> [Context.Context] -> [Edge] -> Float -> String
searchContext qV cts edgs threshold = do
  case findBestNode of
    Nothing -> ""
    Just bestNode -> 
      let similarityScore = similarity bestNode
      in if similarityScore < threshold
        then ""
        else do
          let cText = text bestNode
              edgesFromBest = [e | e <- edgs, from e == cId bestNode]
          case edgesFromBest of
            [] -> cText
            _ -> do
              let nextCtxs = [c | c <- cts, e <- edgesFromBest, cId c == to e]
                  queryPercentile = percentile 90 (map score edgesFromBest)
                  threshold' = queryThreshold queryPercentile
                  rest = searchContext qV nextCtxs edgs threshold'
              cText ++ "\n" ++ show queryPercentile ++ "\n" ++ rest
  where
    similarity c = cosineSim qV (titleVec c)
    findBestNode =
      if null cts
        then Nothing
        else Just $ maximumBy (comparing similarity) cts

main :: IO ()
main = do
  -- Context.createContextsFile
  content <- BL.readFile "assets/contexts.json"
  case eitherDecode content :: Either String [Context.Context] of
    Left err -> putStrLn $ "Error parsing JSON" ++ err
    Right list -> do
      args <- getArgs
      case args of
        [query] -> do
          response <- Context.postEmbedding query :: IO Context.EmbeddingResponse
          let queryVec = head (embeddings response)

          let allEdges =
                [ Edge
                    (cId c1)
                    (cId c2)
                    (cosineSim (titleVec c1) (titleVec c2))
                  | c1 <- list,
                    c2 <- list,
                    cId c1 /= cId c2,
                    cosineSim (titleVec c1) (titleVec c2) >= edgeTreshold
                ]

          -- createGraphFile list allEdges

          let sims = [(title c, cosineSim queryVec (titleVec c)) | c <- list]
              queryPercentile = percentile 90 (map snd sims)
              threshold = queryThreshold queryPercentile

          -- print queryPercentile
          -- print threshold
          -- mapM_ (\(c, s) -> putStrLn $ "[" ++ c ++ "] -> " ++ show s) (sortBy (comparing (Down . snd)) sims)

          putStrLn $ searchContext queryVec list allEdges threshold
        _ -> putStrLn "Usage ./r"
