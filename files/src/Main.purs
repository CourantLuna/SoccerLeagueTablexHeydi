module Main where

import Prelude
import Data.Maybe (catMaybes)
import Data.String (split)
import Data.String.Common (trim)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Effect (Effect)
import Effect.Console (logShow, log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type FootballData = { team :: String, goalsFor :: Int, goalsAgainst :: Int }

parseFootballData :: String -> Maybe FootballData
parseFootballData line = case split (== ' ') $ trim line of
  team : _ : _ : _ : fStr : aStr : _ -> do
    goalsFor <- readInt fStr
    goalsAgainst <- readInt aStr
    Just { team, goalsFor, goalsAgainst }
  _ -> Nothing

goalDifference :: FootballData -> Int
goalDifference { goalsFor, goalsAgainst } = abs (goalsFor - goalsAgainst)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "football.dat"
  let linesOfData = split (== '\n') content
  let footballData = catMaybes $ map parseFootballData linesOfData
  case minimumBy (comparing goalDifference) footballData of
    Nothing -> log "No team data was found."
    Just teamWithSmallestDiff -> do
      logShow teamWithSmallestDiff.team
      log $ "Smallest goal difference: " <> show (goalDifference teamWithSmallestDiff)
