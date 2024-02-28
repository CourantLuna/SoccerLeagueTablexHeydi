module Main where

import Prelude
import Data.Maybe (catMaybes)
import Data.String (split)
import Data.String.Common (trim)
import Effect (Effect)
import Effect.Console (logShow)
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

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "football.dat"
  let linesOfData = split (== '\n') content
  let footballData = catMaybes $ map parseFootballData linesOfData
  traverse_ logShow footballData
