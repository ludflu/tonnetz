{-# LANGUAGE DeriveDataTypeable #-}

module TonnetzCommands where

import Options.Applicative
import Data.Text (Text)

data Transform = L | P | R
  deriving (Show, Eq)

data CommandArgs = CommandArgs
  { startingChord :: String
  , transformations :: [Transform]
  } deriving (Show)

parseTransform :: ReadM Transform
parseTransform = eitherReader $ \s -> case s of
  "L" -> Right L
  "P" -> Right P 
  "R" -> Right R
  _   -> Left $ "Invalid transformation: " ++ s

commandArgs :: Parser CommandArgs
commandArgs = CommandArgs
  <$> strOption
      ( long "chord"
      <> short 'c' 
      <> metavar "CHORD"
      <> help "Root notes of the starting chord (e.g. 'C,E,G')")
  <*> many (option parseTransform
      ( long "transform"
      <> short 't'
      <> metavar "TRANSFORM"
      <> help "Transformation to apply (L, P, or R)"))

opts :: ParserInfo CommandArgs
opts = info (commandArgs <**> helper)
  ( fullDesc
  <> progDesc "Generate Tonnetz transformations of chords"
  <> header "tonnetz - neo-Riemannian chord transformations")
