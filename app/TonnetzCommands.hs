{-# LANGUAGE DeriveDataTypeable #-}

module TonnetzCommands where

import Options.Applicative
import NeoRiemann


data CommandArgs = CommandArgs
  { startingKey :: NoteClass,
     startingMood :: Mood,
   transformations :: [Transform]
  } deriving (Show)

parseMood :: ReadM Mood 
parseMood = eitherReader $ \s -> case s of
  "major" -> Right Major
  "minor" -> Right Minor
  _       -> Left $ "Invalid mood: " ++ s

parseTransform :: ReadM Transform
parseTransform = eitherReader $ \s -> case s of
  "L" -> Right L
  "P" -> Right P 
  "R" -> Right R
  _   -> Left $ "Invalid transformation: " ++ s

parseNoteClass :: ReadM NoteClass
parseNoteClass = eitherReader $ \s -> case s of
  "C"  -> Right C
  "Cs" -> Right Cs
  "D"  -> Right D
  "Ds" -> Right Ds
  "E"  -> Right E
  "F"  -> Right F
  "Fs" -> Right Fs
  "G"  -> Right G
  "Gs" -> Right Gs
  "A"  -> Right A
  "As" -> Right As
  "B"  -> Right B
  _    -> Left $ "Invalid note class: " ++ s

commandArgs :: Parser CommandArgs
commandArgs = CommandArgs
  <$> option parseNoteClass
      ( long "key"
      <> short 'k' 
      <> metavar "key"
      <> help "Root notes of the starting chord (e.g. C in 'C,E,G')")
  <*> option parseMood
      ( long "mood"
      <> short 'm'
      <> metavar "mood"
      <> help "Mood of the starting chord (major or minor)")
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
