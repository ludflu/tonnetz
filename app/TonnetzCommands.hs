

module TonnetzCommands where

import Options.Applicative
import NeoRiemann
import Progressions


data CommandArgs = CommandArgs
  { startingKey :: NoteClass,
     startingMood :: Mood,
   transformations :: Maybe [Transform],
   progression :: Maybe [(Progression,Mood)],
   contextSize :: Int,
   duration :: Integer,
   randomize :: Maybe Int,
   play :: Bool,
   _help:: Bool,
   version :: Bool,
   verbose :: Bool,
   midi :: Maybe FilePath
  } deriving (Show)

parseMood :: ReadM Mood
parseMood = eitherReader $ \s -> case s of
  "major" -> Right Major
  "minor" -> Right Minor
  _       -> Left $ "Invalid mood: " ++ s

parseProgression :: ReadM [(Progression,Mood)]
parseProgression = eitherReader readProgressions

parseTransform :: ReadM Transform
parseTransform = eitherReader $ \s ->
  -- Check if input is a single character or a comma-delimited list
  case s of
    "L" -> Right Leading
    "P" -> Right Parallel
    "R" -> Right Relative
    "N" -> Right Nebenverwandt
    "S" -> Right Slide
    "H" -> Right Hexapole
    _ -> Left $ "Invalid transformation: " ++ s

parseTransforms :: ReadM [Transform]
parseTransforms = eitherReader $ \s ->
  -- Split input by commas and parse each character
  let chars = filter (/= ' ') s  -- Remove any spaces
      transformChars = if ',' `elem` chars then splitOnCommas chars else [chars]
  in mapM parseSingleTransform transformChars
  where
    parseSingleTransform "L" = Right Leading
    parseSingleTransform "P" = Right Parallel
    parseSingleTransform "R" = Right Relative
    parseSingleTransform "N" = Right Nebenverwandt
    parseSingleTransform "S" = Right Slide
    parseSingleTransform "H" = Right Hexapole
    parseSingleTransform x   = Left $ "Invalid transformation: " ++ x

    splitOnCommas :: String -> [String]
    splitOnCommas = foldr (\c acc ->
                            case acc of
                              (cur:rest) -> if c == ','
                                           then "":cur:rest
                                           else (c:cur):rest
                              [] -> [""]  -- This case should never happen in practice
                          ) [""]

parseNoteClass :: ReadM NoteClass
parseNoteClass = eitherReader $ \s -> case s of
  "C"  -> Right C
  "Cs" -> Right Cs
  "D"  -> Right D
  "Ds" -> Right Ds
  "E"  -> Right E
  "Es" -> Right F  -- Es is enharmonically equivalent to F
  "F"  -> Right F
  "Fs" -> Right Fs
  "G"  -> Right G
  "Gs" -> Right Gs
  "A"  -> Right A
  "As" -> Right As
  "B"  -> Right B
  "Bs" -> Right C -- Bs is enharmonically equivalent to C
  _    -> Left $ "Invalid note class: " ++ s

commandArgs :: Parser CommandArgs
commandArgs = CommandArgs
  <$> option parseNoteClass
      ( long "key"
      <> short 'k'
      <> metavar "key"
      <> help "Root note of the starting chord (e.g. C in 'C,E,G')")
  <*> option parseMood
      ( long "mood"
      <> short 'm'
      <> metavar "mood"
      <> help "Mood of the starting chord (major or minor)")
  <*> optional (option parseTransforms
      ( long "transform"
      <> short 't'
      <> metavar "TRANSFORMS"
      <> help "Transformations to apply (comma-delimited, e.g., L,P,R,N,S,H) where:\n   L: Leading, P: Parallel, R: Relative, N: Nebenverwandt, S: Slide, H: Hexapole"))
  <*> optional (option parseProgression
      ( long "progression"
      <> metavar "PROGRESSIONS"
      <> help "Chord progressions to apply (I-V-vi-IV)"))
  <*> option auto
      ( long "context"
      <> short 'c'
      <> metavar "CONTEXT"
      <> value 4
      <> help "Context size for the transformations (default: 4)")
  <*> option auto
      ( long "duration"
      <> short 'd'
      <> metavar "DURATION"
      <> value 4
      <> help "Duration of each note in the transformations (defaults to quarter note: 4)")
  <*> optional (option auto
      ( long "randomize"
      <> short 'r'
      <> metavar "RANDOMIZE"
      <> help "Randomize the transformations (default: None)"))
  <*> switch
      ( long "play"
      <> short 'p'    
      <> help "Play the generated transformations using Euterpea (default: False)")
  <*> switch
      ( long "help"
      <> short 'h'
      <> help "Show this help message")
  <*> switch
      ( long "version"
      <> short 'v'
      <> help "Show version information")
  <*> switch
      ( long "verbose"  )
  <*> optional ( strOption 
      (long "midi" <> short 'm' <> metavar "MIDI" <> help "a filepath to write MIDI output" ))

opts :: ParserInfo CommandArgs
opts = info (commandArgs <**> helper)
  ( fullDesc
  <> progDesc "Generate Tonnetz transformations of chords"
  <> header "tonnetz - neo-Riemannian chord transformations")
