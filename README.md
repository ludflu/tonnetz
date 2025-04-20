# Chord Progression Generator

![tonnetz](https://github.com/user-attachments/assets/568d55f2-3c69-4f24-8077-8fb7da06bcef)

A Haskell program that generates parsimonious voice leading chord progressions using graph transformations on a variation of the tonnetz graph seen here:

## Overview

This program takes a starting chord (C major by default) and applies a sequence of random transformations to generate a musical chord progression. The transformations are based on Neo-Riemannian theory and include operations like:

- Parallel (P): Changes the chord quality between major and minor
- Leading-tone exchange (L): Moves to the nearest chord sharing two common tones
- Relative (R): Moves between relative major/minor chords

## Usage

tonnetz (-k|--key key) 
        (-m|--mood mood) 
        (-t|--transform TRANSFORMS)
        [-c|--context CONTEXT]

Generate Chord paths through the Tonnetz via neo-reimann triad transformations
