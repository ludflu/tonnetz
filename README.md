# Chord Progression Generator

A Haskell program that generates parsimonious voice leading chord progressions using graph transformations.

## Overview

This program takes a starting chord (C major by default) and applies a sequence of random transformations to generate a musical chord progression. The transformations are based on Neo-Riemannian theory and include operations like:

- Parallel (P): Changes the chord quality between major and minor
- Leading-tone exchange (L): Moves to the nearest chord sharing two common tones
- Relative (R): Moves between relative major/minor chords

## Usage
