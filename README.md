# Chord Progression Generator

A Haskell program that generates parsimonious voice leading chord progressions using graph transformations on the dual of the tonnetz graph seen here:

![image](https://github.com/user-attachments/assets/9d97563c-9487-4365-bcd9-0f97be6c7de0 "credit Tilman Piesk https://commons.wikimedia.org/wiki/User:Watchduck")

## Overview

This program takes a starting chord (C major by default) and applies a sequence of random transformations to generate a musical chord progression. The transformations are based on Neo-Riemannian theory and include operations like:

- Parallel (P): Changes the chord quality between major and minor
- Leading-tone exchange (L): Moves to the nearest chord sharing two common tones
- Relative (R): Moves between relative major/minor chords

## Usage
