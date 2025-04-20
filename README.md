# Tonnetz Traversals

Generates parsimonious voice leading chord progressions using [neo-Riemann transformations](https://en.wikipedia.org/wiki/Neo-Riemannian_theory#Triadic_transformations_and_voice_leading) on triadic nodes the tonnetz graph seen here:

![tonnetz](https://github.com/user-attachments/assets/568d55f2-3c69-4f24-8077-8fb7da06bcef)

Above, starting in CMajor, we apply the Leading transform, followed by Parallel
and then move to the Relative, which you can see labeled on the tonnetz diagram.

    1. Triad: (C4,E4,G4) Mood: Major   (Origin)
    2. Triad: (E4,G4,B3) Mood: Minor   (Leading Tone)
    3. Triad: (E4,G#4,B3) Mood: Major  (Parallel)
    4. Triad: (C#4,E4,G#4) Mood: Minor (Relative)

[You can learn more about the tonnetz here.](https://en.wikipedia.org/wiki/Tonnetz)

## Overview

This program takes a starting chord (C major by default) and applies a sequence of random transformations to generate a musical chord progression. The transformations are based on Neo-Riemannian theory and include operations like:

- Parallel (P): Changes the chord quality between major and minor
- Leading-tone exchange (L): Moves to the nearest chord sharing two common tones
- Relative (R): Moves between relative major/minor chords

## Usage

tonnetz (-k|--key key)
        (-m|--mood mood major| minor)
        (-t|--transform TRANSFORMS L,P,R,N,S,H)
        [-c|--context CONTEXT]

Generate Chord paths through the Tonnetz via neo-reimann triad transformations
