{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sound.Internotes.Types.Midi where

import qualified Prelude as P
import Sound.Internotes.Prelude

data MidiEvent = NoteOn Channel Note Velocity
               | NoteOff Channel Note Velocity
               deriving (Eq, Ord, Show, Read)

newtype Channel = Channel Int
  deriving (Eq, Ord, Show, Read)

newtype Note = Note Int
  deriving (Eq, Ord, Show, Read, Num)

newtype Velocity = Velocity Int
  deriving (Eq, Ord, Show, Read, Num)
