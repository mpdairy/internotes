{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Internotes.Types.Midi where

import qualified Prelude as P
import Internotes.Prelude
import System.Random (Random)

data MidiEvent = NoteOn Channel Note Velocity
               | NoteOff Channel Note Velocity
               deriving (Eq, Ord, Show, Read)

newtype Channel = Channel Int
  deriving (Eq, Ord, Show, Read)

newtype Note = Note Int
  deriving (Eq, Ord, Show, Read, Num, Random)

newtype Velocity = Velocity Int
  deriving (Eq, Ord, Show, Read, Num)
