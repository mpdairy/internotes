{-# LANGUAGE NoImplicitPrelude          #-}

module Internotes.Midi.Generate where

import Internotes.Prelude
import System.Random (randomRIO)
import Internotes.Types.Midi ( Note(Note) )

simpleSequence :: MonadIO m => Note -> Note -> Note -> Int -> Note -> m [Note]
simpleSequence _ _ _ 0 _ = return []
simpleSequence minNote maxNote maxJump len currentNote = do
  j <- liftIO $ randomRIO ((-1) * maxJump, maxJump)
  let jplus = currentNote + j
      r' = if jplus > maxNote || jplus < minNote
           then currentNote - j
           else jplus
  (r':) <$> simpleSequence minNote maxNote maxJump (len - 1) r'
