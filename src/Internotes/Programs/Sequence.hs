{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Internotes.Programs.Sequence where

import qualified Prelude as P
import Internotes.Prelude hiding (sleep, waitEither)
import System.IO hiding (print)
import System.Environment
import System.Process
import Control.Monad
import System.Random
import System.Exit
import Control.Monad.State.Lazy
import Control.Concurrent
import Internotes.Types.Midi
import qualified Data.Text as Text
import Internotes.Types.MonadInternotes

-- internotes 9999 2.0 5.0
-- internotes 4 1.0 7.0

reallySimple :: MonadInternotes m => m ()
reallySimple = forever $ do
  r <- randomInt (40, 42)
  playNote (Note r) 100
  sleep 1.5

simplePlay :: MonadInternotes m => m ()
simplePlay = forever $ pollMidiEvent >>= \case
  NoteOn _ n v -> playNote n v
  _ -> return ()

simple :: MonadInternotes m => m ()
simple = forever $ do
  r <- randomInt (40, 52)
  let goal = (Note r)
  playNote goal 120
  waitForProperNote goal
  sleep 0.88

simpleFollow :: MonadInternotes m => Int -> m ()
simpleFollow maxJump = do
  (n, v) <- anyNote
  playNote n v
  sleep 0.88
  run n where
    run lastNote = do
      goal <- nextRandomNote maxJump lastNote
      debug $ show goal
      playNote goal 120
      waitForProperNote goal
      sleep 0.88
      run goal

maxNote :: Note
maxNote = 55

minNote :: Note
minNote = 25

nextRandomNote :: MonadInternotes m => Int -> Note -> m Note
nextRandomNote range (Note n) = do
  r <- randomInt ((-1) * range, range)
  let r' = if Note (n + r) > maxNote || Note (n + r) < minNote
           then Note (n - r)
           else Note (n + r)
  return r'

playAnyNote :: MonadInternotes m => Float -> m ()
playAnyNote reducer = do
  (n, (Velocity v)) <- anyNote
  playNote n . Velocity . floor $ fromIntegral v * reducer

anyNote :: MonadInternotes m => m (Note, Velocity)
anyNote = pollMidiEvent >>= \case
  (NoteOn _ n v) -> return (n, v)
  _ -> anyNote

waitForProperNote :: MonadInternotes m => Note -> m ()
waitForProperNote goalNote = do
  pollMidiEvent >>= \case
    (NoteOn _ n v) -> do
      debug $ "Playing: " <> show n
      playNote n v
      if goalNote == n
        then return ()
        else waitForProperNote goalNote
    _ -> waitForProperNote goalNote

octaveSequenceTo :: Note -> Note -> [Note]
octaveSequenceTo n1 n2
  | n1 == n2 = [n2]
  | abs (n1 - n2) <= 12 = [n1, n2]
  | n1 < n2 = n1 : octaveSequenceTo (n1 + 12) n2
  | n1 > n2 = n1 : octaveSequenceTo (n1 - 12) n2

playSequence :: MonadInternotes m
             => [Note] -> m ()
playSequence = traverse_ (\n -> playNote n 110 >> sleep 0.88)

follow :: forall m. ( Alternative m, MonadInternotes m )
                      => Int -> [Note] -> m ()
follow seqLength song = do
  debug "Play a starting note, genius!"
  (n, v) <- anyNote
  playNote n v
  sleep 0.88
  run n song
    where
      run _ [] = return ()
      run lastNote_ notes = do
        let targetNotes = take seqLength notes
        playSequence targetNotes <|> forever (playAnyNote 0.5)
        waitForNotesOrTimeout 2.0 targetNotes >>= \case
          False -> run lastNote_ notes
          True -> run lastNote_ $ drop seqLength notes
        where
          waitForNotesOrTimeout :: Double -> [Note] -> m Bool
          waitForNotesOrTimeout _ [] = return True
          waitForNotesOrTimeout w (n:ns) = do
            er <- waitEither (sleep w) anyNote
            case er of
              Left _ -> return False
              Right (n', v) -> do
                playNote n' v
                waitForNotesOrTimeout w $ if n' == n then ns else (n:ns)

-- waitForProperNotes :: MonadInternotes m => [Note] -> m ()
-- waitForProperNotes notes = traverse waitForP

randomNotes :: MonadInternotes m => Int -> Int -> Note -> m [Note]
randomNotes _ 0 _ = return []
randomNotes range ll currentNote = do
  nextNote <- nextRandomNote range currentNote
  (nextNote:) <$> randomNotes range (ll - 1) nextNote
