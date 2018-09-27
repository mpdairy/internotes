{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Internotes.Programs.Simple where

import qualified Prelude as P
import Internotes.Prelude hiding (sleep)
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
--import Internotes.Types.MonadInternotes hiding (sleep)
import Internotes.Types.MonadInternotes (MonadInternotes
                                        , sleep
                                        , playNote
                                        , randomInt
                                        , debug
                                        , getCurrentTime
                                        )
import Monad.Flume (cmd, listen, globalEvent)
import Internotes.Types.Internotes ( Internotes
                                   , InternotesEvent( MidiEvent )
                                         )
-- internotes 9999 2.0 5.0
-- internotes 4 1.0 7.0

reallySimple :: MonadInternotes m => Internotes m ()
reallySimple = forever $ do
  r <- cmd $ randomInt (40, 42)
  cmd $ playNote (Note r) 100
  cmd $ sleep 1.5

reallySimple_ :: MonadInternotes m => NominalDiffTime -> Text -> Internotes m ()
reallySimple_ t txt= forever $ do
  r <- cmd $ randomInt (40, 42)
  cmd $ playNote (Note r) 100
  cmd $ debug txt
  cmd $ sleep t

biSimple :: MonadInternotes m => Internotes m ()
biSimple = reallySimple_ 1.5 "Buster" <|> reallySimple_ 4.0 "Wiggy Wiggy"

-- simplePlay :: MonadInternotes m => m ()
-- simplePlay = forever $ pollMidiEvent >>= \case
--   NoteOn _ n v -> playNote n v
--   _ -> return ()

-- simple :: MonadInternotes m => m ()
-- simple = forever $ do
--   r <- randomInt (40, 52)
--   let goal = (Note r)
--   playNote goal 120
--   waitForProperNote goal
--   sleep 0.88

simpleFollow :: MonadInternotes m => Int -> Internotes m ()
simpleFollow maxJump = do
  (n, v) <- listen anyNote
  cmd $ playNote n v
  cmd $ sleep 0.88
  run n where
    run lastNote = do
      goal <- nextRandomNote maxJump lastNote
      cmd . debug $ show goal
      cmd $ playNote goal 120
      waitForProperNote goal
      cmd $ sleep 0.88
      run goal

maxNote :: Note
maxNote = 55

minNote :: Note
minNote = 25

nextRandomNote :: MonadInternotes m => Int -> Note -> Internotes m Note
nextRandomNote range (Note n) = do
  r <- cmd $ randomInt ((-1) * range, range)
  let r' = if Note (n + r) > maxNote || Note (n + r) < minNote
           then Note (n - r)
           else Note (n + r)
  return r'

anyNote :: InternotesEvent -> Maybe (Note, Velocity)
anyNote (MidiEvent (NoteOn _ n v)) = Just (n, v)
anyNote _ = Nothing

waitForProperNote :: MonadInternotes m => Note -> Internotes m ()
waitForProperNote goalNote = do
  (n, v) <- listen anyNote
  cmd . debug $ "Playing: " <> show n
  cmd $ playNote n v
  if goalNote == n
    then return ()
    else waitForProperNote goalNote
