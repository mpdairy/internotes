{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Sound.Internotes.Types.MonadInternotes where

import Sound.Internotes.Prelude hiding (sleep)
import Sound.Internotes.Types.Midi

type Seconds = Double

class (Monad m) => MonadInternotes m where
  pollMidiEvent :: m MidiEvent
  playNote :: Note -> Velocity -> m ()
  sleep :: Seconds -> m ()
  waitEither :: m a -> m b -> m (Either a b)
  randomInt :: (Int, Int) -> m Int
--  timeout :: Double -> m a -> m a -> m a
  debug :: Text -> m ()


