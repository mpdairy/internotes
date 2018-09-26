{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Internotes.Types.Internotes where

import Internotes.Prelude hiding (sleep)
import Internotes.Types.Midi
import Internotes.Types.MonadInternotes ( MonadInternotes )
import qualified Internotes.Types.MonadInternotes as MI
import Monad.EventListen ( EventListenT( runEventListenT )
                         , listen
                         )
import qualified Monad.Flume as Flume
import Control.Concurrent.STM.TQueue
import Data.Time.Clock ( NominalDiffTime )
import Monad.Flume ( Flume, runFlume )

data InternotesEvent = MidiEvent MidiEvent
                     | CurrentTime NominalDiffTime
                     | ExitInternotes
                     | RandomInt Int
                     | Noop

type Internotes m a = Flume m InternotesEvent a

-- sleep :: MonadInternotes m => NominalDiffTime -> Internotes m ()
-- sleep dt = do
--   ct <- getCurrentTime
--   EventListen.cmd $ const (CurrentTime $ ct + dt) <$> MI.sleep dt
--   listen $ ll ct
--     where
--       ll ct (CurrentTime t)
--         | t >= ct + dt = Just ()
--         | otherwise = Nothing
--       ll _ _ = Nothing

-- noop :: Functor f => f a -> f InternotesEvent
-- noop = fmap (const Noop)

-- playNote :: MonadInternotes m => Note -> Velocity -> Internotes m ()
-- playNote n = EventListen.cmd . noop . MI.playNote n

-- getCurrentTime :: MonadInternotes m => Internotes m NominalDiffTime
-- getCurrentTime = do
--   EventListen.cmd $ CurrentTime <$> MI.getCurrentTime
--   listen ct
--     where
--       ct (CurrentTime t) = Just t
--       ct _ = Nothing

-- randomInt :: MonadInternotes m => (Int, Int) -> Internotes m Int
-- randomInt rr = do
--   EventListen.cmd $ RandomInt <$> MI.randomInt rr
--   listen ct
--     where
--       ct (RandomInt t) = Just t
--       ct _ = Nothing

-- debug :: MonadInternotes m => Text -> Internotes m ()
-- debug = EventListen.cmd . noop . MI.debug

-- instance MonadInternotes m => MonadInternotes (Internotes m) where
--   sleep t = do
--     lift $ MI.sleep t
--     ct <- lift MI.getCurrentTime
--     listen (pastTime $ ct + t)
--       where
--         pastTime targetTime (CurrentTime time)
--           | time >= targetTime = return ()
--           | otherwise = Nothing
--         pastTime _ _ = Nothing

--   playNote n = lift . MI.playNote n
--   getCurrentTime = lift MI.getCurrentTime
--   randomInt = lift . MI.randomInt
--   debug = lift . MI.debug

