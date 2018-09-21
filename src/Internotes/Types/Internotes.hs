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
import qualified Monad.EventListen as EventListen
import Control.Concurrent.STM.TQueue
import Data.Time.Clock ( NominalDiffTime )


data InternotesEvent = MidiEvent MidiEvent
                     | CurrentTime NominalDiffTime
                     | ExitInternotes

type Internotes m a = EventListenT InternotesEvent m a

runInternotes :: Internotes m a
               -> Maybe InternotesEvent
               -> m (Bool, Either (EventListenT InternotesEvent m a) a)
runInternotes m mevent = runEventListenT m mevent


runProgram :: (MonadIO m) => TQueue InternotesEvent -> Internotes m a -> m a
runProgram q m = innerloop Nothing m where
  innerloop mevent m' = do
    (_, er) <- runInternotes m' mevent
    case er of
      Right a -> return a
      Left cont -> do
        event <- liftIO . atomically . readTQueue $ q
        innerloop (Just event) cont

sleep :: MonadInternotes m => NominalDiffTime -> Internotes m ()
sleep t = do
  lift $ MI.sleep t
  ct <- lift MI.getCurrentTime
  listen (pastTime $ ct + t)
  where
    pastTime targetTime (CurrentTime time)
      | time >= targetTime = return ()
      | otherwise = Nothing
    pastTime _ _ = Nothing

playNote :: MonadInternotes m => Note -> Velocity -> Internotes m ()
playNote n = lift . MI.playNote n

getCurrentTime :: MonadInternotes m => Internotes m NominalDiffTime
getCurrentTime = lift MI.getCurrentTime

randomInt :: MonadInternotes m => (Int, Int) -> Internotes m Int
randomInt = lift . MI.randomInt

debug :: MonadInternotes m => Text -> Internotes m ()
debug = lift . MI.debug

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

