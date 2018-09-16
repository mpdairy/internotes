{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Sound.Internotes.Types.Internotes where

import Sound.Internotes.Prelude hiding (sleep)
import Sound.Internotes.Types.Midi
import Sound.Internotes.Types.MonadInternotes
import qualified Sound.Internotes.Types.MonadInternotes as MI
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
  ct <- lift $ getCurrentTime
  listen (pastTime $ ct + t)
  where
    pastTime targetTime (CurrentTime time)
      | time >= targetTime = return ()
      | otherwise = Nothing
    pastTime _ _ = Nothing
