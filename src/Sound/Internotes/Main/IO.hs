{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Internotes.Main.IO where

import qualified Prelude as P
import Sound.Internotes.Prelude hiding (sleep)
import qualified Sound.Internotes.Prelude as IP
import System.IO hiding (print)
import System.Environment
import System.Process
import Control.Monad
import System.Random
import System.Exit
import Control.Monad.State.Lazy
import qualified Control.Concurrent.Async as Async
import Control.Concurrent
import Sound.Internotes.Types.Midi
import qualified Data.Text as Text
import qualified Sound.Internotes.Types.MonadInternotes as MonadInternotes
import Sound.Internotes.Types.MonadInternotes (MonadInternotes
                                              )
import qualified Streamly.Prelude as S
import Streamly hiding (async)
import qualified Sound.Internotes.Midi.Alsa as Alsa
import Control.Concurrent.STM.TQueue
import Data.Time.Calendar ( Day(ModifiedJulianDay))
import Sound.Internotes.Types.Internotes (InternotesEvent( MidiEvent
                                                         , CurrentTime ))
import Data.Time.Clock ( NominalDiffTime, UTCTime(UTCTime)
                       , diffUTCTime
                       )
import qualified Data.Time.Clock as Clock

-- internotes 9999 2.0 5.0
-- internotes 4 1.0 7.0

instance MonadInternotes InternotesIO where
  playNote n = liftIO . playNote n
  getCurrentTime = ask >>= \ctx -> do
    utc <- liftIO $ Clock.getCurrentTime
    return . diffUTCTime utc $ startUTCTime ctx
  sleep t = ask >>= \ctx -> do
    ct <- MonadInternotes.getCurrentTime
    liftIO . void . forkIO $ do
      IP.sleep t
      atomically . writeTQueue (events ctx) . CurrentTime $ ct + t
  randomInt (low, high) = liftIO $ randomRIO (low, high)
  debug = liftIO . print


data InternotesCtx = InternotesCtx
  { events :: TQueue InternotesEvent
  , startUTCTime :: UTCTime
  , playNoteAudio :: Note -> Velocity -> IO ()
  }

newtype InternotesIO a = InternotesIO
  { _runInternotesIO :: ReaderT InternotesCtx IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader InternotesCtx
           , MonadIO
           )

runInternotesIO :: InternotesIO a -> InternotesCtx -> IO a
runInternotesIO m ctx = flip runReaderT ctx . _runInternotesIO $ m

alsaCtx :: Alsa.Source -> IO InternotesCtx
alsaCtx src = do
  s <- Alsa.midiStream src
  utc <- Clock.getCurrentTime
  q <- newTQueueIO
  forkIO $ S.mapM_ (atomically . writeTQueue q . MidiEvent) s 
  return $ InternotesCtx { events = q
                         , startUTCTime = utc
                         , playNoteAudio   = playNote }

minNote :: Int
minNote = 15

maxNote :: Int
maxNote = 60

main :: IO ()
main = getArgs >>= run

run :: [P.String] -> IO ()
run args = do
  (clustersize :: Int, noteDelay :: Double , clusterDelay :: Double) <- case args of
    [a, b, c] -> return (P.read a, P.read b, P.read c)
    _ -> do
      print "internotes [cluster size] [note delay] [cluster delay]"
      exitFailure
  internotes clustersize noteDelay clusterDelay

internotes :: Int -> Double -> Double -> IO ()
internotes clustersize noteDelay clusterDelay = void . flip runStateT 40 . forever $ do
  lastNote <- get
  cluster <- (lastNote:) <$> replicateM (clustersize - 1) (genRandomNote 12)
  forM_ cluster $ \n -> liftIO $ do
    playNote_ $ Note n
    delayS noteDelay
  delayS clusterDelay
  return ()


delayS :: MonadIO m => Double -> m ()
delayS s = liftIO . threadDelay . fromIntegral . floor $ s * 1000000

genRandomNote :: Int -> StateT Int IO Int
genRandomNote interval = do
  lastNote <- get
  jump <- liftIO $ randomRIO ((-1) * interval, interval)
  let newNote = if lastNote + jump > maxNote || lastNote + jump < minNote
        then lastNote - jump
        else lastNote + jump
  put newNote
  return newNote

between :: Ord a => (a, a) -> a -> a
between (low, high) = min high . max low

-- newtype Note = Note Double
--   deriving (Eq, Show, Ord, Num, Fractional, Floating)

newtype Freq = Freq Double
  deriving (Eq, Show, Ord, Num, Fractional, Floating)

getFreq :: Note -> Freq
getFreq (Note n) = Freq $ 2 ** (fromIntegral n / 12.0) * 27.5

playNote :: Note -> Velocity -> IO ()
playNote note (Velocity v) = do
  let (Freq f) = getFreq note
  void . system $ "play -qn synth 1 pluck " <> show f
--    <> " -vol " <> show (fromIntegral v / 128.0 :: Float)
    <> " &"

playNote_ :: Note -> IO ()
playNote_ = flip playNote (Velocity 100)
