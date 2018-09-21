{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}

module Internotes.Midi.Alsa where

import Internotes.Prelude
import qualified Prelude as P
import Streamly
import Streamly.Prelude ((|:), (.:))
import System.Process
import qualified Streamly.Prelude as S
-- import Text.ParserCombinators.ReadP
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Text as Text
import Internotes.Types.Midi
import Internotes.Types.MonadInternotes (MonadInternotes)
import qualified Sound.MIDI as MIDI

demo :: IO ()
demo = do
  (_ ,mout ,_ ,_) <- createProcess $
    (proc "aseqdump" ["-p", "20:0"]) { std_out = CreatePipe }
  case mout of
    Nothing -> print "Could not load asqdump"
    Just out -> do
      void . runStream . serially
        . S.mapM_ print
        . S.mapMaybe p
        . S.map Text.pack
        $ S.fromHandle out
      where
        p = either (const Nothing) Just . parseOnly aseqMidiEvent


--TODO: Make a thing that lets you select the source from a list
midiStream :: ( MonadIO m, IsStream t, MonadIO m' ) => Source -> m' (t m MidiEvent)
midiStream src = do
  (_ ,mout ,_ ,_) <- liftIO . createProcess $
    (proc "aseqdump" ["-p", toCommandString src]) { std_out = CreatePipe }
  case mout of
    Nothing -> panic "Sorry"
    Just out -> return . serially
      . S.mapMaybe p
      . S.map Text.pack
      $ S.fromHandle out
      where
        p = either (const Nothing) Just . parseOnly aseqMidiEvent


-- sample output from aseqdump
noteOn :: Text
noteOn = " 20:0   Note on                15, note 59, velocity 44"

data Source = Source Int Int
  deriving (Eq, Ord, Show, Read)

instance ToCommandText Source where
  toCommandText (Source a b) = show a <> ":" <> show b

source :: Parser Source
source = do
  a <- decimal
  char ':'
  b <- decimal
  return $ Source a b

channel :: Parser Channel
channel = Channel <$> decimal

note :: Parser Note
note = string "note" >> space >> Note <$> decimal

velocity :: Parser Velocity
velocity = string "velocity" >> space  >> Velocity <$> decimal

bigSpace :: Parser ()
bigSpace = void $ space >> space >> many space

-- rewrite to use sequence or traverse or something!
stringUntil :: Parser a -> Parser Text
stringUntil end = p [] where
  p ls = (lookAhead end >> return (Text.pack . reverse $ ls))
         <|> (anyChar >>= \c -> p (c:ls))

aseqMidiEvent :: Parser MidiEvent
aseqMidiEvent = do
  void $ many space
  src <- source
  bigSpace
  stringUntil bigSpace >>= \case
    "Note on" -> do
      chan <- bigSpace >> channel
      commaSep
      n <- note
      commaSep
      vel <- velocity
      return $ NoteOn chan n vel
    "Note off" -> do
      chan <- bigSpace >> channel
      commaSep
      n <- note
      commaSep
      vel <- velocity
      return $ NoteOff chan n vel
    _ -> mzero
    where
      commaSep = void $ char ',' >> many space
