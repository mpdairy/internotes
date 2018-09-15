{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Sound.Internotes.Main where

import qualified Prelude as P
import Sound.Internotes.Prelude hiding (sleep)
import System.IO hiding (print)
import System.Environment
import System.Process
import Control.Monad
import System.Random
import System.Exit
import Control.Monad.State.Lazy
import Control.Concurrent
import Sound.Internotes.Types.Midi
import qualified Data.Text as Text
import Sound.Internotes.Programs.Simple
import qualified Sound.Internotes.Programs.Sequence as Sequence
import Sound.Internotes.Main.IO
import Sound.Internotes.Types.MonadInternotes
import Sound.Internotes.Midi.Alsa (Source(Source))
-- internotes 9999 2.0 5.0
-- internotes 4 1.0 7.0

runProgram :: InternotesIO a -> IO ()
runProgram program = do
  ctx <- alsaCtx (Source 20 0)
  void $ runInternotesIO program ctx



