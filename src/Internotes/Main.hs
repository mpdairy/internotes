{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Internotes.Main where

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
import Internotes.Programs.Simple
import qualified Internotes.Programs.Sequence as Sequence
import Internotes.Main.IO
import Internotes.Types.MonadInternotes
import Internotes.Midi.Alsa (Source(Source))
-- internotes 9999 2.0 5.0
-- internotes 4 1.0 7.0

runProgram :: InternotesIO a -> IO ()
runProgram program = do
  ctx <- alsaCtx (Source 20 0)
  void $ runInternotesIO program ctx



