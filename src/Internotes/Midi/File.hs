{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Internotes.Midi.File where

import qualified Prelude as P
import Internotes.Prelude hiding (sleep)
import Sound.MIDI ( decodeMidi )
import Data.ByteString as BS
import Internotes.Types.Midi ( Note )




