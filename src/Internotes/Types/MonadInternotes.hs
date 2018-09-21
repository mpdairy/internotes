{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Internotes.Types.MonadInternotes where

import Internotes.Prelude hiding (sleep)
import Internotes.Types.Midi
import Data.Time.Clock ( NominalDiffTime )

class (Monad m) => MonadInternotes m where
  playNote :: Note -> Velocity -> m ()
  getCurrentTime :: m NominalDiffTime
  sleep :: NominalDiffTime -> m ()
  randomInt :: (Int, Int) -> m Int
  debug :: Text -> m ()
