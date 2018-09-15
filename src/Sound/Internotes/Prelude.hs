{-# LANGUAGE NoImplicitPrelude    #-}

module Sound.Internotes.Prelude
  ( module Exports
  , ToCommandText(..)
  , sleep
  ) where


import qualified Prelude as P
import Protolude
import Protolude as Exports hiding (note, Note)
import qualified Data.Text as Text


class ToCommandText a where
  toCommandText :: a -> Text
  toCommandString :: a -> P.String
  toCommandString = Text.unpack . toCommandText

sleep :: Double -> IO ()
sleep t = threadDelay . fromIntegral . floor $ t * 1000000
