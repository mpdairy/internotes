{-# LANGUAGE NoImplicitPrelude    #-}

module Sound.Internotes.Prelude
  ( module Exports
  , ToCommandText(..)
  ) where


import qualified Prelude as P
import Protolude
import Protolude as Exports hiding (note, Note)
import qualified Data.Text as Text
import Control.Timeout as Exports (sleep)


class ToCommandText a where
  toCommandText :: a -> Text
  toCommandString :: a -> P.String
  toCommandString = Text.unpack . toCommandText

