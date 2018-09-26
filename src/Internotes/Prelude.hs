{-# LANGUAGE NoImplicitPrelude    #-}

module Internotes.Prelude
  ( module Exports
  , ToCommandText(..)
  ) where


import qualified Prelude as P
import Protolude
import Protolude as Exports hiding (note, Note)
import qualified Data.Text as Text
import Control.Timeout as Exports (sleep)
import Control.Concurrent.MonadIO as Exports (HasFork, fork)
import Control.Monad.Trans.State as Exports (StateT, runStateT)

class ToCommandText a where
  toCommandText :: a -> Text
  toCommandString :: a -> P.String
  toCommandString = Text.unpack . toCommandText

