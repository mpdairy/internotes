{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Monad.EventListen where

import Sound.Internotes.Prelude hiding (sleep, waitEither)
import           Control.Monad.Trans                         ( MonadTrans )
import qualified Data.Text                      as Text

newtype EventListenT event m a =
  EventListenT { runEventListenT ::
                   Maybe event -> m (Bool, Either (EventListenT event m a) a) }

instance MonadTrans (EventListenT event) where
  lift ma = EventListenT $ \_ -> (False,) . Right <$> ma

instance Monad m => Functor (EventListenT event m) where
  fmap f (EventListenT g) = EventListenT $ \event -> do
    (consumed, er) <- g event
    case er of
      Right a -> return (consumed, Right $ f a)
      Left cont -> return (consumed, Left $ f <$> cont)

instance Monad m => Applicative (EventListenT event m) where
  pure a = EventListenT $ \_ -> return (False, Right a)
  (EventListenT mfab) <*> (EventListenT ma) = EventListenT $ \mevent -> do
    (consumed, efab) <- mfab mevent
    case efab of
      Left cont -> return (consumed, Left $ cont <*> (EventListenT ma))
      Right fab -> do
        (consumed', ea) <- ma $ bool mevent Nothing consumed
        case ea of
          Left cont -> return (consumed || consumed', Left $ fab <$> cont)
          Right a -> return (consumed || consumed', Right $ fab a)

instance Monad m => Monad (EventListenT event m) where
  return = pure
  (EventListenT ma) >>= famb = EventListenT $ \mevent -> do
    (consumed, er) <- ma mevent
    case er of
      Left errma -> return (consumed, Left $ errma >>= famb)
      Right a -> do
        let (EventListenT mb) = famb a
        (consumed', er') <- mb $ bool mevent Nothing consumed
        case er' of
          Left errmb -> return (consumed || consumed', Left errmb)
          Right b -> return (consumed || consumed', Right b)

instance Monad m => Alternative (EventListenT event m) where
  empty = EventListenT $ \_ -> return (False, Left empty)
  (EventListenT ma) <|> (EventListenT mb) = EventListenT $ \ mevent -> do
    (consumed, er) <- ma mevent
    case er of
      Right a -> return (consumed, Right a)
      Left conta -> do
        (consumed', er') <- mb $ bool mevent Nothing consumed
        case er' of
          Right b -> return (consumed' || consumed, Right b)
          Left contb -> return (consumed' || consumed, Left $ conta <|> contb)

listen :: Monad m => (event -> Maybe a) -> EventListenT event m a
listen p = EventListenT $ \mevent -> case join (p <$> mevent) of
  Nothing -> return (False, Left $ listen p)
  (Just a) -> return (True, Right a)

consumeEvent :: Monad m => EventListenT event m ()
consumeEvent = EventListenT $ \_ -> return (True, Right ())


run :: Monad m => [event] -> EventListenT event m a
       -> m (Either (EventListenT event m a) a)
run [] m = do
  (_, er) <- runEventListenT m Nothing
  case er of
    Right a -> return $ Right a
    Left errm -> return . Left $ errm
run (event:events) m = do
  (_, er) <- runEventListenT m (Just event)
  case er of
    Right a -> return $ Right a
    Left errm -> run events errm

runIO :: Show event => [event] -> EventListenT event IO a
       -> IO (Either (EventListenT event IO a) a)
runIO [] m = do
  (_, er) <- runEventListenT m Nothing
  case er of
    Right a -> return $ Right a
    Left errm -> return . Left $ errm
runIO (event:events) m = do
  (consumed, er) <- runEventListenT m (Just event)
  putText $ (bool "Ignored" "Consumed" consumed) <> " " <> show event
  case er of
    Right a -> return $ Right a
    Left errm -> runIO events errm

