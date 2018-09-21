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

import Internotes.Prelude hiding (sleep, waitEither)
import           Control.Monad.Trans                         ( MonadTrans )
import qualified Data.Text                      as Text
import Control.Concurrent.STM.TQueue (TQueue)

newtype EventListenT event m' m a =
  EventListenT { runEventListenT ::
                   Maybe event -> m ( Bool
                                    , [m' event]
                                    , Either (EventListenT event m' m a) a) }

instance MonadTrans (EventListenT event m') where
  lift ma = EventListenT $ \_ -> (False,[],) . Right <$> ma

instance Monad m => Functor (EventListenT event m' m) where
  fmap f (EventListenT g) = EventListenT $ \event -> do
    (consumed, actions, er) <- g event
    case er of
      Right a -> return (consumed, actions, Right $ f a)
      Left cont -> return (consumed, actions, Left $ f <$> cont)

instance Monad m => Applicative (EventListenT event m' m) where
  pure a = EventListenT $ \_ -> return (False, [], Right a)
  (EventListenT mfab) <*> (EventListenT ma) = EventListenT $ \mevent -> do
    (consumed, actions, efab) <- mfab mevent
    case efab of
      Left cont -> return (consumed, actions, Left $ cont <*> (EventListenT ma))
      Right fab -> do
        (consumed', actions', ea) <- ma $ bool mevent Nothing consumed
        case ea of
          Left cont -> return (consumed || consumed', actions <> actions', Left $ fab <$> cont)
          Right a -> return (consumed || consumed', actions <> actions', Right $ fab a)

instance Monad m => Monad (EventListenT event m' m) where
  return = pure
  (EventListenT ma) >>= famb = EventListenT $ \mevent -> do
    (consumed, actions, er) <- ma mevent
    case er of
      Left errma -> return (consumed, actions, Left $ errma >>= famb)
      Right a -> do
        let (EventListenT mb) = famb a
        (consumed', actions', er') <- mb $ bool mevent Nothing consumed
        case er' of
          Left errmb -> return (consumed || consumed', actions <> actions', Left errmb)
          Right b -> return (consumed || consumed', actions <> actions', Right b)

instance Monad m => Alternative (EventListenT event m' m) where
  empty = EventListenT $ \_ -> return (False, [], Left empty)
  (EventListenT ma) <|> (EventListenT mb) = EventListenT $ \ mevent -> do
    (consumed, actions, er) <- ma mevent
    case er of
      Right a -> return (consumed, actions, Right a)
      Left conta -> do
        (consumed', actions', er') <- mb $ bool mevent Nothing consumed
        case er' of
          Right b -> return (consumed' || consumed, actions <> actions', Right b)
          Left contb -> return (consumed' || consumed, actions <> actions',  Left $ conta <|> contb)

listen :: Monad m => (event -> Maybe a) -> EventListenT event m' m a
listen p = EventListenT $ \mevent -> case join (p <$> mevent) of
  Nothing -> return (False, [], Left $ listen p)
  (Just a) -> return (True, [], Right a)

peek :: Monad m => (event -> Maybe a) -> EventListenT event m' m a
peek p = EventListenT $ \mevent -> case join (p <$> mevent) of
  Nothing -> return (False, [], Left $ listen p)
  (Just a) -> return (False, [], Right a)


consumeEvent :: Monad m => EventListenT event m' m ()
consumeEvent = EventListenT $ \_ -> return (True, [], Right ())

-- runPure :: (Monad m, Monad m')
--         => [event]
--         -> (m' b -> m b) -> EventListenT event m' m a
--         -> m (Either (EventListenT event m' m a) a)
-- runPure [] doAction m = do
--   (_, actions, er) <- runEventListenT m Nothing
--   case er of
--     Right a -> return $ Right a
--     Left cont -> do
--       actionEvents <- doAction $ sequence actions
--       case actionEvents of
--         [] -> return . Left $ cont
--         xs -> runPure actionEvents cont
-- runPure (event:events) doAction m = do
--   (_, actions, er) <- runEventListenT m (Just event)
--   case er of
--     Right a -> return $ Right a
--     Left cont -> do
--       actionEvents <- doAction $ sequence actions
--       case actionEvents of
--         [] -> return . Left $ cont
--         xs -> runPure (events <> actionEvents) cont
--       runPure events cont

-- runWithIOEffects :: TQueue event -> EventListenT event IO a
--        -> IO (Either (TQueue event, (EventListenT event IO a)) a)
-- runWithIOEffects [] m = do
--   (_, er) <- runEventListenT m Nothing
--   case er of
--     Right a -> return $ Right a
--     Left errm -> return . Left $ errm
-- runWithIOEffects (event:events) m = do
--   (consumed, er) <- runEventListenT m (Just event)
--   putText $ (bool "Ignored" "Consumed" consumed) <> " " <> show event
--   case er of
--     Right a -> return $ Right a
--     Left errm -> runIO events errm

