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

module Monad.EventListen2 where

import Internotes.Prelude hiding (sleep, waitEither)
import           Control.Monad.Trans                         ( MonadTrans )
import qualified Data.Text                      as Text
import Control.Concurrent.STM.TQueue (TQueue)
import Data.Dynamic (Dynamic)

newtype FlumeEventId = FlumeEventId Int
  deriving (Eq, Ord, Show, Read, Num)

data FlumeEvent event = LocalEvent FlumeEventId (LocalEventType event)
                      | GlobalEvent event

data LocalEventType event = LocalUserEvent event
                          | LocalCmd Dynamic

data FlumeState m' event m a = FlumeState
  { consumedEvent :: Bool
  , nextEventId :: FlumeEventId
  , effects :: [m' (FlumeEvent event)]
  , ret :: (Either (FlumeT m' event m a) a)
  }

newtype FlumeT m' event m a =
  FlumeT { runFlumeT ::
             Maybe (FlumeEvent event) -> m (FlumeState m' event m a)
         }

-- instance MonadTrans (FlumeT event m') where
--   lift ma = FlumeT $ \_ -> (False,[],) . Right <$> ma

-- instance Monad m => Functor (FlumeT event m' m) where
--   fmap f (FlumeT g) = FlumeT $ \event -> do
--     (consumed, actions, er) <- g event
--     case er of
--       Right a -> return (consumed, actions, Right $ f a)
--       Left cont -> return (consumed, actions, Left $ f <$> cont)

-- instance Monad m => Applicative (FlumeT event m' m) where
--   pure a = FlumeT $ \_ -> return (False, [], Right a)
--   (FlumeT mfab) <*> (FlumeT ma) = FlumeT $ \mevent -> do
--     (consumed, actions, efab) <- mfab mevent
--     case efab of
--       Left cont -> return (consumed, actions, Left $ cont <*> (FlumeT ma))
--       Right fab -> do
--         (consumed', actions', ea) <- ma $ bool mevent Nothing consumed
--         case ea of
--           Left cont -> return (consumed || consumed', actions <> actions', Left $ fab <$> cont)
--           Right a -> return (consumed || consumed', actions <> actions', Right $ fab a)

-- instance Monad m => Monad (FlumeT event m' m) where
--   return = pure
--   (FlumeT ma) >>= famb = FlumeT $ \mevent -> do
--     (consumed, actions, er) <- ma mevent
--     case er of
--       Left errma -> return (consumed, actions, Left $ errma >>= famb)
--       Right a -> do
--         let (FlumeT mb) = famb a
--         (consumed', actions', er') <- mb $ bool mevent Nothing consumed
--         case er' of
--           Left errmb -> return (consumed || consumed', actions <> actions', Left errmb)
--           Right b -> return (consumed || consumed', actions <> actions', Right b)

-- instance Monad m => Alternative (FlumeT event m' m) where
--   empty = FlumeT $ \_ -> return (False, [], Left empty)
--   (FlumeT ma) <|> (FlumeT mb) = FlumeT $ \ mevent -> do
--     (consumed, actions, er) <- ma mevent
--     case er of
--       Right a -> return (consumed, actions, Right a)
--       Left conta -> do
--         (consumed', actions', er') <- mb $ bool mevent Nothing consumed
--         case er' of
--           Right b -> return (consumed' || consumed, actions <> actions', Right b)
--           Left contb -> return (consumed' || consumed, actions <> actions',  Left $ conta <|> contb)

-- listen :: Monad m => (event -> Maybe a) -> FlumeT event m' m a
-- listen p = FlumeT $ \mevent -> case join (p <$> mevent) of
--   Nothing -> return (False, [], Left $ listen p)
--   (Just a) -> return (True, [], Right a)

-- peek :: Monad m => (event -> Maybe a) -> FlumeT event m' m a
-- peek p = FlumeT $ \mevent -> case join (p <$> mevent) of
--   Nothing -> return (False, [], Left $ listen p)
--   (Just a) -> return (False, [], Right a)

-- cmd :: Monad m =>  m' event -> FlumeT event m' m ()
-- cmd action = FlumeT $ \_ -> return (False, [action], Right ())

-- consumeEvent :: Monad m => FlumeT event m' m ()
-- consumeEvent = FlumeT $ \_ -> return (True, [], Right ())



-- runPure :: (Monad m, Monad m')
--         => [event]
--         -> (m' b -> m b) -> FlumeT event m' m a
--         -> m (Either (FlumeT event m' m a) a)
-- runPure [] doAction m = do
--   (_, actions, er) <- runFlumeT m Nothing
--   case er of
--     Right a -> return $ Right a
--     Left cont -> do
--       actionEvents <- doAction $ sequence actions
--       case actionEvents of
--         [] -> return . Left $ cont
--         xs -> runPure actionEvents cont
-- runPure (event:events) doAction m = do
--   (_, actions, er) <- runFlumeT m (Just event)
--   case er of
--     Right a -> return $ Right a
--     Left cont -> do
--       actionEvents <- doAction $ sequence actions
--       case actionEvents of
--         [] -> return . Left $ cont
--         xs -> runPure (events <> actionEvents) cont
--       runPure events cont

-- runWithIOEffects :: TQueue event -> FlumeT event IO a
--        -> IO (Either (TQueue event, (FlumeT event IO a)) a)
-- runWithIOEffects [] m = do
--   (_, er) <- runFlumeT m Nothing
--   case er of
--     Right a -> return $ Right a
--     Left errm -> return . Left $ errm
-- runWithIOEffects (event:events) m = do
--   (consumed, er) <- runFlumeT m (Just event)
--   putText $ (bool "Ignored" "Consumed" consumed) <> " " <> show event
--   case er of
--     Right a -> return $ Right a
--     Left errm -> runIO events errm

