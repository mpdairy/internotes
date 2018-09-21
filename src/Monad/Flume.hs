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
{-# LANGUAGE LambdaCase                 #-}

module Monad.Flume where

import Internotes.Prelude hiding (sleep, waitEither)
import           Control.Monad.Trans                         ( MonadTrans )
import qualified Data.Text                      as Text
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue, readTQueue)
import Monad.EventListen ( EventListenT, runEventListenT )
import Control.Concurrent.STM.TVar ( newTVarIO, readTVar, writeTVar )

type Flume event m a = EventListenT event m Identity a

runFlume :: Maybe event -> Flume event m a
         -> (Bool, [m event], Either (Flume event m a) a)
runFlume mevent m = runIdentity $ runEventListenT m mevent 

--sequential, non concurrent execution of m actions
execFlumeSequential :: Monad m => [event] -> Flume event m a
                    -> m (Either (Flume event m a) a)
execFlumeSequential [] m = case runFlume Nothing m of
  (_, _, Right a) -> return $ Right a
  (_, [], er) -> return er
  (_, effects, Left cont) -> do
    events <- sequence effects
    execFlumeSequential events cont
execFlumeSequential (x:xs) m = case runFlume (Just x) m of
  (_, _, Right a) -> return $ Right a
  (_, effects, Left cont) -> do
    events <- sequence effects
    execFlumeSequential (xs <> events) cont

execFlumeAsync :: (MonadIO m)
               => (m event -> IO (Maybe event)) -> TQueue event -> Flume event m a -> m a
execFlumeAsync runEffect eventsTQ fm = case runFlume Nothing fm of
  (_, effects, Right a) -> do
    traverse_ execEffect effects
    return a
  (_, effects, Left cont) -> do
    traverse_ affectQueue effects
    ftv <- liftIO $ newTVarIO cont
    loop ftv
  where
    execEffect = liftIO . forkIO . void . runEffect
    affectQueue eff = liftIO . forkIO . void $ do
      mevent <- runEffect eff
      maybe (return ()) (atomically . writeTQueue eventsTQ) mevent
    loop ftv = do
      event <- liftIO . atomically $ readTQueue eventsTQ
      (effects, mresult) <- liftIO . atomically $ do
        fm <- readTVar ftv
        case runFlume (Just event) fm of
          (_, effects, Right a) -> return (effects, Just a)
          (_, effects, Left cont) -> do
            writeTVar ftv cont
            return (effects, Nothing)
      case mresult of
        Just a -> do
          traverse_ execEffect effects
          return a
        Nothing -> traverse_ affectQueue effects >> loop ftv
