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
import qualified Monad.EventListen as EL
import Control.Concurrent.STM.TVar ( newTVarIO, readTVar, writeTVar )
import Data.Dynamic (Dynamic, fromDynamic, toDyn)

data FlumeState = FlumeState
  { nextEventId :: FlumeEventId }

type Flume m' event a = EventListenT m' (FlumeEvent event) (State FlumeEventId) a

newtype FlumeEventId = FlumeEventId Int
  deriving (Eq, Ord, Show, Read, Num)

data FlumeEvent event = CmdEvent FlumeEventId Dynamic
                      | GlobalEvent event

-- data LocalEventType event = LocalUserEvent event
--                           | LocalCmd Dynamic

runFlume :: Maybe (FlumeEvent event) -> FlumeEventId -> Flume m' event a
         -> ((Bool, [m' (FlumeEvent event)], Either (Flume m' event a) a), FlumeEventId)
runFlume mevent eid m = flip runState eid $ runEventListenT m mevent 

--sequential, non concurrent execution of m actions
execFlumeSequential :: Monad m => [FlumeEvent event] -> FlumeEventId -> Flume m event a
                    -> m (Either ((Flume m event a), FlumeEventId) a)
execFlumeSequential [] ceid m = case runFlume Nothing ceid m of
  ((_, _, Right a), _) -> return $ Right a
  ((_, [], er), eid) -> return $ first (,eid) er
  ((_, effects, Left cont), eid) -> do
    events <- sequence effects
    execFlumeSequential events eid cont
execFlumeSequential (x:xs) ceid m = case runFlume (Just x) ceid m of
  ((_, _, Right a), _) -> return $ Right a
  ((_, effects, Left cont), eid) -> do
    events <- sequence effects
    execFlumeSequential (xs <> events) eid cont

execFlumeAsync :: (MonadIO m)
               => (m (FlumeEvent event) -> IO (Maybe (FlumeEvent event)))
               -> FlumeEventId
               -> TQueue (FlumeEvent event) -> Flume m event a -> m a
execFlumeAsync runEffect neid eventsTQ fm = case runFlume Nothing neid fm of
  ((_, effects, Right a), _) -> do
    traverse_ execEffect effects
    return a
  ((_, effects, Left cont), eid) -> do
    traverse_ affectQueue effects
    loop cont eid
  where
    execEffect = liftIO . forkIO . void . runEffect
    affectQueue eff = liftIO . forkIO . void $ do
      mevent <- runEffect eff
      maybe (return ()) (atomically . writeTQueue eventsTQ) mevent
    loop fm ceid = do
      event <- liftIO . atomically $ readTQueue eventsTQ
      case runFlume (Just event) ceid fm of
        ((_, effects, Right a), _) -> do
          traverse_ execEffect effects
          return a
        ((_, effects, Left cont), eid) -> do
          traverse_ affectQueue effects >> loop cont eid

cmd :: (Monad m', Typeable a) => m' a -> Flume m' event a
cmd action = do
  eid <- lift get
  EL.cmd (CmdEvent eid . toDyn <$> action)
  lift . put $ eid + 1
  EL.listen $ cmdListen eid
    where
      cmdListen eid (CmdEvent eid' x)
        | eid == eid' = fromDynamic x
        | otherwise = Nothing
      cmdListen _ _ = Nothing

globalEvent :: Monad m' => m' event -> Flume m' event ()
globalEvent action = EL.cmd $ GlobalEvent <$> action

listen :: (event -> Maybe a) -> Flume m' event a
listen mf = EL.listen f where
  f (GlobalEvent e) = mf e
  f _ = Nothing

testjim :: Identity Text
testjim = do
  j <- flip runReaderT () $ return "jim"
  return j
