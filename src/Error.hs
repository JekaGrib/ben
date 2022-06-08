{-# LANGUAGE AllowAmbiguousTypes #-}

module Error where

import qualified Control.Exception as E
import Control.Monad.Catch (Exception, MonadCatch (..), SomeException, throwM)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Configurator.Types as C
import Logger (LogHandle (..), logError)
import Types

data BotException a
  = SendMsgException (MsgType a) ToUserId String
  | CheckSendMsgResponseException (MsgType a) ToUserId String
  | SendKeybException ToUserId String
  | CheckSendKeybResponseException ToUserId String
  | ConfigException ConfigException
  deriving (Eq, Show)

instance (Attachy a) => Exception (BotException a)

data ConfigException
  = PullConfigException String
  | ParseConfigException String
  | GetTimeException String
  | InputException String
  deriving (Eq, Show)

instance Exception ConfigException

throwAndLogEx :: (Monad m, MonadCatch m, Attachy a) => LogHandle m -> BotException a -> m b
throwAndLogEx logH ex = do
  let info = show ex
  logError logH info
  throwM ex

-- handles to catch exceptions in logic functions:

handleExSendMsg ::
  (Monad m, MonadCatch m, Attachy a) =>
  LogHandle m ->
  UserId ->
  MsgType a ->
  SomeException ->
  m LBS.ByteString
handleExSendMsg logH usId msgType e = do
  let ex = SendMsgException msgType (ToUserId usId) $ show e
  throwAndLogEx logH ex

handleExSendKeyb ::
  (Monad m, MonadCatch m) =>
  LogHandle m ->
  UserId ->
  SomeException ->
  m LBS.ByteString
handleExSendKeyb logH usId e = do
  let ex = SendKeybException (ToUserId usId) $ show e :: BotException AttachNotMatter
  throwAndLogEx logH ex

-- handles to catch exceptions in IO configuration functions:
handleExPullConf :: E.SomeException -> IO C.Config
handleExPullConf e = do
  print e
  E.throw $ PullConfigException $ show e

handleExParseConf :: String -> E.SomeException -> IO a
handleExParseConf str e = do
  print e
  E.throw $ ParseConfigException $ str ++ "\n" ++ show e

handleExGetTime :: E.SomeException -> IO String
handleExGetTime e = do
  print e
  E.throw $ GetTimeException $ show e

handleExInput :: String -> E.SomeException -> IO a
handleExInput str e = do
  print e
  E.throw $ InputException $ str ++ "\n" ++ show e
