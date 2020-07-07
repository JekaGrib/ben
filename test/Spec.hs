{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           App
import           Logger
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State

data MockAction = GOTUPDATES | SENDMESSAGE Int T.Text | CONFIRMUPDATES Int | SENDKEYBWITHMSG Int T.Text | LOGMSG Priority String
                                       deriving (Eq,Show)


getUpdatesTest1 :: StateT [MockAction] IO LBS.ByteString
getUpdatesTest1 = StateT $ \s -> return ("{\"name\":\"Joe\",\"age\":12}" , GOTUPDATES : s)

getShortUpdatesTest1 :: StateT [MockAction] IO LBS.ByteString
getShortUpdatesTest1 = StateT $ \s -> return ("{\"ok\":true,\"result\":[]}" , GOTUPDATES : s)

confirmUpdatesTest1 :: Int -> StateT [MockAction] IO LBS.ByteString
confirmUpdatesTest1 offset = StateT $ \s -> 
    return ("{\"ok\":true,\"result\":[]}" , (CONFIRMUPDATES offset) : s)

sendMessageTest1 :: Int -> T.Text -> StateT [MockAction] IO LBS.ByteString
sendMessageTest1 usId msg = StateT $ \s -> 
    return ("{\"name\":\"Joe\",\"age\":12}" , (SENDMESSAGE usId msg) : s)

sendKeybWithMsgTest1 :: Int -> Int -> T.Text-> StateT [MockAction] IO LBS.ByteString
sendKeybWithMsgTest1 usId currN msg = StateT $ \s -> 
    return ("{\"name\":\"Joe\",\"age\":12}" , (SENDKEYBWITHMSG usId msg) : s)

logTest1 :: Priority -> String -> StateT [MockAction] IO ()
logTest1 prio text = StateT $ \s -> 
    return (() , LOGMSG prio text : s)

configTest1 = Config { cStartN = 2 , cBotToken = "ABC123" , cHelpMsg = "Lala" , cRepeatQ = "Why?"}
handleLogTest1 = LogHandle (LogConfig DEBUG) logTest1
handleTest1 = Handle configTest1 handleLogTest1 getUpdatesTest1 getShortUpdatesTest1 confirmUpdatesTest1 sendMessageTest1 sendKeybWithMsgTest1

main :: IO ()
main = hspec $ do
  describe "startApp" $ do
    it "returns a [LOGMSG DEBUG, GOTUPDATES] when given empty update list" $ do
      (res,state) <- runStateT (startApp handleTest1) []
      reverse state `shouldBe` [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", GOTUPDATES]
