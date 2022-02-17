{-{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}-}
{-# LANGUAGE OverloadedStrings #-} 

module VkTest.PrepareAttachment where

import Vk.App.PrepareAttachment
import VkTest.PrepareAttachment.Handlers
import VkTest.PrepareAttachment.ResponseExample
import VkTest.PrepareAttachment.Types
import Control.Monad.State (execStateT,runStateT)
import Control.Monad.Except (runExceptT)
import Vk.Logger (Priority(..))
import Vk.Types
import Vk.Api.Response (Size(..),Photo(..),Attachment(..),Doc(..),Audio(..))
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow)
import Vk.Api.Response (LoadPhotoResp(..),LoadDocResp(..),DocInfo(..))



testVkPrAtt :: IO ()
testVkPrAtt =
  hspec $ do
    describe "getPhotoDocInfo" $ do
      it "work with user 5 pic_url:\"http://pic.jpg\" server 24" $ do
        (docInfo,actions) <- 
          runStateT 
           (getPhotoDocInfo handle1 5 "http://pic.jpg") 
           []
        reverse actions `shouldBe`
          [GOTPhotoSERVER 5
          ,LOG DEBUG
          ,GOTOURL "http://pic.jpg"
          ,LOADPhotoTOSERV "http://toLoadPic" "http://pic.jpg" "anyPhoto"
          ,LOG DEBUG
          ,SAVEPhotoONSERV (LoadPhotoResp  24 "anyHash" "anyPhotoSring" )
          ,LOG DEBUG]
        docInfo `shouldBe`
          DocInfo 25 50
      it "without extra json parameters" $ do
        (docInfo,actions) <- 
          runStateT 
           (getPhotoDocInfo handle4 5 "http://pic.jpg") 
           []
        reverse actions `shouldBe`
          [GOTPhotoSERVER 5
          ,LOG DEBUG
          ,GOTOURL "http://pic.jpg"
          ,LOADPhotoTOSERV "http://toLoadPic" "http://pic.jpg" "anyPhoto"
          ,LOG DEBUG
          ,SAVEPhotoONSERV (LoadPhotoResp  24 "anyHash" "anyPhotoSring")
          ,LOG DEBUG]
        docInfo `shouldBe`
          DocInfo 25 50
      it "work with wrong extra json parameters" $ do
        (docInfo,actions) <- 
          runStateT 
           (getPhotoDocInfo handle5 5 "http://pic.jpg") 
           []
        reverse actions `shouldBe`
          [GOTPhotoSERVER 5
          ,LOG DEBUG
          ,GOTOURL "http://pic.jpg"
          ,LOADPhotoTOSERV "http://toLoadPic" "http://pic.jpg" "anyPhoto"
          ,LOG DEBUG
          ,SAVEPhotoONSERV (LoadPhotoResp  24 "anyHash" "anyPhotoSring")
          ,LOG DEBUG]
        docInfo `shouldBe`
          DocInfo 25 50
    describe "getDocInfo" $ do
      it "work with photo attachment with user 6 doc_url=\"http://doc\"" $ do
        (docInfo,actions) <- 
          runStateT
           (getDocInfo handle2 6 (Doc "http://doc" "hs" "MyDoc")) 
           []
        reverse actions `shouldBe`
          [GOTDocSERVER 6 "doc"
          ,LOG DEBUG
          ,GOTOURL "http://doc"
          ,LOADDocTOSERV "http://toLoadDoc" "http://doc" "anyDoc" "hs"
          ,LOG DEBUG
          ,SAVEDocONSERV (LoadDocResp "anyFile") "MyDoc"
          ,LOG DEBUG]
        docInfo `shouldBe`
          DocInfo 25 50
    describe "getAudioMsgDocInfo" $ do
      it "work with audioMsg attachment with user 4 doc_url=\"http://doc\"" $ do
        (docInfo,actions) <- 
          runStateT 
           (getAudioMsgDocInfo handle3 4 (Audio "http://doc")) 
           []
        reverse actions `shouldBe`
          [GOTDocSERVER 4 "audio_message"
          ,LOG DEBUG,GOTOURL "http://doc"
          ,LOADDocTOSERV "http://toLoadDoc" "http://doc" "anyDoc" "ogg"
          ,LOG DEBUG,SAVEDocONSERV (LoadDocResp "anyFile") "audio_message"
          ,LOG DEBUG]
        docInfo `shouldBe`
          DocInfo 25 50
    describe "getAttachmentString" $ do
      it "work with photo attachment with user 5" $ do
        actions <- 
          execStateT 
           (runExceptT $ (getAttachmentString handle1 5 (PhotoAttachment (Photo [Size 3 3 "http://pic.jpg"])))) 
           []
        reverse actions `shouldBe`
          [GOTPhotoSERVER 5
          ,LOG DEBUG
          ,GOTOURL "http://pic.jpg"
          ,LOADPhotoTOSERV "http://toLoadPic" "http://pic.jpg" "anyPhoto"
          ,LOG DEBUG
          ,SAVEPhotoONSERV (LoadPhotoResp  24 "anyHash" "anyPhotoSring")
          ,LOG DEBUG]
