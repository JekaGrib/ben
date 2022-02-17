{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-} 

module VkTest.PrepareAttachment where

import Vk.App.PrepareAttachment
import VkTest.PrepareAttachment.Handlers
import VkTest.PrepareAttachment.Types
import VkTest.PrepareAttachment.Oops
import Control.Monad.State (runStateT,evalStateT)
import Control.Monad.Except (runExceptT)
import Vk.Logger (Priority(..))
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
      it "throw CheckGetUploadServerResponseException on unknown getUploadServer response" $ do
        runStateT (getPhotoDocInfo handle6 5 "http://pic.jpg") [] `shouldThrow`
          isCheckGetUploadServerResponseException
      it "throw CheckLoadToServResponseException on unknown loadToServer response" $ do
        runStateT (getPhotoDocInfo handle7 5 "http://pic.jpg") [] `shouldThrow`
          isCheckLoadToServResponseException
      it "throw CheckSaveOnServResponseException on unknown saveOnServer response" $ do
        runStateT (getPhotoDocInfo handle8 5 "http://pic.jpg") [] `shouldThrow`
          isCheckSaveOnServResponseException
      it "throw GetUploadServerException on getUploadServer HttpException" $ do
        runStateT (getPhotoDocInfo handle12 5 "http://pic.jpg") [] `shouldThrow`
          isGetUploadServerException
      it "throw LoadToServException on loadToServer HttpException" $ do
        runStateT (getPhotoDocInfo handle13 5 "http://pic.jpg") [] `shouldThrow`
          isLoadToServException
      it "throw SaveOnServException on saveOnServer HttpException" $ do
        runStateT (getPhotoDocInfo handle14 5 "http://pic.jpg") [] `shouldThrow`
          isSaveOnServException
      it "throw GoToUrlException on goToUrl HttpException" $ do
        runStateT (getPhotoDocInfo handle15 5 "http://pic.jpg") [] `shouldThrow`
          isGoToUrlException
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
      it "throw CheckGetUploadServerResponseException on unknown getUploadServer response" $ do
        runStateT (getDocInfo handle9 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isCheckGetUploadServerResponseException
      it "throw CheckLoadToServResponseException on unknown loadToServer response" $ do
        runStateT (getDocInfo handle10 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isCheckLoadToServResponseException
      it "throw CheckSaveOnServResponseException on unknown saveOnServer response" $ do
        runStateT (getDocInfo handle11 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isCheckSaveOnServResponseException
      it "throw GetUploadServerException on getUploadServer HttpException" $ do
        runStateT (getDocInfo handle16 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isGetUploadServerException
      it "throw LoadToServException on loadToServer HttpException" $ do
        runStateT (getDocInfo handle17 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isLoadToServException
      it "throw SaveOnServException on saveOnServer HttpException" $ do
        runStateT (getDocInfo handle18 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isSaveOnServException
      it "throw GoToUrlException on goToUrl HttpException" $ do
        runStateT (getDocInfo handle15 6 (Doc "http://doc" "hs" "MyDoc"))[] `shouldThrow`
          isGoToUrlException
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
      it "throw CheckGetUploadServerResponseException on unknown getUploadServer response" $ do
        runStateT (getAudioMsgDocInfo handle9 4 (Audio "http://doc")) [] `shouldThrow`
          isCheckGetUploadServerResponseException
      it "throw CheckLoadToServResponseException on unknown loadToServer response" $ do
        runStateT (getAudioMsgDocInfo handle10 4 (Audio "http://doc")) [] `shouldThrow`
          isCheckLoadToServResponseException
      it "throw CheckSaveOnServResponseException on unknown saveOnServer response" $ do
        runStateT (getAudioMsgDocInfo handle11 4 (Audio "http://doc")) [] `shouldThrow`
          isCheckSaveOnServResponseException
      it "throw GetUploadServerException on getUploadServer HttpException" $ do
        runStateT (getAudioMsgDocInfo handle16 4 (Audio "http://doc")) [] `shouldThrow`
          isGetUploadServerException
      it "throw LoadToServException on loadToServer HttpException" $ do
        runStateT (getAudioMsgDocInfo handle17 4 (Audio "http://doc")) [] `shouldThrow`
          isLoadToServException
      it "throw SaveOnServException on saveOnServer HttpException" $ do
        runStateT (getAudioMsgDocInfo handle18 4 (Audio "http://doc")) [] `shouldThrow`
          isSaveOnServException
      it "throw GoToUrlException on goToUrl HttpException" $ do
        runStateT (getAudioMsgDocInfo handle15 4 (Audio "http://doc"))[] `shouldThrow`
          isGoToUrlException
    describe "getAttachmentString" $ do
      it "work with photo attachment with user 5" $ do
        attStr <- 
          evalStateT
           (runExceptT $ (getAttachmentString handle1 5 (PhotoAttachment (Photo [Size 3 3 "http://pic.jpg"])))) 
           []
        attStr `shouldBe`
          Right "photo50_25"

        
