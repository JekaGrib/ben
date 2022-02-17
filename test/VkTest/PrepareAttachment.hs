{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-} 

module VkTest.PrepareAttachment where

import Vk.App.PrepareAttachment
import VkTest.PrepareAttachment.Handlers
import VkTest.Types
import VkTest.PrepareAttachment.Oops
import Control.Monad.State (runStateT,evalStateT)
import Control.Monad.Except (runExceptT)
import Vk.Logger (Priority(..))
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow, shouldSatisfy)
import Vk.Api.Response
         (Size(..), Photo(..), Attachment(..), Doc(..), Audio(..),
          StickerInfo(..), LoadPhotoResp(..), LoadDocResp(..), DocInfo(..))


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
      it "throw CheckGetUploadServerResponseException on unknown getUploadServer response" $ 
        runStateT (getPhotoDocInfo handle6 5 "http://pic.jpg") [] `shouldThrow`
          isCheckGetUploadServerResponseException
      it "throw CheckLoadToServResponseException on unknown loadToServer response" $ 
        runStateT (getPhotoDocInfo handle7 5 "http://pic.jpg") [] `shouldThrow`
          isCheckLoadToServResponseException
      it "throw CheckSaveOnServResponseException on unknown saveOnServer response" $ 
        runStateT (getPhotoDocInfo handle8 5 "http://pic.jpg") [] `shouldThrow`
          isCheckSaveOnServResponseException
      it "throw GetUploadServerException on getUploadServer HttpException" $ 
        runStateT (getPhotoDocInfo handle12 5 "http://pic.jpg") [] `shouldThrow`
          isGetUploadServerException
      it "throw LoadToServException on loadToServer HttpException" $ 
        runStateT (getPhotoDocInfo handle13 5 "http://pic.jpg") [] `shouldThrow`
          isLoadToServException
      it "throw SaveOnServException on saveOnServer HttpException" $ 
        runStateT (getPhotoDocInfo handle14 5 "http://pic.jpg") [] `shouldThrow`
          isSaveOnServException
      it "throw GoToUrlException on goToUrl HttpException" $ 
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
      it "throw CheckGetUploadServerResponseException on unknown getUploadServer response" $ 
        runStateT (getDocInfo handle9 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isCheckGetUploadServerResponseException
      it "throw CheckLoadToServResponseException on unknown loadToServer response" $ 
        runStateT (getDocInfo handle10 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isCheckLoadToServResponseException
      it "throw CheckSaveOnServResponseException on unknown saveOnServer response" $ 
        runStateT (getDocInfo handle11 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isCheckSaveOnServResponseException
      it "throw GetUploadServerException on getUploadServer HttpException" $ 
        runStateT (getDocInfo handle16 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isGetUploadServerException
      it "throw LoadToServException on loadToServer HttpException" $ 
        runStateT (getDocInfo handle17 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isLoadToServException
      it "throw SaveOnServException on saveOnServer HttpException" $ 
        runStateT (getDocInfo handle18 6 (Doc "http://doc" "hs" "MyDoc")) [] `shouldThrow`
          isSaveOnServException
      it "throw GoToUrlException on goToUrl HttpException" $ 
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
      it "throw CheckGetUploadServerResponseException on unknown getUploadServer response" $ 
        runStateT (getAudioMsgDocInfo handle9 4 (Audio "http://doc")) [] `shouldThrow`
          isCheckGetUploadServerResponseException
      it "throw CheckLoadToServResponseException on unknown loadToServer response" $ 
        runStateT (getAudioMsgDocInfo handle10 4 (Audio "http://doc")) [] `shouldThrow`
          isCheckLoadToServResponseException
      it "throw CheckSaveOnServResponseException on unknown saveOnServer response" $ 
        runStateT (getAudioMsgDocInfo handle11 4 (Audio "http://doc")) [] `shouldThrow`
          isCheckSaveOnServResponseException
      it "throw GetUploadServerException on getUploadServer HttpException" $ 
        runStateT (getAudioMsgDocInfo handle16 4 (Audio "http://doc")) [] `shouldThrow`
          isGetUploadServerException
      it "throw LoadToServException on loadToServer HttpException" $ 
        runStateT (getAudioMsgDocInfo handle17 4 (Audio "http://doc")) [] `shouldThrow`
          isLoadToServException
      it "throw SaveOnServException on saveOnServer HttpException" $ 
        runStateT (getAudioMsgDocInfo handle18 4 (Audio "http://doc")) [] `shouldThrow`
          isSaveOnServException
      it "throw GoToUrlException on goToUrl HttpException" $ 
        runStateT (getAudioMsgDocInfo handle15 4 (Audio "http://doc"))[] `shouldThrow`
          isGoToUrlException
    describe "getAttachmentString" $ do
      it "work with photo attachment" $ do
        attStr <- 
          evalStateT
           (runExceptT $ getAttachmentString handle1 5 (PhotoAttachment (Photo [Size 3 3 "http://pic.jpg"]))) 
           []
        attStr `shouldBe`
          Right "photo50_25"
      it "work with doc attachment" $ do
        attStr <- 
          evalStateT
           (runExceptT $ getAttachmentString handle2 5 (DocAttachment (Doc "http://doc" "hs" "MyDoc"))) 
           []
        attStr `shouldBe`
          Right "doc50_25"
      it "work with audio message attachment" $ do
        attStr <- 
          evalStateT
           (runExceptT $ getAttachmentString handle3 5 (AudioMesAttachment (Audio "http://doc"))) 
           []
        attStr `shouldBe`
          Right "doc50_25"
      it "work with video attachment" $ do
        attStr <- 
          evalStateT
           (runExceptT $ getAttachmentString handle3 5 (VideoAttachment (DocInfo 25 50))) 
           []
        attStr `shouldBe`
          Right "video50_25"
      it "throwError SomethingWrong on empty photo sizes" $ do
        attStr <- 
          evalStateT
           (runExceptT $ getAttachmentString handle1 5 (PhotoAttachment (Photo []))) 
           []
        attStr `shouldSatisfy`
          null
      it "throwError SomethingWrong on sticker attachment " $ do
        attStr <- 
          evalStateT
           (runExceptT $ getAttachmentString handle1 5 (StickerAttachment (StickerInfo 5))) 
           []
        attStr `shouldSatisfy`
          null
      it "throwError SomethingWrong on unknown attachment " $ do
        attStr <- 
          evalStateT
           (runExceptT $ getAttachmentString handle1 5 (UnknownAttachment "ff")) 
           []
        attStr `shouldSatisfy`
          null
      
        

        
