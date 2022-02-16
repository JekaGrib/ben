{-{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}-}
{-# LANGUAGE OverloadedStrings #-} 

module VkTest.PrepareAttachment where

import Vk.App.PrepareAttachment
import VkTest.PrepareAttachment.Handlers
import VkTest.PrepareAttachment.ResponseExample
import VkTest.PrepareAttachment.Types
import Control.Monad.State (execStateT)
import Vk.Logger (Priority(..))
import Vk.Types
import Vk.Api.Response (Size(..),Photo(..))
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow)
import Vk.Api.Response (LoadPhotoResp(..),LoadDocResp(..))



testVkPrAtt :: IO ()
testVkPrAtt =
  hspec $ do
    describe "getAttachmentString" $ do
      it "work with photo attachment with user 5 server 24" $ do
        actions <- 
          execStateT
            (getAttachmentString handle1 5 (PhotoAttachment (Photo [Size 3 3 "http://pic.jpg"])))
            []
        reverse actions `shouldBe`
          [GOTPhotoSERVER 5
          ,LOG DEBUG
          ,GOTOURL "http://pic.jpg"
          ,LOADPhotoTOSERV "http://toLoadPic" "http://pic.jpg" "photo"
          ,LOG DEBUG
          ,SAVEPhotoONSERV (LoadPhotoResp  24 "anyHash" "[{\"markers_restarted\":true,\"photo\":\"79a6b96823:x\",\"sizes\":[],\"latitude\":0,\"longitude\":0,\"kid\":\"8bd681ca71d9263d4eab61b1355a55aa\",\"sizes2\":[[\"s\",\"55bd8ea631621f617e7002492a9e31e8818447f59eecc08783c9701a\",\"-7522010445187125223\",17,17],[\"m\",\"55bd8ea631621f617e7002492a9e31e8818447f59eecc08783c9701a\",\"-7522010445187125223\",17,17],[\"x\",\"111de3b9f23b5a7405a4d250857612d75615e62304a224b0a0a333b4\",\"8389943903789619595\",17,17],[\"o\",\"55bd8ea631621f617e7002492a9e31e8818447f59eecc08783c9701a\",\"-7522010445187125223\",17,17],[\"p\",\"55bd8ea631621f617e7002492a9e31e8818447f59eecc08783c9701a\",\"-7522010445187125223\",17,17],[\"q\",\"55bd8ea631621f617e7002492a9e31e8818447f59eecc08783c9701a\",\"-7522010445187125223\",17,17],[\"r\",\"55bd8ea631621f617e7002492a9e31e8818447f59eecc08783c9701a\",\"-7522010445187125223\",17,17]],\"urls\":[],\"urls2\":[\"Vb2OpjFiH2F-cAJJKp4x6IGER_We7MCHg8lwGg/GfDXeVpznJc.jpg\",\"Vb2OpjFiH2F-cAJJKp4x6IGER_We7MCHg8lwGg/GfDXeVpznJc.jpg\",\"ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA/i5nVVocRb3Q.jpg\",\"Vb2OpjFiH2F-cAJJKp4x6IGER_We7MCHg8lwGg/GfDXeVpznJc.jpg\",\"Vb2OpjFiH2F-cAJJKp4x6IGER_We7MCHg8lwGg/GfDXeVpznJc.jpg\",\"Vb2OpjFiH2F-cAJJKp4x6IGER_We7MCHg8lwGg/GfDXeVpznJc.jpg\",\"Vb2OpjFiH2F-cAJJKp4x6IGER_We7MCHg8lwGg/GfDXeVpznJc.jpg\"],\"peer_id\":16063921}]" )
          ,LOG DEBUG]
