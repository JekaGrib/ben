{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module VkTest.PrepareAttachment.Types where

import Vk.Logger ( Priority(..))
import Vk.Types

data MockAction
  = GOTPhotoSERVER UserId
  | LOADPhotoTOSERV ServerUrl PicUrl ResponseS
  | SAVEPhotoONSERV LoadPhotoResp
  | GOTDocSERVER UserId TypeInGetServerReq 
  | LOADDocTOSERV ServerUrl DocUrl ResponseS Extention
  | SAVEDocONSERV LoadDocResp Title
  | GOTOURL Url
  deriving (Eq, Show)

