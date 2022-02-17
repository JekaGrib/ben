{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module VkTest.Types where


import Vk.Logger ( Priority(..))
import Vk.Api.Response (ServerInfo,LoadPhotoResp,LoadDocResp)
import Vk.Types

data MockAction
  = GOTSERVER
  | GOTUPDATES ServerInfo
  | SENDMSG UserId MSG
  | SENDKEYB UserId N TextOfKeyb
  | GOTPhotoSERVER UserId
  | LOADPhotoTOSERV ServerUrl PicUrl ResponseS
  | SAVEPhotoONSERV LoadPhotoResp
  | GOTDocSERVER UserId TypeInGetServerReq 
  | LOADDocTOSERV ServerUrl DocUrl ResponseS Extention
  | SAVEDocONSERV LoadDocResp Title
  | GOTOURL Url
  | LOG Priority
  | LOGMSG Priority String
  deriving (Eq, Show)
