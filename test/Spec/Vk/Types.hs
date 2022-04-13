
module Spec.Vk.Types where

import Vk.Api.Response (LoadDocResp, LoadPhotoResp, ServerInfo)
import Vk.Logger (Priority (..))
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
