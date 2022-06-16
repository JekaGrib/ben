module Spec.Vk.Types where

import Types
import Vk.Api.Response (LoadDocResp, LoadPhotoResp, ServerInfo)
import Vk.Types

data VkMockAction
  = GOTSERVER
  | GOTUPDATES ServerInfo
  | GOTPhotoSERVER UserId
  | LOADPhotoTOSERV ServerUrl PicUrl ResponseS
  | SAVEPhotoONSERV LoadPhotoResp
  | GOTDocSERVER UserId TypeInGetServerReq
  | LOADDocTOSERV ServerUrl DocUrl ResponseS Extention
  | SAVEDocONSERV LoadDocResp Title
  | GOTOURL Url
  deriving (Eq, Show)
