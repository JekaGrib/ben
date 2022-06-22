module Spec.Vk.PrepareAttachment.Handlers where

import Control.Monad.State (StateT (..))
import qualified Spec.App.Handlers as App
import Spec.Conf
import Spec.Types
import Spec.Vk.Log
import Spec.Vk.PrepareAttachment.ResponseExample
import Spec.Vk.Types
import Types
import Vk.Api.Response (LoadDocResp, LoadPhotoResp)
import Vk.App.PrepareAttachment
import Vk.Types

handle0, handle1 :: Handle (StateT [MockAction VkAttachMSG] IO)
handle1 =
  Handle
    { hConf = config1,
      hLog = handLogDebug,
      getPhotoServer = getPhotoServerTest json2,
      loadPhotoToServ = loadPhotoToServTest json3,
      savePhotoOnServ = savePhotoOnServTest json4,
      getDocServer = getDocServerTest json5,
      loadDocToServ = loadDocToServTest json6,
      saveDocOnServ = saveDocOnServTest json7,
      goToUrl = goToUrlTest "anyPhoto"
    }
handle0 = handle1 {hLog = handLogWarn}

getPhotoServerTest ::
  Response ->
  UserId ->
  StateT [MockAction VkAttachMSG] IO Response
getPhotoServerTest json usId = StateT $ \s ->
  return (json, VkMock (GOTPhotoSERVER usId) : s)

loadPhotoToServTest ::
  Response ->
  ServerUrl ->
  PicUrl ->
  ResponseS ->
  StateT [MockAction VkAttachMSG] IO Response
loadPhotoToServTest json serUrl picUrl bs = StateT $ \s ->
  return (json, VkMock (LOADPhotoTOSERV serUrl picUrl bs) : s)

savePhotoOnServTest ::
  Response ->
  LoadPhotoResp ->
  StateT [MockAction VkAttachMSG] IO Response
savePhotoOnServTest json loadPhotoResp = StateT $ \s ->
  return (json, VkMock (SAVEPhotoONSERV loadPhotoResp) : s)

getDocServerTest ::
  Response ->
  UserId ->
  TypeInGetServerReq ->
  StateT [MockAction VkAttachMSG] IO Response
getDocServerTest json usId type' = StateT $ \s ->
  return (json, VkMock (GOTDocSERVER usId type') : s)

loadDocToServTest ::
  Response ->
  ServerUrl ->
  DocUrl ->
  ResponseS ->
  Extention ->
  StateT [MockAction VkAttachMSG] IO Response
loadDocToServTest json serUrl docUrl bs ext = StateT $ \s ->
  return (json, VkMock (LOADDocTOSERV serUrl docUrl bs ext) : s)

saveDocOnServTest ::
  Response ->
  LoadDocResp ->
  Title ->
  StateT [MockAction VkAttachMSG] IO Response
saveDocOnServTest json loadDocResp title = StateT $ \s ->
  return (json, VkMock (SAVEDocONSERV loadDocResp title) : s)

goToUrlTest :: ResponseS -> Url -> StateT [MockAction VkAttachMSG] IO ResponseS
goToUrlTest bs url = StateT $ \s -> return (bs, VkMock (GOTOURL url) : s)

getPhotoServerTestEx :: UserId -> StateT [MockAction VkAttachMSG] IO Response
getPhotoServerTestEx _ = App.throwHttpEx

loadPhotoToServTestEx ::
  ServerUrl ->
  PicUrl ->
  ResponseS ->
  StateT [MockAction VkAttachMSG] IO Response
loadPhotoToServTestEx _ _ _ = App.throwHttpEx

savePhotoOnServTestEx :: LoadPhotoResp -> StateT [MockAction VkAttachMSG] IO Response
savePhotoOnServTestEx _ = App.throwHttpEx

getDocServerTestEx ::
  UserId ->
  TypeInGetServerReq ->
  StateT [MockAction VkAttachMSG] IO Response
getDocServerTestEx _ _ = App.throwHttpEx

loadDocToServTestEx ::
  ServerUrl ->
  DocUrl ->
  ResponseS ->
  Extention ->
  StateT [MockAction VkAttachMSG] IO Response
loadDocToServTestEx _ _ _ _ = App.throwHttpEx

saveDocOnServTestEx :: LoadDocResp -> Title -> StateT [MockAction VkAttachMSG] IO Response
saveDocOnServTestEx _ _ = App.throwHttpEx

goToUrlTestEx :: Url -> StateT [MockAction VkAttachMSG] IO ResponseS
goToUrlTestEx _ = App.throwHttpEx

handle2,
  handle3,
  handle4,
  handle5,
  handle6,
  handle7,
  handle8,
  handle9 ::
    Handle (StateT [MockAction VkAttachMSG] IO)
handle2 = handle1 {goToUrl = goToUrlTest "anyDoc"}
handle3 = handle2 {saveDocOnServ = saveDocOnServTest json8}
handle4 =
  handle1
    { getPhotoServer = getPhotoServerTest json10,
      loadPhotoToServ = loadPhotoToServTest json3,
      savePhotoOnServ = savePhotoOnServTest json11
    }
handle5 =
  handle1
    { getPhotoServer = getPhotoServerTest json12,
      loadPhotoToServ = loadPhotoToServTest json13,
      savePhotoOnServ = savePhotoOnServTest json14
    }
handle6 = handle1 {getPhotoServer = getPhotoServerTest json1}
handle7 = handle1 {loadPhotoToServ = loadPhotoToServTest json1}
handle8 = handle1 {savePhotoOnServ = savePhotoOnServTest json1}
handle9 = handle1 {getDocServer = getDocServerTest json1}

handle10,
  handle11,
  handle12,
  handle13,
  handle14,
  handle15,
  handle16,
  handle17,
  handle18,
  handle19 ::
    Handle (StateT [MockAction VkAttachMSG] IO)
handle10 = handle1 {loadDocToServ = loadDocToServTest json1}
handle11 = handle1 {saveDocOnServ = saveDocOnServTest json1}
handle12 = handle1 {getPhotoServer = getPhotoServerTestEx}
handle13 = handle1 {loadPhotoToServ = loadPhotoToServTestEx}
handle14 = handle1 {savePhotoOnServ = savePhotoOnServTestEx}
handle15 = handle1 {goToUrl = goToUrlTestEx}
handle16 = handle1 {getDocServer = getDocServerTestEx}
handle17 = handle1 {loadDocToServ = loadDocToServTestEx}
handle18 = handle1 {saveDocOnServ = saveDocOnServTestEx}
handle19 = handle2 {hLog = handLogWarn}

handle20 :: Handle (StateT [MockAction VkAttachMSG] IO)
handle20 = handle3 {hLog = handLogWarn}
