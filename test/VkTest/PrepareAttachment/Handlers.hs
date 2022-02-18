{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-} 

module VkTest.PrepareAttachment.Handlers where

import Vk.App.PrepareAttachment
import VkTest.PrepareAttachment.ResponseExample
import VkTest.Log
import VkTest.Types
import VkTest.Conf
import Vk.Types
import Control.Monad.State (StateT(..))
import Vk.Api.Response (LoadPhotoResp,LoadDocResp)
import Network.HTTP.Client (HttpException( InvalidUrlException ))
import Control.Monad.Catch (throwM)



handle0,handle1 :: Handle (StateT [MockAction] IO)
handle1 =
  Handle
    { hConf = config1
    , hLog = handLogDebug
    , getPhotoServer = getPhotoServerTest json2
    , loadPhotoToServ = loadPhotoToServTest json3
    , savePhotoOnServ = savePhotoOnServTest json4
    , getDocServer = getDocServerTest json5
    , loadDocToServ = loadDocToServTest json6
    , saveDocOnServ = saveDocOnServTest json7
    , goToUrl = goToUrlTest "anyPhoto"
    }

handle0 = handle1 {hLog = handLogWarn}

getPhotoServerTest :: Response -> UserId -> StateT [MockAction] IO Response
getPhotoServerTest json usId = StateT $ \s -> return (json, GOTPhotoSERVER usId : s)

loadPhotoToServTest :: Response -> ServerUrl -> PicUrl -> ResponseS -> StateT [MockAction] IO Response
loadPhotoToServTest json serUrl picUrl bs = StateT $ \s -> return (json, LOADPhotoTOSERV serUrl picUrl bs : s)

savePhotoOnServTest :: Response -> LoadPhotoResp -> StateT [MockAction] IO Response
savePhotoOnServTest json loadPhotoResp = StateT $ \s -> return (json, SAVEPhotoONSERV loadPhotoResp : s)

getDocServerTest :: Response -> UserId -> TypeInGetServerReq -> StateT [MockAction] IO Response
getDocServerTest json usId type' = StateT $ \s -> return (json, GOTDocSERVER usId type' : s)

loadDocToServTest :: Response -> ServerUrl -> DocUrl -> ResponseS -> Extention -> StateT [MockAction] IO Response
loadDocToServTest json serUrl docUrl bs ext = StateT $ \s -> return (json, LOADDocTOSERV serUrl docUrl bs ext : s)

saveDocOnServTest :: Response -> LoadDocResp -> Title -> StateT [MockAction] IO Response
saveDocOnServTest json loadDocResp title = StateT $ \s -> return (json, SAVEDocONSERV loadDocResp title : s)

goToUrlTest :: ResponseS -> Url -> StateT [MockAction] IO ResponseS
goToUrlTest bs url = StateT $ \s -> return (bs, GOTOURL url : s)

throwHttpEx :: StateT [MockAction] IO a
throwHttpEx = throwM $ InvalidUrlException "" ""

getPhotoServerTestEx :: UserId -> StateT [MockAction] IO Response
getPhotoServerTestEx _ = throwHttpEx

loadPhotoToServTestEx :: ServerUrl -> PicUrl -> ResponseS -> StateT [MockAction] IO Response
loadPhotoToServTestEx _ _ _ = throwHttpEx

savePhotoOnServTestEx :: LoadPhotoResp -> StateT [MockAction] IO Response
savePhotoOnServTestEx _ = throwHttpEx

getDocServerTestEx :: UserId -> TypeInGetServerReq -> StateT [MockAction] IO Response
getDocServerTestEx _ _ = throwHttpEx

loadDocToServTestEx :: ServerUrl -> DocUrl -> ResponseS -> Extention -> StateT [MockAction] IO Response
loadDocToServTestEx _ _ _ _ = throwHttpEx

saveDocOnServTestEx :: LoadDocResp -> Title -> StateT [MockAction] IO Response
saveDocOnServTestEx _ _ = throwHttpEx

goToUrlTestEx :: Url -> StateT [MockAction] IO ResponseS
goToUrlTestEx _ = throwHttpEx

handle2,handle3,handle4,handle5,handle6,handle7,handle8,handle9 :: Handle (StateT [MockAction] IO)
handle2 = handle1 {goToUrl = goToUrlTest "anyDoc"}

handle3 = handle2 {saveDocOnServ = saveDocOnServTest json8}

handle4 = handle1
    { getPhotoServer = getPhotoServerTest json10
    , loadPhotoToServ = loadPhotoToServTest json3
    , savePhotoOnServ = savePhotoOnServTest json11
    }

handle5 = handle1
    { getPhotoServer = getPhotoServerTest json12
    , loadPhotoToServ = loadPhotoToServTest json13
    , savePhotoOnServ = savePhotoOnServTest json14
    }

handle6 = handle1 {getPhotoServer = getPhotoServerTest json1}

handle7 = handle1 {loadPhotoToServ = loadPhotoToServTest json1}

handle8 = handle1 {savePhotoOnServ = savePhotoOnServTest json1}

handle9 = handle1 {getDocServer = getDocServerTest json1}

handle10,handle11,handle12,handle13,handle14,handle15,handle16,handle17,handle18,handle19 :: Handle (StateT [MockAction] IO)
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

handle20 :: Handle (StateT [MockAction] IO)
handle20 = handle3 {hLog = handLogWarn}
