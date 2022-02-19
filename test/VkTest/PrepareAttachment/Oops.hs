{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module VkTest.PrepareAttachment.Oops where

import Vk.Oops (VKBotException(PrependAttachmetToSendException),PrependAttachmetException(..))

isGetUploadServerException :: VKBotException -> Bool
isGetUploadServerException (PrependAttachmetToSendException GetUploadServerException{}) = True
isGetUploadServerException _ = False

isCheckGetUploadServerResponseException :: VKBotException -> Bool
isCheckGetUploadServerResponseException (PrependAttachmetToSendException CheckGetUploadServerResponseException{}) = True
isCheckGetUploadServerResponseException _ = False

isLoadToServException :: VKBotException -> Bool
isLoadToServException (PrependAttachmetToSendException LoadToServException{}) = True
isLoadToServException _ = False

isCheckLoadToServResponseException :: VKBotException -> Bool
isCheckLoadToServResponseException (PrependAttachmetToSendException CheckLoadToServResponseException{}) = True
isCheckLoadToServResponseException _ = False

isSaveOnServException :: VKBotException -> Bool
isSaveOnServException (PrependAttachmetToSendException SaveOnServException{}) = True
isSaveOnServException _ = False

isCheckSaveOnServResponseException :: VKBotException -> Bool
isCheckSaveOnServResponseException (PrependAttachmetToSendException CheckSaveOnServResponseException{}) = True
isCheckSaveOnServResponseException _ = False

isGoToUrlException :: VKBotException -> Bool
isGoToUrlException (PrependAttachmetToSendException GoToUrlException{}) = True
isGoToUrlException _ = False
