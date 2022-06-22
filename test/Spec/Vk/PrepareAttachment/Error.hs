module Spec.Vk.PrepareAttachment.Error where

import Vk.Error
  ( PrependAttachmetException (..),
    VKBotException (PrependAttachmetToSendException),
  )

isGetUploadServerException :: VKBotException -> Bool
isGetUploadServerException (PrependAttachmetToSendException GetUploadServerException {}) = True
isGetUploadServerException _ = False

isCheckGetUploadServerResponseException :: VKBotException -> Bool
isCheckGetUploadServerResponseException ex = case ex of
  PrependAttachmetToSendException CheckGetUploadServerResponseException {} -> True
  _ -> False

isLoadToServException :: VKBotException -> Bool
isLoadToServException (PrependAttachmetToSendException LoadToServException {}) = True
isLoadToServException _ = False

isCheckLoadToServResponseException :: VKBotException -> Bool
isCheckLoadToServResponseException ex = case ex of
  PrependAttachmetToSendException CheckLoadToServResponseException {} -> True
  _ -> False

isSaveOnServException :: VKBotException -> Bool
isSaveOnServException ex = case ex of
  PrependAttachmetToSendException SaveOnServException {} -> True
  _ -> False

isCheckSaveOnServResponseException :: VKBotException -> Bool
isCheckSaveOnServResponseException ex = case ex of
  PrependAttachmetToSendException CheckSaveOnServResponseException {} -> True
  _ -> False

isGoToUrlException :: VKBotException -> Bool
isGoToUrlException (PrependAttachmetToSendException GoToUrlException {}) = True
isGoToUrlException _ = False
