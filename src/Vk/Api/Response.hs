--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.Api.Response
  ( Answer(AnswerOk, updates, FailAnswer, FailTSAnswer, tsFTSA,
       failFTSA, ErrorAnswer, errorEA)
  , Update(Update, objectUpd, UnknownUpdate)
  , AboutObj(AboutObj, from_id, text)
  , Attachment(..)
  , Doc(Doc)
  , Audio(Audio)
  , Photo(Photo)
  , Size(..)
  , LoadDocResp(LoadDocResp)
  , LoadPhotoResp(LoadPhotoResp)
  , SavePhotoResp(SavePhotoResp)
  , StickerInfo(StickerInfo)
  , SaveDocResp(SaveDocResp)
  , ResponseSDR(ResponseSDR)
  , DocInfo(DocInfo)
  , WallInfo(WallInfo)
  , SaveDocAuMesResp(SaveDocAuMesResp)
  , ResponseSDAMR(ResponseSDAMR)
  , GetPollServerJSONBody(GetPollServerJSONBody, responseGPSJB,
                      ErrorAnswerServ, errorEAS)
  , ResponseOk(ResponseOk, ErrorAnswerMsg, errorEAM)
  , ServerInfo(ServerInfo, tsSI, keySI, serverSI)
  , UploadServerResponse(UploadServerResponse)
  , UploadUrl(UploadUrl)
  , Geo(Geo)
  , Coordinates(Coordinates)
  ) where

import Control.Applicative ((<|>), liftA2,empty)
import Data.Aeson (FromJSON(parseJSON), Value(..), (.:), (.:?), withObject,withText)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Answer
  = AnswerOk
      { tsAOk :: T.Text
      , updates :: [Update]
      }
  | FailAnswer
      { failFA :: Integer
      }
  | FailTSAnswer
      { failFTSA :: Integer
      , tsFTSA :: Integer
      }
  | ErrorAnswer
      { errorEA :: Value
      }
  deriving (Generic, Show)

instance FromJSON Answer where
  parseJSON val =
    withObject "AnswerOk" (\v -> AnswerOk <$> v .: "ts" <*> v .: "updates") val <|>
    withObject
      "FailTSAnswer"
      (\v -> FailTSAnswer <$> v .: "fail" <*> v .: "ts")
      val <|>
    withObject "FailAnswer" (\v -> FailAnswer <$> v .: "fail") val <|>
    withObject "ErrorAnswer" (\v -> ErrorAnswer <$> v .: "error") val

data Update
  = Update
      { typeUpd :: T.Text
      , objectUpd :: AboutObj
      }
  | UnknownUpdate Value
  deriving (Show)

instance FromJSON Update where
  parseJSON =
    liftA2
      (<|>)
      (withObject "Update" (\v -> Update <$> v .: "type" <*> v .: "object"))
      (fmap UnknownUpdate . parseJSON)

data AboutObj =
  AboutObj
    { from_id :: Integer
    , id :: Integer
    , peer_id :: Maybe Integer
    , text :: T.Text
    , fwd_messages :: [Value]
    , attachments :: [Attachment]
    , geo :: Maybe Geo
    }
  deriving (Generic, Show)

instance FromJSON AboutObj

data Attachment 
    = PhotoAttachment 
      { photoPA :: Photo }
    | DocAttachment
      { docDA :: Doc }
    | AudioMesAttachment
      { audio_message :: Audio }
    | VideoAttachment
      { docVA :: DocInfo }
    | StickerAttachment
      { sticker :: StickerInfo }
    | AudioAttachment
      { audio :: DocInfo }
    | MarketAttachment
      { market :: DocInfo }
    | WallAttachment
      { wall :: WallInfo }
    | PollAttachment
      { poll :: DocInfo }
    | UnknownAttachment Value 
     deriving (Generic, Show)

instance FromJSON Attachment where
    parseJSON (Object v) = do
      txt <- (v .: "type") :: Parser T.Text
      case txt of
        "photo" -> PhotoAttachment <$> v .: "photo"
        "doc"   -> DocAttachment <$> v .: "doc"
        "audio_message" -> AudioMesAttachment <$> v .: "audio_message"
        "video" -> VideoAttachment <$> v .: "video"
        "sticker" -> StickerAttachment <$> v .: "sticker"
        "audio" -> AudioAttachment <$>  v .: "audio"
        "market" -> MarketAttachment <$> v .: "market"
        "wall" -> WallAttachment <$> v .: "wall"
        "poll" -> PollAttachment <$> v .: "poll"
        _ -> fmap UnknownAttachment . parseJSON $ (Object v)
    parseJSON v = fmap UnknownAttachment . parseJSON $ v  




data Doc =
  Doc
    { urlD :: T.Text
    , extD :: String
    , titleD :: String
    }
  deriving (Eq, Show)

instance FromJSON Doc where
  parseJSON =
    withObject "Doc" $ \v -> Doc <$> v .: "url" <*> v .: "ext" <*> v .: "title"

newtype Audio =
  Audio
    { link_ogg :: T.Text
    }
  deriving (Generic, Eq, Show)

instance FromJSON Audio

newtype Photo =
  Photo
    { sizes :: [Size]
    }
  deriving (Generic, Eq, Show)

instance FromJSON Photo

data Size =
  Size
    { height :: Integer
    , width :: Integer
    , url :: T.Text
    }
  deriving (Generic, Eq, Show)

instance FromJSON Size

newtype LoadDocResp =
  LoadDocResp
    { file :: String
    }
  deriving (Generic, Show,Eq)

instance FromJSON LoadDocResp

data LoadPhotoResp =
  LoadPhotoResp
    { server :: Integer
    , hash :: String
    , photo :: String
    }
  deriving (Generic, Show,Eq)

instance FromJSON LoadPhotoResp

newtype SavePhotoResp =
  SavePhotoResp
    { responseSPR :: [DocInfo]
    }
  deriving (Generic, Show)

instance FromJSON SavePhotoResp where
  parseJSON =
    withObject "SavePhotoResp" $ \v -> SavePhotoResp <$> v .: "response"

data PhotoInfo =
  PhotoInfo
    { idPI :: Integer
    , owner_id :: Integer
    , access_key :: String
    }
  deriving (Generic, Show)

instance FromJSON PhotoInfo where
  parseJSON =
    withObject "PhotoInfo" $ \v ->
      PhotoInfo <$> v .: "id" <*> v .: "owner_id" <*> v .: "access_key"

data AudioMesInfo =
  AudioMesInfo
    { idAMI :: Integer
    , owner_idAMI :: Integer
    , access_keyAMI :: String
    }
  deriving (Generic, Show)

instance FromJSON AudioMesInfo where
  parseJSON =
    withObject "AudioMesInfo" $ \v ->
      AudioMesInfo <$> v .: "id" <*> v .: "owner_id" <*> v .: "access_key"

newtype StickerInfo =
  StickerInfo
    { sticker_id :: Integer
    }
  deriving (Generic, Eq, Show)

instance FromJSON StickerInfo

newtype SaveDocResp =
  SaveDocResp
    { responseSDR :: ResponseSDR
    }
  deriving (Generic, Show)

instance FromJSON SaveDocResp where
  parseJSON = withObject "SaveDocResp" $ \v -> SaveDocResp <$> v .: "response"

data ResponseSDR =
  ResponseSDR
    { typeRSDR :: T.Text
    , docRSDR :: DocInfo
    }
  deriving (Generic, Show)

instance FromJSON ResponseSDR where
  parseJSON =
    withObject "ResponseSDR" $ \v -> ResponseSDR <$> v .: "type" <*> v .: "doc"

data DocInfo =
  DocInfo
    { idDI :: Integer
    , owner_idDI :: Integer
    }
  deriving (Generic, Eq, Show)

instance FromJSON DocInfo where
  parseJSON =
    withObject "DocInfo" $ \v -> DocInfo <$> v .: "id" <*> v .: "owner_id"

data WallInfo =
  WallInfo
    { idWI :: Integer
    , from_idWI :: Integer
    }
  deriving (Generic, Eq, Show)

instance FromJSON WallInfo where
  parseJSON =
    withObject "WallInfo" $ \v -> WallInfo <$> v .: "id" <*> v .: "from_id"

newtype SaveDocAuMesResp =
  SaveDocAuMesResp
    { responseSDAMR :: ResponseSDAMR
    }
  deriving (Generic, Show)

instance FromJSON SaveDocAuMesResp where
  parseJSON =
    withObject "SaveDocAuMesResp" $ \v -> SaveDocAuMesResp <$> v .: "response"

data ResponseSDAMR =
  ResponseSDAMR
    { typeSDAMR :: T.Text
    , docSDAMR :: DocInfo
    }
  deriving (Generic, Show)

instance FromJSON ResponseSDAMR where
  parseJSON =
    withObject "ResponseSDAMR" $ \v ->
      ResponseSDAMR <$> v .: "type" <*> v .: "audio_message"

data GetPollServerJSONBody
  = GetPollServerJSONBody
      { responseGPSJB :: ServerInfo
      }
  | ErrorAnswerServ
      { errorEAS :: Value
      }
  deriving (Generic, Show)

instance FromJSON GetPollServerJSONBody where
  parseJSON =
    liftA2
      (<|>)
      (withObject "GetPollServerJSONBody" $ \v ->
         GetPollServerJSONBody <$> v .: "response")
      (withObject "ErrorAnswerServ" $ \v -> ErrorAnswerServ <$> v .: "error")

data ServerInfo =
  ServerInfo
    { keySI :: T.Text
    , serverSI :: T.Text
    , tsSI :: T.Text
    }
  deriving (Eq, Generic, Show)

instance FromJSON ServerInfo where
  parseJSON =
    withObject "ServerInfo" $ \v ->
      ServerInfo <$> v .: "key" <*> v .: "server" <*> v .: "ts"

data ResponseOk
  = ResponseOk
      { responseR :: Integer
      }
  | ErrorAnswerMsg
      { errorEAM :: Value
      }
  deriving (Generic, Show)

instance FromJSON ResponseOk where
  parseJSON =
    liftA2
      (<|>)
      (withObject "ResponseOk" $ \v -> ResponseOk <$> v .: "response")
      (withObject "ErrorAnswerMsg" $ \v -> ErrorAnswerMsg <$> v .: "error")

newtype ErrorInfo =
  ErrorInfo
    { error_code :: Integer
    }
  deriving (Generic, Show)

instance FromJSON ErrorInfo

newtype UploadServerResponse =
  UploadServerResponse
    { responsePSR :: UploadUrl
    }
  deriving (Generic, Show)

instance FromJSON UploadServerResponse where
  parseJSON =
    withObject "UploadServerResponse" $ \v ->
      UploadServerResponse <$> v .: "response"

newtype UploadUrl =
  UploadUrl
    { upload_url :: T.Text
    }
  deriving (Generic, Show)

instance FromJSON UploadUrl

data Geo =
  Geo
    { typeG :: T.Text
    , coordinates :: Coordinates
    }
  deriving (Eq, Generic, Show)

instance FromJSON Geo where
  parseJSON =
    withObject "Geo" $ \v -> Geo <$> v .: "type" <*> v .: "coordinates"

data Coordinates =
  Coordinates
    { latitude :: Double
    , longitude :: Double
    }
  deriving (Eq, Generic, Show)

instance FromJSON Coordinates

isText :: T.Text -> Parser T.Text -> Parser T.Text
isText txt m = do
  a <- m
  if a  == txt
    then m
    else empty

isPhoto,isDoc,isAuMes,isVideo,isSticker,isAudio,isMarket,isWall,isPoll :: Parser T.Text -> Parser T.Text
isPhoto m = isText "photo" m
isDoc m = isText "doc" m
isAuMes m = isText "audio_message" m
isVideo m = isText "video" m
isSticker m = isText "sticker" m
isAudio m = isText "audio" m
isMarket m = isText "market" m
isWall m = isText "wall" m
isPoll m = isText "poll" m
