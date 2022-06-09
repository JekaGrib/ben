{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Response
  ( Answer
      ( AnswerOk,
        FailAnswer,
        FailTSAnswer,
        ErrorAnswer
      ),
    Update (Update, UnknownUpdate),
    AboutObj (AboutObj, fromId, text),
    Attachment (..),
    Doc (Doc),
    Audio (Audio),
    Photo (Photo),
    Size (..),
    LoadDocResp (LoadDocResp),
    LoadPhotoResp (LoadPhotoResp),
    SavePhotoResp (SavePhotoResp),
    StickerInfo (StickerInfo),
    SaveDocResp (SaveDocResp),
    ResponseSDR (ResponseSDR),
    DocInfo (DocInfo),
    WallInfo (WallInfo),
    SaveDocAuMesResp (SaveDocAuMesResp),
    ResponseSDAMR (ResponseSDAMR),
    GetPollServerJSONBody
      ( GetPollServerJSONBody,
        ErrorAnswerServ
      ),
    ResponseOk (ResponseOk, ErrorAnswerMsg),
    ServerInfo (ServerInfo, tsSI, keySI, serverSI),
    UploadServerResponse (UploadServerResponse),
    UploadUrl (UploadUrl),
    Geo (Geo),
    Coordinates (Coordinates),
  )
where

import Api (optionsEraseSuffix, optionsSnakeCase, optionsSnakeCasePreEraseSuffix)
import Control.Applicative ((<|>), empty, liftA2)
import Data.Aeson ((.:), (.:?), FromJSON (parseJSON), Object, Value (..), genericParseJSON, withObject)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Types
import Vk.Types

data Answer
  = AnswerOk Integer [Update]
  | FailAnswer Integer
  | FailTSAnswer (Maybe Integer) Integer
  | ErrorAnswer Value
  deriving (Generic, Show)

tryReadTs :: Object -> Parser Integer
tryReadTs v =
  (v .: "ts")
    <|> ( do
            tsTxt <- v .: "ts" :: Parser T.Text
            tryReadNum tsTxt
        )

instance FromJSON Answer where
  parseJSON val =
    withObject "AnswerOk" (\v -> AnswerOk <$> tryReadTs v <*> v .: "updates") val
      <|> withObject
        "FailTSAnswer"
        (\v -> FailTSAnswer <$> v .:? "fail" <*> tryReadTs v)
        val
      <|> withObject "FailAnswer" (\v -> FailAnswer <$> v .: "fail") val
      <|> withObject "ErrorAnswer" (\v -> ErrorAnswer <$> v .: "error") val

data Update
  = Update T.Text AboutObj
  | UnknownUpdate Value
  deriving (Show)

instance FromJSON Update where
  parseJSON =
    liftA2
      (<|>)
      (withObject "Update" (\v -> Update <$> v .: "type" <*> v .: "object"))
      (fmap UnknownUpdate . parseJSON)

data AboutObj = AboutObj
  { fromId :: UserId,
    id :: Integer,
    peerId :: Maybe Integer,
    text :: T.Text,
    fwdMessages :: [Value],
    attachments :: [Attachment],
    geo :: Maybe Geo
  }
  deriving (Generic, Show)

instance FromJSON AboutObj where
  parseJSON = genericParseJSON optionsSnakeCase

data Attachment
  = PhotoAttachment Photo
  | DocAttachment Doc
  | AudioMesAttachment Audio
  | VideoAttachment DocInfo
  | StickerAttachment StickerInfo
  | AudioAttachment DocInfo
  | MarketAttachment DocInfo
  | WallAttachment WallInfo
  | PollAttachment DocInfo
  | UnknownAttachment Value
  deriving (Generic, Show)

instance FromJSON Attachment where
  parseJSON (Object v) = do
    txt <- (v .: "type") :: Parser T.Text
    case txt of
      "photo" -> PhotoAttachment <$> v .: "photo"
      "doc" -> DocAttachment <$> v .: "doc"
      "audio_message" -> AudioMesAttachment <$> v .: "audio_message"
      "video" -> VideoAttachment <$> v .: "video"
      "sticker" -> StickerAttachment <$> v .: "sticker"
      "audio" -> AudioAttachment <$> v .: "audio"
      "market" -> MarketAttachment <$> v .: "market"
      "wall" -> WallAttachment <$> v .: "wall"
      "poll" -> PollAttachment <$> v .: "poll"
      _ -> fmap UnknownAttachment . parseJSON $ Object v
  parseJSON v = fmap UnknownAttachment . parseJSON $ v

data Doc = Doc
  { urlD :: T.Text,
    extD :: String,
    titleD :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON Doc where
  parseJSON = genericParseJSON (optionsEraseSuffix "D")

newtype Audio = Audio
  { linkOgg :: T.Text
  }
  deriving (Generic, Eq, Show)

instance FromJSON Audio where
  parseJSON = genericParseJSON optionsSnakeCase

newtype Photo = Photo
  { sizes :: [Size]
  }
  deriving (Generic, Eq, Show, FromJSON)

data Size = Size
  { height :: Integer,
    width :: Integer,
    url :: T.Text
  }
  deriving (Generic, Eq, Show, FromJSON)

newtype LoadDocResp = LoadDocResp
  { file :: String
  }
  deriving (Generic, Show, Eq, FromJSON)

data LoadPhotoResp = LoadPhotoResp
  { server :: Integer,
    hash :: String,
    photo :: String
  }
  deriving (Generic, Show, Eq, FromJSON)

newtype SavePhotoResp = SavePhotoResp
  { responseSPR :: [DocInfo]
  }
  deriving (Generic, Show)

instance FromJSON SavePhotoResp where
  parseJSON = genericParseJSON (optionsEraseSuffix "SPR")

data PhotoInfo = PhotoInfo
  { idPI :: Integer,
    ownerIdPI :: Integer,
    accessKeyPI :: String
  }
  deriving (Generic, Show)

instance FromJSON PhotoInfo where
  parseJSON = genericParseJSON (optionsSnakeCasePreEraseSuffix "PI")

data AudioMesInfo = AudioMesInfo
  { idAMI :: Integer,
    ownerIdAMI :: Integer,
    accessKeyAMI :: String
  }
  deriving (Generic, Show)

instance FromJSON AudioMesInfo where
  parseJSON = genericParseJSON (optionsSnakeCasePreEraseSuffix "AMI")

newtype StickerInfo = StickerInfo
  { stickerId :: StickerId
  }
  deriving (Generic, Eq, Show)

instance FromJSON StickerInfo where
  parseJSON = genericParseJSON optionsSnakeCase

newtype SaveDocResp = SaveDocResp
  { responseSDR :: ResponseSDR
  }
  deriving (Generic, Show)

instance FromJSON SaveDocResp where
  parseJSON = genericParseJSON (optionsEraseSuffix "SDR")

data ResponseSDR = ResponseSDR
  { typeRSDR :: T.Text,
    docRSDR :: DocInfo
  }
  deriving (Generic, Show)

instance FromJSON ResponseSDR where
  parseJSON = genericParseJSON (optionsEraseSuffix "RSDR")

data DocInfo = DocInfo
  { idDI :: Integer,
    ownerIdDI :: Integer
  }
  deriving (Generic, Eq, Show)

instance FromJSON DocInfo where
  parseJSON = genericParseJSON (optionsSnakeCasePreEraseSuffix "DI")

data WallInfo = WallInfo
  { idWI :: Integer,
    fromIdWI :: Integer
  }
  deriving (Generic, Eq, Show)

instance FromJSON WallInfo where
  parseJSON = genericParseJSON (optionsSnakeCasePreEraseSuffix "WI")

newtype SaveDocAuMesResp = SaveDocAuMesResp
  { responseSDAMR :: ResponseSDAMR
  }
  deriving (Generic, Show)

instance FromJSON SaveDocAuMesResp where
  parseJSON = genericParseJSON (optionsEraseSuffix "SDAMR")

data ResponseSDAMR = ResponseSDAMR
  { typeSDAMR :: T.Text,
    audioMessageSDAMR :: DocInfo
  }
  deriving (Generic, Show)

instance FromJSON ResponseSDAMR where
  parseJSON = genericParseJSON (optionsSnakeCasePreEraseSuffix "SDAMR")

data GetPollServerJSONBody
  = GetPollServerJSONBody ServerInfo
  | ErrorAnswerServ Value
  deriving (Generic, Show)

instance FromJSON GetPollServerJSONBody where
  parseJSON =
    liftA2
      (<|>)
      ( withObject "GetPollServerJSONBody" $ \v ->
          GetPollServerJSONBody <$> v .: "response"
      )
      (withObject "ErrorAnswerServ" $ \v -> ErrorAnswerServ <$> v .: "error")

data ServerInfo = ServerInfo
  { keySI :: T.Text,
    serverSI :: T.Text,
    tsSI :: Integer
  }
  deriving (Eq, Generic, Show)

instance FromJSON ServerInfo where
  parseJSON =
    withObject "ServerInfo" $ \v ->
      ServerInfo <$> v .: "key" <*> v .: "server" <*> tryReadTs v

data ResponseOk
  = ResponseOk Integer
  | ErrorAnswerMsg Value
  deriving (Generic, Show)

instance FromJSON ResponseOk where
  parseJSON =
    liftA2
      (<|>)
      (withObject "ResponseOk" $ \v -> ResponseOk <$> v .: "response")
      (withObject "ErrorAnswerMsg" $ \v -> ErrorAnswerMsg <$> v .: "error")

newtype ErrorInfo = ErrorInfo
  { errorCode :: Integer
  }
  deriving (Generic, Show)

instance FromJSON ErrorInfo where
  parseJSON = genericParseJSON optionsSnakeCase

newtype UploadServerResponse = UploadServerResponse
  { responsePSR :: UploadUrl
  }
  deriving (Generic, Show)

instance FromJSON UploadServerResponse where
  parseJSON = genericParseJSON (optionsEraseSuffix "PSR")

newtype UploadUrl = UploadUrl
  { uploadUrl :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON UploadUrl where
  parseJSON = genericParseJSON optionsSnakeCase

data Geo = Geo
  { typeG :: T.Text,
    coordinatesG :: Coordinates
  }
  deriving (Eq, Generic, Show)

instance FromJSON Geo where
  parseJSON = genericParseJSON (optionsEraseSuffix "G")

data Coordinates = Coordinates
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Eq, Generic, Show, FromJSON)

tryReadNum :: T.Text -> Parser Integer
tryReadNum "" = empty
tryReadNum xs = case reads . T.unpack $ xs of
  [(a, "")] -> return a
  _ -> empty
