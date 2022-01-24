{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.Api.Response where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T
import           Control.Applicative         
import Data.Aeson.Types (Parser)
import Data.Foldable (asum)


data Answer
    = AnswerOk     { ts      :: T.Text,
                     updates :: [Update] }
    | FailAnswer   { fail'   :: Integer }               
    | FailTSAnswer { fail''  :: Integer,
                     ts''    :: Integer }  
    | ErrorAnswer  { error' :: Value } deriving (Generic, Show)
         
instance FromJSON Answer where
  parseJSON val = 
      (withObject "AnswerOk" (\v -> AnswerOk <$> v .: "ts" <*> v .: "updates")) val <|>
      (withObject "FailTSAnswer" (\v -> FailTSAnswer <$> v .: "fail" <*> v .: "ts")) val <|>
      (withObject "FailAnswer" (\v -> FailAnswer <$> v .: "fail")) val <|>
      (withObject "ErrorAnswer" (\v -> ErrorAnswer <$> v .: "error")) val 


data Update 
  = Update {typeUpd :: T.Text,
            objectUpd  :: AboutObj
            } 
    | UnknownUpdate Value 
       deriving ( Show)

instance FromJSON Update where
  parseJSON = 
    liftA2 
      (<|>)
        (withObject "Update" (\v -> Update <$> v .: "type" <*> v .: "object"))
        (fmap UnknownUpdate . parseJSON )


data AboutObj = AboutObj {
      from_id  :: Integer
    , id  :: Integer
    , peer_id  :: Integer
    , text  :: T.Text
    , fwd_messages :: [Object]
    , attachments :: [Attachment]
    , geo :: Maybe Geo
    } deriving (Generic, Show)

instance FromJSON AboutObj

data Attachment 
    = PhotoAttachment 
      { type' :: T.Text
      , photoPA :: Photo }
    | DocAttachment
      { type' :: T.Text
      , docDA :: Doc }
    | AudioMesAttachment
      { type' :: T.Text
      , audio_message :: Audio }
    | VideoAttachment
      { type' :: T.Text
      , docVA :: DocInfo }
    | StickerAttachment
      { type' :: T.Text
      , sticker :: StickerInfo }
    | AudioAttachment
      { type' :: T.Text
      , audio :: DocInfo }
    | MarketAttachment
      { type' :: T.Text
      , market :: DocInfo }
    | WallAttachment
      { type' :: T.Text
      , wall :: WallInfo }
    | PollAttachment
      { type' :: T.Text
      , poll :: DocInfo }
    | UnknownAttachment Value 
     deriving (Generic, Show)

instance FromJSON Attachment where
    parseJSON v = asum [parsePhotoAtt v,parseAudioMesAtt v,parseVideoAtt v,parseStickerAtt v,parseAudioAtt v,parseMarketAtt v,parseWallAtt v,parsePollAtt v,parseUnknownAtt v]

parsePhotoAtt, parseAudioMesAtt, parseVideoAtt, parseStickerAtt, parseAudioAtt, parseMarketAtt, parseWallAtt, parsePollAtt, parseUnknownAtt :: Value -> Parser Attachment
parsePhotoAtt =  withObject "PhotoAttachment" $ \v -> PhotoAttachment
        <$> v .: "type"
        <*> v .: "photo"

parseAudioMesAtt = withObject "DocAttachment" $ \v -> DocAttachment
        <$> v .: "type"
        <*> v .: "doc"

parseVideoAtt = withObject "VideoAttachment" $ \v -> VideoAttachment
        <$> v .: "type"
        <*> v .: "video"

parseStickerAtt = withObject "StickerAttachment" $ \v -> StickerAttachment
        <$> v .: "type"
        <*> v .: "sticker"

parseAudioAtt = withObject "AudioAttachment" $ \v -> AudioAttachment
        <$> v .: "type"
        <*> v .: "audio"

parseMarketAtt = withObject "MarketAttachment" $ \v -> MarketAttachment
        <$> v .: "type"
        <*> v .: "market"

parseWallAtt = withObject "WallAttachment" $ \v -> WallAttachment
        <$> v .: "type"
        <*> v .: "wall"

parsePollAtt = withObject "PollAttachment" $ \v -> PollAttachment
        <$> v .: "type"
        <*> v .: "poll"

parseUnknownAtt = fmap UnknownAttachment . parseJSON 

data Doc 
    = Doc{
        urlD   :: T.Text
       , ext   :: String
       , title :: String
    } deriving (Show)

instance FromJSON Doc where
    parseJSON = 
      withObject "Doc" $ \v -> Doc
        <$> v .: "url"
        <*> v .: "ext"
        <*> v .: "title"

data Audio = Audio {
      link_ogg :: T.Text
    } deriving (Generic, Show)

instance FromJSON Audio

data Photo = Photo {
      sizes :: [Size]
    } deriving (Generic, Show)

instance FromJSON Photo

data Size = Size {
      height :: Integer
    , width :: Integer
    , url :: T.Text
    } deriving (Generic, Show)

instance FromJSON Size

data LoadDocResp = LoadDocResp {
      file :: String
    } deriving (Generic, Show)

instance FromJSON LoadDocResp

data LoadPhotoResp = LoadPhotoResp {
      server :: Integer
    , hash :: String
    , photo :: String
    } deriving (Generic, Show)

instance FromJSON LoadPhotoResp

data SavePhotoResp = SavePhotoResp {responseSPR :: [DocInfo]} deriving (Generic, Show)

instance FromJSON SavePhotoResp where
    parseJSON = withObject "SavePhotoResp" $ \v -> SavePhotoResp
        <$> v .: "response"

data PhotoInfo = PhotoInfo {
      idPI :: Integer
    , owner_id :: Integer
    , access_key :: String
    } deriving (Generic, Show)

instance FromJSON PhotoInfo where
      parseJSON = withObject "PhotoInfo" $ \v -> PhotoInfo
        <$> v .: "id"
        <*> v .: "owner_id"
        <*> v .: "access_key"

data AudioMesInfo = AudioMesInfo {
      idAMI :: Integer
    , owner_idAMI :: Integer
    , access_keyAMI :: String
    } deriving (Generic, Show)

instance FromJSON AudioMesInfo where
      parseJSON = withObject "AudioMesInfo" $ \v -> AudioMesInfo
        <$> v .: "id"
        <*> v .: "owner_id"
        <*> v .: "access_key"

data StickerInfo = StickerInfo {
      sticker_id :: Integer
    } deriving (Generic, Show)

instance FromJSON StickerInfo

data SaveDocResp = SaveDocResp {responseSDR :: ResponseSDR} deriving (Generic, Show)

instance FromJSON SaveDocResp where
    parseJSON = withObject "SaveDocResp" $ \v -> SaveDocResp
        <$> v .: "response"

data ResponseSDR = ResponseSDR {
      typeRSDR :: T.Text
    , docRSDR  :: DocInfo
    } deriving (Generic, Show)

instance FromJSON ResponseSDR where
    parseJSON = withObject "ResponseSDR" $ \v -> ResponseSDR
        <$> v .: "type"
        <*> v .: "doc"

data DocInfo = DocInfo {
      idDI :: Integer
    , owner_idDI :: Integer
    } deriving (Generic, Show)

instance FromJSON DocInfo where
      parseJSON =  withObject "DocInfo" $ \v -> DocInfo
        <$> v .: "id"
        <*> v .: "owner_id"

data WallInfo = WallInfo {
      idWI :: Integer
    , from_idWI :: Integer
    } deriving (Generic, Show)

instance FromJSON WallInfo where
      parseJSON = withObject "WallInfo" $ \v -> WallInfo
        <$> v .: "id"
        <*> v .: "from_id"

data SaveDocAuMesResp = SaveDocAuMesResp {responseSDAMR :: ResponseSDAMR} deriving (Generic, Show)

instance FromJSON SaveDocAuMesResp where
    parseJSON = withObject "SaveDocAuMesResp" $ \v -> SaveDocAuMesResp
        <$> v .: "response"

data ResponseSDAMR = ResponseSDAMR {
      typeSDAMR :: T.Text
    , docSDAMR  :: DocInfo
    } deriving (Generic, Show)

instance FromJSON ResponseSDAMR where
    parseJSON = withObject "ResponseSDAMR" $ \v -> ResponseSDAMR
        <$> v .: "type"
        <*> v .: "audio_message"





data GetPollServerJSONBody 
    = GetPollServerJSONBody { response :: ServerInfo} 
    | ErrorAnswerServ  { error'' :: Object } deriving (Generic, Show)

instance FromJSON GetPollServerJSONBody where
  parseJSON = 
    liftA2
      (<|>)
      (withObject "GetPollServerJSONBody" $ \v -> GetPollServerJSONBody <$> v .: "response")
      (withObject "ErrorAnswerServ" $ \v -> ErrorAnswerServ <$> v .: "error")


data ServerInfo 
    = ServerInfo { key :: T.Text,
                   serverSI  :: T.Text,
                   tsSI  :: T.Text} deriving (Generic, Show)


instance FromJSON ServerInfo where
    parseJSON = withObject "ServerInfo" $ \v -> ServerInfo
        <$> v .: "key"
        <*> v .: "server"
        <*> v .: "ts" 

data Response 
    = Response { response' :: Integer }
    | ErrorAnswerMsg  { error''' :: Value } deriving (Generic, Show)

instance FromJSON Response where
  parseJSON  = 
    liftA2
      (<|>) 
      (withObject "Response" $ \v -> Response <$> v .: "response")
      (withObject "ErrorAnswerMsg" $ \v -> ErrorAnswerMsg <$> v .: "error")

data ErrorInfo = ErrorInfo { error_code :: Integer} deriving (Generic, Show)

instance FromJSON ErrorInfo

data UploadServerResponse = UploadServerResponse {responsePSR :: UploadUrl} deriving (Generic, Show)

instance FromJSON UploadServerResponse where
    parseJSON = withObject "UploadServerResponse" $ \v -> UploadServerResponse 
      <$> v .: "response"

data UploadUrl = UploadUrl {upload_url :: T.Text} deriving (Generic, Show)

instance FromJSON UploadUrl

data Geo = Geo {
      typeG :: T.Text
    , coordinates  :: Coordinates
    } deriving (Eq, Generic, Show)

instance FromJSON Geo where
    parseJSON = withObject "Geo" $ \v -> Geo
        <$> v .: "type"
        <*> v .: "coordinates"

data Coordinates = Coordinates {
      latitude :: Double
    , longitude  :: Double
    } deriving (Eq, Generic, Show)

instance FromJSON Coordinates 