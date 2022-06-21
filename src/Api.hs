module Api where

import Data.Aeson
  ( Options,
    defaultOptions,
    defaultOptions,
    fieldLabelModifier,
  )
import Data.Char (isUpper, toLower)
import Data.List (isSuffixOf)

optionsSnakeCasePreEraseSuffix :: String -> Options
optionsSnakeCasePreEraseSuffix suffix =
  defaultOptions
    { fieldLabelModifier = fromCamelToSnake . eraseSuffix suffix
    }

optionsSnakeCase :: Options
optionsSnakeCase =
  defaultOptions
    { fieldLabelModifier = fromCamelToSnake
    }

optionsEraseSuffix :: String -> Options
optionsEraseSuffix suffix =
  defaultOptions
    { fieldLabelModifier = eraseSuffix suffix
    }

fromCamelToSnake :: String -> String
fromCamelToSnake = foldr (\x acc -> if isUpper x then '_' : toLower x : acc else x : acc) []

eraseSuffix :: String -> String -> String
eraseSuffix suffix str =
  if suffix `isSuffixOf` str
    then take (length str - length suffix) str
    else str
