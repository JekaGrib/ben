{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           TestTg
import           TestVk


main :: IO ()
main = do
  putStrLn "Test telegram app"
  testTG
  putStrLn "Test vkontakte app"
  testVk