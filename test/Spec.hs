{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

import TgTest.Test (testTG)
--import Vk.Test (testVk)

main :: IO ()
main = do
  putStrLn "Test telegram app"
  testTG
  putStrLn "Test vkontakte app"
  --testVk
