{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

import           TestTg
import           TestVk


main :: IO ()
main = do
  putStrLn "Test telegram app"
  testTG
  putStrLn "Test vkontakte app"
  testVk