{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

import           TestTg (testTG)
import           TestVk (testVk)


main :: IO ()
main = do
  putStrLn "Test telegram app"
  testTG
  putStrLn "Test vkontakte app"
  testVk