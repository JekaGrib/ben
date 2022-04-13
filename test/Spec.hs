
import Spec.Tg.App (testTGApp)
import Spec.Vk.App (testVkApp)
import Spec.Vk.PrepareAttachment (testVkPrAtt)

main :: IO ()
main = do
  putStrLn "Test TELEGRAM"
  putStrLn "Test Tg.App.hs"
  testTGApp
  putStrLn "Test Vkontakte"
  putStrLn "Test Vk.App.PrepareAttachment.hs"
  testVkPrAtt
  putStrLn "Test Vk.App.hs"
  testVkApp
