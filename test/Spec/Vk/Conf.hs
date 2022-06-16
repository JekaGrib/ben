module Spec.Vk.Conf where

import qualified Spec.Conf as C
import Vk.Conf (VkConfig (..))

config1 :: VkConfig
config1 =
  VkConfig
    { cConf = C.config1,
      cGroupId = 321
    }
