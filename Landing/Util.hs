module Landing.Util (textToString) where

import qualified Data.Text.Lazy as L

textToString = L.unpack . L.fromStrict
