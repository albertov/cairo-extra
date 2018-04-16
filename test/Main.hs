{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Data.ByteString.Lazy as LBS
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Extra (surfaceWriteToPNGByteString)
import Data.Maybe (isJust)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "can create png" $ do
    bs <- withImageSurface FormatARGB32 100 100 surfaceWriteToPNGByteString
    LBS.stripPrefix "\137PNG\r\n" bs `shouldSatisfy` isJust
