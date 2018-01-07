import Test.Hspec
import MessageSpec
import NetworkSpec

main :: IO ()
main =
  hspec $ do
    messageSpec
    idSpec
    networkSpec
