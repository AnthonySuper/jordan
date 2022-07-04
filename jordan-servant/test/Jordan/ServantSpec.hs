import qualified Jordan.Servant.Query.ParseSpec as PS
import qualified Jordan.Servant.Query.RoundtripSpec as RTS
import Test.Hspec

main = hspec $ do
  PS.spec
  RTS.spec
