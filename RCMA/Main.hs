import FRP.Yampa
import Hails.Yampa
import RCMA.Semantics
import RCMA.Translator.Message
import RCMA.Translator.Translator

-- The whole system is a single SF getting new messages, transforming
-- them and adding some more.
rcma :: SF [(Frames, RawMessage)] [(Frames, RawMessage)]
rcma = undefined

main :: IO ()
main = do
  (inp, out) <- yampaReactiveDual [] rcma
  return ()
