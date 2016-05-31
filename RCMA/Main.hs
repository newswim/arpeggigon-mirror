import FRP.Yampa
import Hails.Yampa
import Reactogon.Semantics
import Reactogon.Translator.Message
import Reactogon.Translator.Translator

-- The whole system is a single SF getting new messages, transforming
-- them and adding some more.
reactogon :: SF [(Frames, RawMessage)] [(Frames, RawMessage)]
reactogon = undefined

main :: IO ()
main = do
  (inp, out) <- yampaReactiveDual [] reactogon
  return ()
