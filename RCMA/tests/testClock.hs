import RCMA.Global.Clock
import RCMA.Auxiliary.Auxiliary
import RCMA.Semantics
import FRP.Yampa

main :: IO ()
main = do
  putStr "Testing metronome: "
  print testMetronome

tempo' :: SF () Tempo
tempo' = switch ((constant 30)
                 &&&
                 (after 20 10)) (\t -> switch ((constant t)
                                               &&&
                                               (after 20 60)) (constant))

testMetronome = embed (metronome (tempo'))
  ((), take 120 $ repeat (1, Nothing))
