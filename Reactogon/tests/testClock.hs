import Reactogon.Global.Clock
import Reactogon.Auxiliary.Auxiliary
import Reactogon.Semantics
import FRP.Yampa

main :: IO ()
main = do{-
  putStr "Test tempo: "
  print testTempo'
  putStr "Test onChange': "
  print testonChange'-}
  putStr "Testing metronome: "
  print testMetronome

tempo' :: SF () Tempo
tempo' = switch ((constant 30)
                 &&&
                 (after 20 10)) (\t -> switch ((constant t)
                                               &&&
                                               (after 20 60)) (constant))

testTempo' = embed ((tempo'))
  ((), take 120 $ repeat (1, Nothing))
testonChange' = embed ((discard ^>> tempo' >>> onChange'))
  ((), take 120 $ repeat (1, Nothing))
testMetronome = embed (metronome (tempo'))
  ((), take 120 $ repeat (1, Nothing))
