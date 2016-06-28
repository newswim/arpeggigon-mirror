import RMCA.Auxiliary.Auxiliary
import FRP.Yampa

main :: IO ()
main = do
  putStr "Testing onChange: "
  print testOnChange
  putStr "Testing onChange': "
  print testOnChange'

testOnChange =
  embed onChange (1, [(1, Just 1), (1, Just 1), (1, Just 2), (1, Just 3), (1, Just 3)])

testOnChange' =
  embed onChange' (1, [(1, Just 1), (1, Just 1), (1, Just 2), (1, Just 3), (1, Just 3)])
