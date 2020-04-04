foreign import ccall unsafe "hello"
  c_hello :: IO ()


main :: IO ()
main = c_hello
