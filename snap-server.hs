import Network.Wai.Handler.Snap
import Controller

main :: IO ()
main = putStrLn "Loaded" >> withLounge (run 3000)

