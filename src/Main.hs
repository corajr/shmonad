import Shmonad (bashBackend, script2)
import qualified Data.Text.Lazy as L

main :: IO ()
main = putStrLn $ L.unpack (bashBackend script2)
