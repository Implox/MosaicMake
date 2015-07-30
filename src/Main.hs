import Util.Geometry
import Util.Random (evalRandIO)
import Poisson

toCoord :: Vector -> (Int, Int)
toCoord (Vector x y) = (truncate x, truncate y)

main :: IO ()
main = do
    let cfg = mkConfigForImg (1920, 1080) 15 25
    vecs <- evalRandIO $ generatePoints cfg
    let pts = map toCoord vecs
    putStrLn $ show pts
    putStrLn . show . length $ pts

