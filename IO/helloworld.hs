import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "which color for this number" ++ show a
        color <- getLine
        return color)
    
    putStrLn "color picked are"
    mapM putStrLn colors

