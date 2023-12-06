module Day6 where
-- easiest day so far. Just use the quadratic formula
midnight :: Int -> Int -> Int
midnight t s = 
    let discriminant = sqrt (fromIntegral t ** 2 - 4 * fromIntegral s)
        mini = (floor :: Double -> Int) $ (fromIntegral t - discriminant) / 2 + 1
        maxi = (ceiling :: Double -> Int) $ (fromIntegral t + discriminant) / 2 - 1
    in maxi - mini + 1

solve :: [(Int, Int)] -> Int
solve = product . map (uncurry midnight)

easyLarge :: IO (Maybe Int)
easyLarge = return (Just (solve [(62,553),(64,1010),(91,1473),(90,1074)]))

hardLarge :: IO (Maybe Int)
hardLarge = return (Just (solve [(62649190,553101014731074)]))