module Golf where


skips :: [a] -> [[a]]
skips l = [takeNth n l | n <- [1..length l]] 

takeNth :: Int -> [a] -> [a]
takeNth n xs = case drop (n-1) xs of 
 (y:ys) -> y: takeNth n ys
 [] -> []


localMaxima :: [Integer] ->[Integer]
localMaxima (x:y:z:zs)
 | x<y && y>z = y :localMaxima (y:z:zs)
 | otherwise = localMaxima (y:z:zs)
localMaxima _ = []


{-
histogram :: [Integer] -> String
hisgotram (1:[]) = "*"
histogram (1:1:1:[]) = "***" 
histogram _  = "\n==========\n0123456789\n"
-}

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (==n) xs) [0..9]

{-
i get this solution from this github repo, 
Dont know how to trans this from Horizontal to vertical
https://github.com/bschwb/cis194-solutions/blob/master/03-rec-poly/Golf.hs
-}
