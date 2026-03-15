-- Program that accepts five student records
-- and sorts them in ascending order by grade

sort5 :: [(String, Int)] -> [(String, Int)]
sort5 [a,b,c,d,e] =
    insert a (insert b (insert c (insert d (insert e []))))

insert :: (String, Int) -> [(String, Int)] -> [(String, Int)]
insert x [] = [x]
insert x@(nameX, gradeX) (y@(nameY, gradeY):ys)
    | gradeX <= gradeY = x : y : ys
    | otherwise        = y : insert x ys


-- Example usage
main :: IO ()
main = do
    let students =
            [ ("Alpha",   99)
            , ("Bravo",   87)
            , ("Charlie", 91)
            , ("Delta", 93)
            , ("Echo",  79)
            ]

    print (sort5 students)