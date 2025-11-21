-- pex6.hs 
-- unKnot Haskell

-- name: Seth Ramsey

{- DOCUMENTATION: I used the class slides and pre-readings to complete this assignment.
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | fst (head tripCode) == fst (head (tail tripCode)) && snd (head tripCode) /= snd (head (tail tripCode)) = removeI tripCode (fst(head tripCode)) []
   | fst (head tripCode) /= fst (head (tail tripCode)) && snd (head tripCode) == snd (head (tail tripCode)) = removeII tripCode (fst(head tripCode)) (fst(head(tail tripCode))) []
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

removeI :: [(Char, Char)] -> Char -> [(Char, Char)] -> String
removeI tripCode x newTrip
   | null tripCode = unKnot newTrip
   | fst(head tripCode) /= x = removeI (tail tripCode) x (newTrip ++[(head tripCode)])
   | otherwise = removeI (tail tripCode) x newTrip

removeII :: [(Char, Char)] -> Char -> Char -> [(Char, Char)] -> String
removeII tripCode x y newTrip
   | null tripCode = unKnot newTrip
   | fst(head tripCode) /= x && fst(head tripCode) /= y = removeII (tail tripCode) x y (newTrip ++[(head tripCode)])
   | otherwise = removeII (tail tripCode) x y newTrip

main :: IO ()
main = do
   -- let t01 = [('a','o'), ('a','u')]
   -- let t01 = [('a','o'), ('a','u'), ('b','u'), ('b','o')]
   -- let t01 = [('a','o'), ('b','o'), ('b','u'), ('a','u')]
   -- let t01 = [('a','o'),('b','o'),('c','o'),('c','u'),('b','u'),('a','u')]
   let t01 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

