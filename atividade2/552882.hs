-- identificação
-- nome: Savio de Carvalho Soares
-- Matrícula: 552882
ident = [("Savio de Carvalho Soares", 552882)]

-- Fundir dois vetores ordenados num vetor ordenado maior.
-- use casamento de padrões.
-- não use meios externos de ordenação.
-- use recursão.

merge :: Ord a => [a] -> [a] -> [a]
merge [] v = v
merge u [] = u
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- implemente mergesort para 
-- ordenação do vetor u.
--   Use a função anterior.

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort u = 
    let (metade1, metade2) = splitAt (length u `div` 2) u
    in merge (mergesort metade1) (mergesort metade2)

-- usando fold implementar função que retorne 
-- a série de Fibonacci com n elementos.

fibo'list :: Int -> [Integer]
fibo'list n = take n $ foldr (\_ (a:b:resto) -> (a+b):a:b:resto) [1,0] [1..n]
