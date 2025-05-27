-- identificação
-- nome: Savio de Carvalho Soares
-- Matrícula: 552882
ident = [("Savio de Carvalho Soares", 552882)]


-- atividade 01


-- (1) criar função que dado um número inteiro gere sua fatoração na forma de uma lista de duplas. Por exemplo,


-- 72 = 2^3  .  3^2


-- de onde a lista deve ser,


-- [(2,3),  , (3,2)]


-- construa função com o cabeçalho,

fprimos :: Integer -> [(Integer, Integer)]
fprimos n = factorize n 2
  where
    factorize n p
      | n < 2 = []
      | n `mod` p == 0 = let (q, e) = countDiv n p 0 in (p, e) : factorize q (p + 1)
      | otherwise = factorize n (p + 1)
    countDiv n p e
      | n `mod` p /= 0 = (n, e)
      | otherwise = countDiv (n `div` p) p (e + 1)

main = print $ fprimos 72




-- (2) Seja uma string s da qual se deseja construir a lista das frequências dos caracteses. Cada frequência é uma dupla formada pelo caractere e o total de vezes que ele acontece. Por exemplo,


-- s = "aaabb222"


-- deve gerar a lista,


-- - [('a',3), ('b',2), ('2',3)] 


-- construa função com o cabeçalho,



freq :: String -> [(Char, Int)]
freq s = freqList (sort s)
  where
    sort [] = []
    sort (x:xs) = insert x (sort xs)
    insert x [] = [x]
    insert x (y:ys)
      | x <= y = x : y : ys
      | otherwise = y : insert x ys
    freqList [] = []
    freqList (x:xs) = let (count, rest) = countChar x xs 1 in (x, count) : freqList rest
    countChar x [] n = (n, [])
    countChar x (y:ys) n
      | x == y = countChar x ys (n + 1)
      | otherwise = (n, y:ys)

main = print $ freq "aaabb222"