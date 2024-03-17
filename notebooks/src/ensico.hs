module ENSICO where

--import Cp
import Data.Char
import Data.List

-- import Data.List.Split

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n x = take n x : chunksOf n (drop n x)

--- composition ---

(|>) = flip ($)
(>>) = flip (.)

--- comparison

(=?) = (==)

-- Basic -----------------------------------

(f `is` v) x = (f x) == v

(f `belongs` v) x = (f x) `elem` v

(f `isnot` v) x = (f x) /= v

(f `_or_` g) x = (f x) || (g x)

-- pairing

swap(a,b) = (b,a)

split f g x = (f x, g x)

pair f g x = (f x, g x)

(f >< g)(a,b) = (f a,g b)

a |-> b = (a,b)

assocl(a,(b,c)) = ((a,b),c)

-- lists

singl x = [x]

cincat s = concat . intersperse s

x .><. y = [(a,b) | a <- x, b <- y]  -- Cartesian product

converse = map swap

discollect = (>>=lstr)

-- set = sort . nub 

collect :: (Ord a, Ord b) => [(b, a)] -> [(b, [a])]
collect x = nub [ k |-> [ d' | (k',d') <- x , k'==k ] | (k,_) <- x ]

conc = uncurry (++)

shrink x = nub [ k |-> minimum [ d' | (k',d') <- x , k'==k ] | (k,d) <- x ]

discard = filter . (not.)

lstr(b,x) = [ (b,a) | a <- x ]

-----------------

numBits = length

numBytes s = div (numBits s) 8

enviar t = t >>= ascii

bytesBits = chunksOf 8

letrasBytes = map (chr . byte2dec)

receber = map (chr . byte2dec) . chunksOf 8

ascii = dec2byte . ord

----- ASCII order to standard order -----

posicao l = ord(l) - 64
alfabeto n = chr(n+64)


--dec2bin :: Int -> [Int]
--dec2bin = dec2byte

dec2bin 0 = [0]
dec2bin n = dec2bin m ++ [b] where (m,b) = (div n 2, mod n 2)

dec2byte :: Int -> [Int]
dec2byte = reverse . take 8 . (++zeros) . reverse . dec2bin where zeros = 0:zeros

byte2dec xs = sum (map (uncurry (*)) (zip (reverse xs) [ 2^i | i <- [0..length xs] ]))

bitflip i [] = []
bitflip i x = take (i-1) x ++ aux i (drop (i-1) x) where
   aux i [] = []
   aux i (a:x) = (1-a) : bitflip i x

--byte2dec [a] = a
--byte2dec b   = byte2dec(init b) * 2 + last b

--- Functional Programming, part II

help x = splitAt (div (length x) 2) x

divide = help

p1 = fst . divide
p2 = snd . divide

--- Auxiliary

pp = putStr

-- esconder:

--inL = either nil cons

--anaL h = inL . (id -|- (id >< (anaL h))) . h

--f 1 = i1 ()
--f n = i2 (p, n `div` p) where p = head [x | x <- [2..n], n `mod` x == 0]

--primos = anaL f -- dá a lista de fatores primos de um número (com multiplicidade)

--g' 1 = i1 ()
--g' n = i2 ((p, n `div` p), n `div` p) where p = head [x | x <- [2..n], n `mod` x == 0]

--arv = init . (anaL g') -- dá os nós da árvore sob a forma de uma sequência de pares

--conta x l = length (filter (== x) l)

--freq' l = [(x, conta x l) | x <- nub l]

--freq = (map p2) . freq'

-- esconder:

diferencas [a] = []
diferencas (a:x) = ((head x) - a) : diferencas x

-- esconder:

progressão x [] = [x]
progressão x (h:t) = x : (progressão n t) where n = x+h

e_termo n seq = n `elem` takeWhile (<=n) seq

---

--letraParaNum = ord >> (+(-64)) -- converte uma letra num número entre 1 e 26
letraParaNum n
    | x `elem` [65..90] = x - 64
    | x `elem` [97..122] = x - 96
    | otherwise = x
    where x = ord n

--novoNum n = if (n `mod` 26 == 0) then 26 else (n `mod` 26) -- coloca um número qq no intervalo [1..26]
novoNum n
    | x == 0 = 26
    | otherwise = x
    where x = n `mod` 26

numParaLetra = novoNum ENSICO.>> (+64) ENSICO.>> chr -- converte um número entre 1 e 26 numa letra
    
numeros = map letraParaNum ENSICO.>> concatMap show -- parte numérica do nome futurista sob a forma de String

somaLetras l1 l2 = ((letraParaNum l1) + (letraParaNum l2)) |> numParaLetra -- soma duas letras

futuro nome = [somaLetras h l] ++ "-" ++ (nome |> numeros) -- programa completo
    where h = head nome
          l = last nome

--- collatz

takeUntil p [] = []
takeUntil p (x:xs) = if p x then [x] else x : (takeUntil p xs) -- assume que existe algum elemento na lista satisfazendo p

collatz n = if n |> even then n |> (`div` 2) else n |> (*3) |> (+1)

collatzSeq n = n |> iterate collatz |> takeUntil (==1)

--- y2

inverte = reverse
ordena = sort
elimina_repetidos = nub
soma = sum
conta = length
quantos = length
maximo = maximum
máximo = maximum

--- mixordia

letters = filter (\l -> elem l (['a'..'z']++['A'..'Z']))
divProp n = [d | d <- [1..(n-1)], n `mod` d == 0]    

--- merge

merge (x,[]) = x
merge ([],y) = y
merge (a:x,b:y) = if (a <= b) then a:(merge (x, (b:y))) else b:(merge ((a:x), y))

a <++> b = merge(a,b)  -- curried

--- loops

while a b x =
   if a x
   then while a b (b x)
   else x

for b i 0 = i
for b i n = b (for b i (n-1))

--- caesar 

suc 'Z' = 'A'
suc ' ' = ' '
suc c = chr(ord c+1)

unsuc 'A' = 'Z'
unsuc ' ' = ' '
unsuc c = chr(ord c-1)

avanca_um = map suc
recua_um = map unsuc

decifra = flip (-) 3

--- digital comm

codigo t = t >>= ascii

--ascii = ord >> dec2byte

--dec2byte = dec2bin >> reverse >> (++zeros) >> take 8 >> reverse where zeros = 0:zeros

--dec2bin 0 = [0]
--dec2bin n = dec2bin m ++ [b] where (m,b) = (div n 2, mod n 2)

comprimento = length
divide_por = flip div

mensagem = chunksOf 8 ENSICO.>> map (byte2dec ENSICO.>> chr)


--- images

troca 0 = 1
troca 1 = 0

--- cripto

cesar = suc . suc . suc
uncesar = unsuc . unsuc . unsuc

cifraCesar = map cesar
decifraCesar = map uncesar

transfAZ ' ' = ' '
transfAZ c = chr (f n) where
   n = ord c
   f n = ord 'Z' - (n - ord 'A')
   
cifraAZ = map transfAZ
decifraAZ = cifraAZ

---

a # b = a `zip` b

agrupa = collect

--- loops

(.@) f 0 = id
(.@) f n = f . (f .@ (n-1))

--- lookups

mT :: Eq a => [(a, b)] -> a -> Maybe b
mT = flip lookup

pap :: Eq a => [(a, t)] -> a -> t
pap m = unJust . (mT m) where unJust (Just a) = a -- partial inspector of simple relation A->B

lkp :: Eq a => [(a, t)] -> a -> t
lkp = pap
