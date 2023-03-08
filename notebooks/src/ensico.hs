module ENSICO where

--import Cp
import Data.Char
import Data.List
--import Data.List.Split

--- composition ---

(|>) = flip ($)
(>>) = flip (.)

--- comparison

(=?) = (==)

-----------------

--numBits = length

--numBytes s = div (numBits s) 8

--enviar t = t >>= ascii

--bytesBits = chunksOf 8

--letrasBytes = map (chr . byte2dec)

--receber = map (chr . byte2dec) . chunksOf 8

--ascii = dec2byte . ord

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

collatz n = if n |> even then n`div`2 else 3*n+1
collatz_seq n = if n == 1 then [1] else n : collatz_seq (collatz n)