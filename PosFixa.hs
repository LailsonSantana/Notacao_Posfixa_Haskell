-- Rogerio Barbosa Chaves
-- Rodrigo Behrmann Pereira
-- Vitor Rayan Farias da Silva
-- Lailson Santana Alves

module PosFixa where
import Stack
import Data.Char(digitToInt,isDigit,intToDigit)
import Data.List

pilha :: Stack String
pilha = empty

--100 200 + 2 / 5 * 7 +

posFixa :: IO()
posFixa = do putStrLn "Digite uma expressao :" 
             txt <- getLine 
             putStrLn ("Resultado : " ++ (top (calculaPosFixa (words txt) pilha)))

calculaPosFixa :: [String] -> Stack String -> Stack String
calculaPosFixa [] p = p
calculaPosFixa (a:b) p  
                      | isNum a == True = calculaPosFixa b p1
                      | otherwise =
                        if a == "+" ||  a == "-" || a == "/" || a == "*"
                          then calculaPosFixa b p4 
                        else error "Expressao Invalida"
                      where
                        operacao = a
                        p1 = push p a
                        x = top p
                        p2 = pop p
                        y = top p2
                        p3 = pop p2
                        z = fazCalculo y x operacao
                        
                        p4 = push p3 (show z) 

-- Realiza o calculo das operações                      
fazCalculo :: String -> String -> String -> Int
fazCalculo op1 op2 operacao
                         | operacao == "+" = read op1 + read op2 
                         | operacao == "-" = read op1 - read op2
                         | operacao == "*" = read op1 * read op2
                         | operacao == "/" = (read op1) `div` (read op2)
                         | otherwise = 1

-- Verifica se uma String é um número
isNum :: String -> Bool
isNum [] = False
isNum [a] = if isDigit a == True then True else False
isNum (a:b)
           | isDigit a == False = False
           | otherwise = isNum b

-- teste
posString :: String -> String
posString str = top (calculaPosFixa (words str) pilha)