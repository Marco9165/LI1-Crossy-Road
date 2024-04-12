{- |
Module      : Tarefa2_2022li1g094
Description : Geração contínua de um mapa
Copyright   : Marco António Fernandes Brito  <a104187 @alunos.uminho.pt>
              Rodrigo da Silva de Sousa <a104528@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}

module Tarefa2_2022li1g094 where

import LI12223
import System.Random
import System.IO.Unsafe

{-| 

A função __estendeMapa__ vai gerar e adicionar uma nova linha válida ao topo de um dado mapa válido. Assim , esta função vai receber um mapa válido e um inteiro (seed) que é um inteiro aleatório (apenas no intervalo [0,100]) esta seed foi usada para dar alguma aleatoriedade à geração das novas linhas.

Para esta função recorremos a várias outras funções auxiliares.

A função ainda utiliza uma seed, que é uma valor inteiro que o gerador vai utilizar para  gerar novos 'Obstaculo's /Terrenos/Velocidades.

A função pode ser definida da seguinte forma:




== Exemplos de utilização

=== Exemplo 1

>>> estendeMapa (Mapa 4 [(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])]) 12
Mapa 4 [(Rio (-1),[Nenhum,Nenhum,Nenhum,Tronco]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])]


-} 


estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ((t,(o:os)):r)) valorRandom  = (Mapa l (completarLinha l (escolherterrenoaleatorio  (Mapa l ((t,(o:os)):r)) valorRandom) valorRandom))
                                              
velocidadeAlea :: Int -> Terreno -> Terreno
velocidadeAlea l (Estrada v) = (Estrada (velocidadeAleaAux l))
velocidadeAlea l (Rio v) = (Rio (velocidadeAleaAux l))
velocidadeAlea l (Relva) = Relva

velocidadeAleaAux :: Int -> Int
velocidadeAleaAux l = unsafePerformIO (getStdRandom (randomR (-l, l)))

completarLinha :: Int -> [(Terreno,[Obstaculo])] -> Int -> [(Terreno,[Obstaculo])]
completarLinha l ((t,(o:os)):r) valorRandom | l > length (o:os) + 1 = completarLinha l ([(velocidadeAlea l t,(o:os)++[proximosObstaculosValidos l (t,(o:os)) !! (escolherNumero $ terrenoaleatorio (proximosObstaculosValidos l (t, (o:os))) valorRandom)])]++r) (valorRandom-((velocidadeAleaAux l)^2))
                                            | otherwise = [(t,(o:os)++[proximosObstaculosValidos l (t, (o:os)) !! (escolherNumero $ terrenoaleatorio (proximosObstaculosValidos l (t, (o:os))) valorRandom)])] ++ r

escolherterrenoaleatorio :: Mapa -> Int -> [(Terreno,[Obstaculo])]
escolherterrenoaleatorio (Mapa l ((t,(o:os)):r)) valorRandom = [(((proximosTerrenosValidos (Mapa l ((t,(o:os)):r))) !! (escolherNumero $ terrenoaleatorio (proximosTerrenosValidos (Mapa l ((t,(o:os)):r))) valorRandom)),[proximosObstaculosValidos l (((proximosTerrenosValidos (Mapa l ((t,(o:os)):r))) !! (escolherNumero $ terrenoaleatorio (proximosTerrenosValidos (Mapa l ((t,(o:os)):r))) valorRandom)), []) !! (escolherNumero $ terrenoaleatorio (proximosObstaculosValidos l (((proximosTerrenosValidos (Mapa l ((t,(o:os)):r))) !! (escolherNumero $ terrenoaleatorio (proximosTerrenosValidos (Mapa l ((t,(o:os)):r))) valorRandom)), [])) valorRandom)])] ++ ((t,(o:os)):r)
           

escolher :: [a] -> Int -> a
escolher (t:ts) valorRandom = (t:ts) !! (escolherNumero $ terrenoaleatorio (t:ts) valorRandom)


terrenoaleatorio :: [a] -> Int -> [Int]     
terrenoaleatorio (t:ts) valorRandom = take(length(t:ts)) $ randoms (mkStdGen valorRandom) 

escolherNumero :: [Int] -> Int
escolherNumero (h:t) = escolherNumeroAux 0 0 h t
             where escolherNumeroAux i im _ [] = im
                   escolherNumeroAux i im x (y:ys) | x < y = escolherNumeroAux (i+1) (i+1) y ys
                                                   | otherwise = escolherNumeroAux (i+1) im x ys

{-|

Função Auxiliar proximosTerrenosValidos: 

== Explicação da função proximosTerrenosValidos:

   A função auxiliar 'proximosTerrenosValidos' recebe um Mapa , composto por uma lista de Terrrenos e por sua vez listas de Obstaculos , e entrega uma lista Terrenos que podem ser adicionados ao Mapa tendo em conta o conteúdo do mesmo.

== Exemplos de utilização:

>>> proximosTerrenosValidos (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])])) 
[Relva ,Estrada 0 , Rio 0]

>>> proximosTerrenosValidos (Mapa 3 [(Rio 1 ,[Nenhum,Tronco,Nenhum]),(Rio (-1) ,[Nenhum,Tronco,Nenhum]),(Rio 2 , [Tronco,Nenhum,Tronco]),(Rio (-2),[Nenhum,Tronco,Nenhum])]))
[Relva ,Estrada 0]
-}

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa l ((Rio _,(o1:os1)):(Rio _,(o2:os2)):(Rio _,(o3:os3)):r)) | r == [] = [Estrada 0, Relva]
                                                                                                         | otherwise = proximosTerrenosValidos (Mapa l r)
proximosTerrenosValidos (Mapa l ((Estrada _,(o1:os1)):(Estrada _,(o2:os2)):(Estrada _,(o3:os3)):(Estrada _,(o5:os5)):r)) | r == [] = [Rio 0,Relva]
                                                                                                                                      | otherwise = proximosTerrenosValidos (Mapa l r)
proximosTerrenosValidos (Mapa l ((Relva,(o1:os1)):(Relva,(o2:os2)):(Relva,(o3:os3)):(Relva,(o4:os4)):r)) | r == [] = [Rio 0, Estrada 0]
                                                                                                         | otherwise = proximosTerrenosValidos (Mapa l r)
proximosTerrenosValidos (Mapa l ((_,(o:os)):r)) = proximosTerrenosValidos (Mapa l r)

{-|

Função Auxiliar proximosObstaculosValidos: 

== Explicação da função proximosObstaculosValidos:

   A função auxiliar 'proximosObstaculosValidos' recebe um Inteiro , que corresponde à largura do Mapa , e um Terreno composto por uma lista de Obstaculos, e entrega uma lista de Obstáculos possíveis de adiconar à lista de Obstaculos do Terreno em questão.

== Exemplos de utilização:

>>> proximosObstaculosValidos 4 (Estrada 2 , [Nenhum,Carro,Nenhum])
[Nenhum,Carro]

>>> proximosObstaculosValidos 4 (Relva , [Arvore,Arvore,Arvore])
[Nenhum]

-}

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (Estrada x, []) =  [Nenhum, Carro]
proximosObstaculosValidos l (Estrada x, (o:os)) | l == length (o:os) = []
                                                | not(elem Nenhum (o:os)) && (l - (length (o:os))) > 1  = [Nenhum, Carro] 
                                                | not(elem Nenhum (o:os)) && (length (o:os)) < l = [Nenhum]                            
                                                | not(elem Nenhum (drop (length (o:os) - 3) (o:os))) && l == 1 + length(o:os) = [Nenhum] 
                                                | not(elem Nenhum (o:os)) = [Nenhum, Carro]
                                                | Carro == head(head(groupaux(o:os))) && Carro == head(last(groupaux(o:os))) && (length(last(groupaux(o:os))))+length(head(groupaux (o:os))) >= 3 && (l - length(o:os)) > 1 = [Nenhum, Carro]
                                                | Carro == head(head(groupaux(o:os))) && Carro == head(last(groupaux(o:os))) && (length(last(groupaux(o:os))))+length(head(groupaux (o:os))) >= 3 = [Nenhum]
                                                | otherwise = [Nenhum, Carro]

proximosObstaculosValidos l (Rio x, []) =  [Nenhum, Tronco]
proximosObstaculosValidos l (Rio x, (o:os)) | l == length (o:os) = []
                                            | Tronco == head(head(groupaux(o:os))) && Tronco == head(last(groupaux(o:os))) && (length(last(groupaux(o:os))))+length(head(groupaux (o:os))) >= 5 = [Nenhum]
                                            | Tronco == head(head(groupaux(o:os))) && Tronco == head(last(groupaux(o:os))) && (length(last(groupaux(o:os))))+length(head(groupaux (o:os))) >= 5 && (l - length(o:os)) > 1  = [Nenhum, Tronco]
                                            | not(elem Nenhum (o:os)) && (l - (length (o:os))) > 1  = [Nenhum, Tronco] 
                                            | not(elem Nenhum (o:os)) && (length (o:os)) < l = [Nenhum] 
                                            | not(elem Nenhum (o:os)) = [Nenhum, Tronco]
                                            | not(elem Nenhum (drop (length (o:os) - 5) (o:os))) && l == 1 + length(o:os) = [Nenhum] 
                                            | not(elem Tronco (o:os)) && (l - length (o:os)) == 1 = [Tronco]                        
                                            | otherwise = [Nenhum, Tronco]

proximosObstaculosValidos l (Relva, []) =  [Nenhum, Arvore]
proximosObstaculosValidos l (Relva, (o:os)) | l == length (o:os) = []
                                            | not(elem Nenhum (o:os)) && (l - (length (o:os))) > 1  = [Nenhum, Arvore]
                                            | not(elem Nenhum (o:os)) && (length (o:os)) < l = [Nenhum] 
                                            | not(elem Nenhum (drop (length (o:os) - 5) (o:os))) && l == 1 + length(o:os) = [Nenhum]
                                            | not(elem Nenhum (o:os)) = [Nenhum, Arvore]
                                            | otherwise = [Nenhum, Arvore]

groupaux :: Eq a => [a] -> [[a]]
groupaux [] = []
groupaux [x] = [[x]]
groupaux (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = groupaux t
