{- |
Module      : Tarefa3_2022li1g094
Description : Movimentação do personagem e obstáculos
Copyright   : Marco António Fernandes Brito  <a104187 @alunos.uminho.pt>
              Rodrigo da Silva de Sousa <a104528@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}

module Tarefa3_2022li1g094 where

import LI12223
import Data.List
import Data.Maybe


{- |

== Explicação da função movimentaVelObs:

   Nesta função, observamos se a velocidade é menor do que 0 e se o Int fornecido é maior que que 0, que servirá de acumulador,
   para fazer V vezes o movimento dos obstaculos.

   Caso a velocidade seja negativa, os obstáculos andam da direita para a esquerda, ou seja, a head da lista, ficará em ultimo e e os obstaculos avançam 1 unidade para a esquerda.

   Caso a velocidade seja positiva, os obstáculos andam da esquerda para a direita, ou seja, o último obstáculo da lista será a head da lista, e os obstaculos avançam 1 unidade para a direita.
  
    Caso não aconteça nenhuma das condições, entregará a mesma lista de obstáculos.

== Exemplos de utilização:
>>> movimentaVelObs 3 [Tronco,Tronco,Nenhum] 2
[Nenhum,Tronco,Tronco]
-}


movimentaVelObs ::Velocidade->[Obstaculo]-> Int ->[Obstaculo]
movimentaVelObs v (obst:obst') n  | v < 0 && n > v = movimentaVelObs v (obst' ++ [obst]) (n-1)
                                  | v > 0 && n < v = movimentaVelObs v (last obst' : obst : init obst') (n+1)
                                  | otherwise = (obst:obst')

{- |
== Explicação da função movimentaVelocidade:

   Nesta função 'movimentaVelocidade, observamos se o Terreno é um Rio,Estrada, ou Rio

   Caso seja uma Estrada ou Rio, a mesma fará a função 'movimentaVelObs' V vezes.

   Caso seja uma relva, dará a mesma lista, pois a Relva não apresenta velocidade.


== Exemplos de utilização:
>>> movimentaVelocidade (Estrada 2, [Carro,Carro,Nenhum])
[(Estrada 2,[Carro,Nenhum,Carro])]
-}

movimentaVelocidade :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
movimentaVelocidade [] = []
movimentaVelocidade ((Rio v,(obs:obst')):t) = (Rio v, movimentaVelObs v (obs:obst') 0) : movimentaVelocidade t
movimentaVelocidade ((Estrada v,(obs:obst')):t) = (Estrada v , movimentaVelObs v (obs:obst') 0) : movimentaVelocidade t
movimentaVelocidade ((Relva,o):t) = (Relva,o) : movimentaVelocidade t


{- |
1ª função auxiliar: 

== Explicação da função localizarTerr:

   A função auxiliar 'localizarTerr' recebe uma lista de pares de terrenos, lista de obstaculos e um inteiro, e entrega um par de terrenos, lista de terrenos.

   Dada uma lista de pares de terrenos, lista de obstaculos e um inteiro, a mesma irá 'entregar' a linha correspondente ao Int fornecido, 
   ou seja, a linha (terreno,[Obstaculos]), em que o personagem se encontra.

== Exemplos de utilização:

>>> localizarTerr (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])])) 1 
(Estrada 2 ,[Carro,Nenhum,Carro])

-}

localizarTerr :: [(Terreno,[Obstaculo])] -> Int -> (Terreno,[Obstaculo])
localizarTerr terObs y = terObs !! y


{- |
2ª função auxiliar 

== Explicação da função verObst:

   A função auxiliar 'verObst' recebe um Jogo, e entrega um par de terrenos, lista de terrenos.

   Dada um Jogo, irá entregar,no primeiro elemento do par, o primeiro elemento do par Terreno, Obstaculo, da linha em que o personagem se encontra, isto é, o Terreno que se encontra.

   De seguida, irá entregar,no segundo elemento do par, o obstáculo em que o personagem se encontra.

== Exemplos de utilização:

>>> verObst (Jogo (Jogador(1,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])]))
(Rio 2,Nenhum)
-}

verObst :: Jogo -> (Terreno,Obstaculo)
verObst (Jogo (Jogador (x,y)) (Mapa l b@((t,(h:hs)):ts))) = ((fst (localizarTerr b y),(snd (localizarTerr b y) !! x)))


{- |
3ª função auxiliar 

== Explicação da função listTerr:

   A função auxiliar 'listTerr' recebe uma lista de pares de terrenos, lista de obstaculos e entrega uma lista de Terrenos.

   Dado uma determinada lista, a função colocará numa só lista, todos os terrenos que aparecem na mesma.

== Exemplos de utilização:

>>> listTerr [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])])
[Relva,Estrada 2,Rio 2]

-}

listTerr :: [(Terreno,[Obstaculo])] -> [Terreno]
listTerr [] = []
listTerr ((t,(obst:obst')):ts) = t : listTerr ts


{- |
4ª função auxiliar 

== Explicação da função foraMapa:

   A função auxiliar 'foraMapa' recebe um Jogo e entrega um Boleano.
   
   Caso a abscissa x , das coordenadas do jogador, seja maior que a largura do mapa -1, está fora do Mapa. A explicação deste (-1), é porque as linhas e colunas começam por 0, 
   a sua posição.

   Caso a abscissa x, das coordenadas do jogador, seja menor que 0, estará fora do Mapa, pois caso a personagem estiver em cima de um tronco e passar a "parede" esquerda 
   do mapa, o jogo irá terminar.
   
   Caso a ordenada y, das coordenadas do jogador, seja menor que 0, estará fora do Mapa, pois caso a personagem estiver em cima de um tronco e passar a "parede" direita 
   do mapa, o jogo irá terminar.

   Caso a ordenada y, das coordenadas do jogador, seja maior que o comprimento da lista de terrenos -1, estará fora do Mapa. A explicação deste (-1), é porque as linhas e colunas começam por 0, 
   a sua posição.

== Exemplos de utilização:

>>> foraMapa (Jogo (Jogador (3,1))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco])]))
False
>>> foraMapa (Jogo (Jogador (4,1))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco])]))
True
>>> foraMapa (Jogo(Jogador (3,2))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco])]))
True
-}

foraMapa :: Jogo -> Bool
foraMapa  (Jogo (Jogador (x,y)) (Mapa l b@((t,(h:hs)):ts))) | x > (l-1) = True
                                                            | x < 0 = True
                                                            | y < 0 = True
                                                            | y > ((length (listTerr b))-1) = True
                                                            | otherwise= False

{- |
5ª função auxiliar 

== Explicação da função observarTerrenos:

   A função auxiliar 'observarTerrenos' recebe um par de Terreno,Obstaculo.

   Dado um determinado par Terreno, Obstaculo, que será o par dado pela função 'verObst', irá entregar um '1', caso receba um Rio.

   Caso receba uma Estrada, irá entregar um '2'

   Caso receba uma Relva, irá entregar um '3'

== Exemplos de utilização:

>>> observarTerrenos (Rio 2,Tronco)
1
>>> observarTerrenos (Estrada 2,Carro)
2
>>> observarTerrenos (Estrada 2,Nenhum)
2
-}

observarTerrenos :: (Terreno,Obstaculo) -> Int
observarTerrenos (Rio v, y) = 1
observarTerrenos (Estrada v, y) = 2
observarTerrenos (Relva, y) = 3

{- |
6ª função auxiliar 

== Explicação da função andamento:
    
    Caso a jogada mova para cima, na coordenada tirará uma unidade ao y pois o refrencial é ao contrário. 

    Caso a jogada mova para baixo, na coordenada somará uma unidade ao y pois o refrencial é ao contrário.

    Caso a jogada mova para a esquerda, na coordenada tirará uma unidade ao x.

    Caso a jogada mova para a direita, na coordenada aumentará uma unidade ao x.

    Caso haja uma Árvore na posição que queremos ir, ficará na mesma posição.

-}



andamento :: Jogo -> Jogada -> Jogo
andamento (Jogo (Jogador (x,y)) map@(Mapa l ((t,obs@(h:hs)):ts))) j | j == Move Cima && (foraMapa (Jogo (Jogador (x,y-1)) map)
                                                || verObst ( Jogo (Jogador (x,y-1)) map ) == (Relva,Arvore)) = (Jogo (Jogador (x,y)) (Mapa l (movimentaVelocidade((t,obs):ts))))
                                                                | j == Move Cima  = Jogo (Jogador (x,y-1)) (Mapa l (movimentaVelocidade((t,obs):ts)))

                                                                | j == Move Baixo && (foraMapa (Jogo (Jogador (x,y+1)) map)
                                                || verObst ( (Jogo (Jogador (x,y+1)) map) ) == (Relva,Arvore)) = (Jogo (Jogador (x,y)) (Mapa l (movimentaVelocidade((t,obs):ts))))
                                                                | j == Move Baixo = Jogo (Jogador (x,y+1)) (Mapa l (movimentaVelocidade((t,obs):ts)))

                                                                | j == Move Esquerda && (foraMapa (Jogo (Jogador (x-1,y)) map)
                                                || verObst ( (Jogo (Jogador (x-1,y)) map) ) == (Relva,Arvore)) = (Jogo (Jogador (x,y)) (Mapa l (movimentaVelocidade((t,obs):ts))))
                                                                | j== Move Esquerda && snd (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) )== Tronco 
                                                = Jogo (Jogador (x+(observarVRio (verObst (Jogo (Jogador (x,y)) map)))-1,y)) (Mapa l (movimentaVelocidade((t,obs):ts)))
                                                                | j == Move Esquerda = Jogo (Jogador (x-1,y)) (Mapa l (movimentaVelocidade((t,obs):ts)))

                                                                | j == Move Direita && (foraMapa (Jogo (Jogador (x+1,y)) map)
                                                || verObst ( (Jogo (Jogador (x+1,y)) map) ) == (Relva,Arvore))  = (Jogo (Jogador (x,y)) (Mapa l (movimentaVelocidade((t,obs):ts))))
                                                                | j == Move Direita && snd (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) )== Tronco 
                                                = Jogo (Jogador (x+(observarVRio (verObst (Jogo (Jogador (x,y)) map)))+1,y)) (Mapa l (movimentaVelocidade((t,obs):ts)))
                                                                | j == Move Direita = Jogo (Jogador (x+1,y)) (Mapa l (movimentaVelocidade((t,obs):ts)))

                                                                | j == Parado && observarTerrenos (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts)))) ==1
                                                             && snd (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) )== Tronco
                                                             = (Jogo (Jogador ((x+(observarVRio (verObst (Jogo (Jogador (x,y)) map)))),y)) (Mapa l (movimentaVelocidade((t,obs):ts))))
                                                                | j == Parado = Jogo (Jogador (x,y)) (Mapa l (movimentaVelocidade((t,obs):ts)))

-- | A função 'observarVRio' têm a funcionalidade de de retirar a velocidade de apenas dos rios.
observarVRio :: (Terreno,Obstaculo) -> Int
observarVRio (Rio v, x)= v
observarVRio (Estrada v, x)= 0
observarVRio (Relva , x)= 0

{- |

== Explicação da função principal animaJogo:

A função principal 'animaJogo' é utilizada para atualizar o mapa, obstáculos e a personagem, segundo certos eventos e jogadas, que o jogador se sustenta.
-}

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((t,obs@(h:hs)):ts))) j =  andamento  (Jogo (Jogador (x,y)) (Mapa l ((((t,obs):ts))))) j

