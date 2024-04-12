{- |
Module      : Tarefa4_2022li1g094
Description : Determinar se o jogo terminou
Copyright   : Marco António Fernandes Brito  <a104187 @alunos.uminho.pt>
              Rodrigo da Silva de Sousa <a104528@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}

module Tarefa4_2022li1g094 where

import LI12223
import Tarefa3_2022li1g094


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

{- |
3ª função auxiliar 

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


{- |
4ª função auxiliar 

== Explicação da função listTerr:

   A função auxiliar 'listTerr' recebe uma lista de pares de terrenos, lista de obstaculos e entrega uma lista de Terrenos.

   Dado uma determinada lista, a função colocará numa só lista, todos os terrenos que aparecem na mesma.

== Exemplos de utilização:

>>> listTerr [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])])
[Relva,Estrada 2,Rio 2]

-}

{- |
5ª função auxiliar 

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

{- | 
A função principal 'jogoTerminou' recebe um Jogo e entregue um Boleano.
A função 'jogoTerminou' é formada por diversas funções, que constituem certas restrições, para que o Jogo termine. 
   Ou seja, indica se o jogador perdeu o jogo, onde True significa que sim. Para isso
   deve testar se o jogador se encontra fora do mapa, na  ́agua, ou “debaixo” de um carro (i.e. na mesma posição de um carro.)
   Caso não viole nenhuma restrição o Mapa será valido, isto é, irá entregar True.
= Explicação da função

=
   Antes de explicar o funcionamento da função principal 'jogoTerminou', é necessário compreender o funcionamento de cada função auxiliar, como fiz anteriormente.

   Caso o jogador se encontre fora do mapa, o jogo termine, ou seja, True, logo caso a função 'foraMapa' der True, o jogo termina.

   Na segunda condição, diz-nos que caso a função 'observarTerrenos' seja igual a '1'(Rio), e a posição que em se encontra é um Nenhum,visualizada através da função 'verObst',
   o mesmo encontra-se na água.

   Na terceira condição, diz-nos que caso a função 'observarTerrenos' seja igual a '2'(Estrada), e a posição que em se encontra é um Carro,visualizada através da função 'verObst'
   o mesmo encontra-se "debaixo" do Carro, logo, o jogo termina.

   se não acontecer nehuma destas condições, o jogo não termina.
-}


jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) | foraMapa (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) ==True = True
                                                             | observarTerrenos (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts)))) ==1 
                                                             && snd (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) )== Nenhum =True
                                                             | observarTerrenos (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) )==2 
                                                             && snd (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))))== Carro = True
                                                             | otherwise = False
             

