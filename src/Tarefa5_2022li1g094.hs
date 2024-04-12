{- |
Module      : Tarefa5_2022li1g094
Description : Movimentação do personagem e obstáculos
Copyright   : Marco António Fernandes Brito  <a104187 @alunos.uminho.pt>
              Rodrigo da Silva de Sousa <a104528@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g094 where

import LI12223
import Tarefa2_2022li1g094

{- |

== Explicação da função deslizaJogo:

   A função principal 'deslizaJogo' recebe um inteiro e um Jogo e entrega um Jogo

   Dado um determinado int Random e um determinado Jogo, a função irá, retirar a última linha do Mapa, através da função auxiliar 'eliminarLast', e irá adicionar uma nova linha 
   através das funções já feitas na Tarefa2, com os devidos parâmetros válidos.

   Para dar o efeito de deslize do Mapa, a função também fará que o personagem aumente 1 unidade na oodenada para que assim o mesmo dê um efeito de que o jogador “ficou para trás”  

== Exemplos de utilização:

>>> deslizaJogo 2 (Jogo(Jogador (2,0))(Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]))
Por exemplo: Jogo (Jogador (2,1)) (Mapa 3 [(Estrada 0,[Carro,Carro,Nenhum]),(Rio 1,[Nenhum,Nenhum,Tronco])])

-}

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo valorRandom (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) = 
    (Jogo (Jogador (x,y-1))) (Mapa l (completarLinha l (escolherterrenoaleatorio (eliminarLast ((Mapa l ((t,(h:hs)):ts)))) valorRandom) valorRandom))

{- |

== Explicação da função eliminarLast:

   A função auxiliar 'eliminarLast' recebe um Mapa e entrega um Mapa

   Dado um determinado Mapa, a função irá, retirar a última linha do Mapa, através da função predefinida 'take', através do comprimento do Mapa e o respetivo Mapa.

== Exemplos de utilização:

>>> eliminarLast (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])
Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco])]

-}

eliminarLast :: Mapa -> Mapa
eliminarLast (Mapa l ((t,(h:hs)):ts)) = Mapa l (take ((length((t,(h:hs)):ts))-1) ((t,(h:hs)):ts))

