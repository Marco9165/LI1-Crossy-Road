module Tarefa1_2022li1g094_Spec where

import LI12223
import Tarefa1_2022li1g094
import Test.HUnit


test1 = TestCase (assertEqual "Troncos na Estrada e na Relva" False (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Tronco]),(Estrada 2 ,[Carro,Nenhum,Tronco])])))
test2 = TestCase (assertEqual "Arvores na Estrada e no Rio" False (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Tronco]),(Estrada 2 ,[Carro,Nenhum,Tronco])])))
test3 = TestCase (assertEqual "Carros na Relva e no Rio" False (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Carro]),(Rio 2 ,[Tronco,Nenhum,Carro])])))
test4 = TestCase (assertEqual "Obstáculos Certos" True (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 ,[Tronco,Nenhum,Tronco])])))
test5 = TestCase (assertEqual "Rios contíguos direções iguais" False (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Tronco,Nenhum,Nenhum]),(Rio 3 ,[Tronco,Nenhum,Nenhum])])))
test6 = TestCase (assertEqual "Rios contíguos direções opostas" True (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Tronco,Nenhum,Nenhum]),(Rio (-3) ,[Tronco,Nenhum,Nenhum])])))
test7 = TestCase (assertEqual "Troncos com + de 5 uni de comprimento" False (mapaValido (Mapa 7 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),(Rio 2 ,[Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])])))
test8 = TestCase (assertEqual "Carros com + de 3 uni de comprimento" False (mapaValido (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 2 ,[Carro,Carro,Carro,Carro,Nenhum])])))
test9 = TestCase (assertEqual "Não existem espaços vazios" False (mapaValido (Mapa 3 [(Relva,[Arvore,Arvore,Arvore]),(Estrada 2 ,[Carro,Carro,Carro]),(Rio 2 , [Tronco,Tronco,Tronco])])))
test10 = TestCase (assertEqual "Existe pelo menos um obstáculo <Nenhum>" True (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])])))
test11 = TestCase (assertEqual "Comprimento da linha de obstáculos = Largura do Mapa" True (mapaValido (Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Nenhum,Nenhum,Tronco])])))
test12 = TestCase (assertEqual "Comprimento da linha de obstáculos =7 Largura do Mapa" False (mapaValido (Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 2 ,[Carro,Nenhum,Carro,Carro]),(Rio 2 , [Nenhum,Nenhum,Tronco,Tronco])])))
test13 = TestCase (assertEqual "Existir contiguamente mais de 4 Rios" False (mapaValido (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco])])))
test14 = TestCase (assertEqual "Existir contiguamente mais de 5 Estradas" False (mapaValido (Mapa 2 [(Estrada 1,[Nenhum,Carro]),(Estrada 1,[Nenhum,Carro]),(Estrada (-1),[Nenhum,Carro]),(Estrada (-1),[Nenhum,Carro]),(Estrada 1,[Nenhum,Carro]),(Estrada 1,[Nenhum,Carro])])))
test15 = TestCase (assertEqual "Existir contiguamente mais de 5 Relvas" False (mapaValido (Mapa 2 [(Relva ,[Nenhum,Arvore]),(Relva ,[Nenhum,Arvore]),(Relva ,[Nenhum,Arvore]),(Relva ,[Nenhum,Arvore]),(Relva ,[Nenhum,Arvore]),(Relva ,[Nenhum,Arvore])])))
test16 = TestCase (assertEqual "Mapa Grande" True (mapaValido (Mapa 12 [(Estrada 3,[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada (-3),[Carro,Nenhum,Carro,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Rio (-5),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),(Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]),(Estrada 3,[Nenhum,Carro,Carro,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Carro,Carro]),(Estrada (-3),[Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-5),[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Rio 2,[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum])])))


testsT1 = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16]
