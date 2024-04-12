module Tarefa3_2022li1g094_Spec where

import LI12223
import Tarefa3_2022li1g094
import Test.HUnit

test1 = TestCase (assertEqual "Árvore Cima" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima)))
test2 = TestCase (assertEqual "Árvore Direita" (Jogo(Jogador (0,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (0,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Direita)))
test3 = TestCase (assertEqual "Árvore Esquerda" (Jogo(Jogador (2,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (2,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Esquerda)))
test4 = TestCase (assertEqual "Arvore Atrás" (Jogo(Jogador (1,0))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])]))(animaJogo (Jogo(Jogador (1,0))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Baixo)))
test5 = TestCase (assertEqual "Parado no Tronco Direita" (Jogo(Jogador (2,0))(Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]))(animaJogo (Jogo(Jogador (1,0))(Mapa 3 [(Rio 1 ,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])])) (Parado)))
test6 = TestCase (assertEqual "Parado no Tronco Esquerda" (Jogo(Jogador (0,0))(Mapa 3 [(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])]))(animaJogo (Jogo(Jogador (1,0))(Mapa 3 [(Rio (-1),[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])])) (Parado)))
test7 = TestCase (assertEqual "Escapar de um Carro" (Jogo(Jogador (1,0))(Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 1 ,[Nenhum,Carro,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva ,[Nenhum,Nenhum,Arvore]),(Estrada 1,[Carro,Nenhum,Nenhum])])) (Move Cima)))
test8 = TestCase (assertEqual "Tentar sair do Mapa Direita" (Jogo(Jogador (2,1))(Mapa 3 [(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (2,1))(Mapa 3 [(Rio 2 ,[Nenhum,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Direita)))
test9 = TestCase (assertEqual "Tentar sair do Mapa Direita" (Jogo(Jogador (0,1))(Mapa 3 [(Rio 2 ,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (0,1))(Mapa 3 [(Rio 2 ,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore])])) (Move Esquerda)))
test10 = TestCase (assertEqual "Atropelamento1" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Parado)))
test11 = TestCase (assertEqual "Atropelamento2" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,2))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 1,[Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima)))
test12 = TestCase (assertEqual "Atropelamento3" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (1,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore])])) (Move Baixo)))
test13 = TestCase (assertEqual "Atropelamento4" (Jogo(Jogador (0,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore])])) (Move Esquerda)))
test14 = TestCase (assertEqual "Atropelamento5" (Jogo(Jogador (2,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore])])) (Move Direita)))
test15 = TestCase (assertEqual "Atropelamento6" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 1,[Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Cima)))
test16 = TestCase (assertEqual "Atropelamento7" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Baixo)))
test17 = TestCase (assertEqual "Atropelamento8" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])]))(animaJogo (Jogo(Jogador (2,1))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Esquerda)))
test18 = TestCase (assertEqual "Atropelamento9" (Jogo(Jogador (1,1))(Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum,Nenhum,Carro,Carro]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (2,1))(Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])])) (Move Esquerda)))
test19 = TestCase (assertEqual "Atropelamento10" (Jogo(Jogador (3,1))(Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (3,1))(Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])])) (Move Cima)))
test20 = TestCase (assertEqual "Atropelamento11" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Carro,Carro]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Parado)))
test21 = TestCase (assertEqual "Atropelamento12" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Carro,Carro]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,2))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima)))
test22 = TestCase (assertEqual "Atropelamento13" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (1,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore])])) (Move Baixo)))
test23 = TestCase (assertEqual "Atropelamento14" (Jogo(Jogador (2,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore])])) (Move Direita)))
test24 = TestCase (assertEqual "Atropelamento15" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Cima)))
test25 = TestCase (assertEqual "Atropelamento16" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Baixo)))
test26 = TestCase (assertEqual "Atropelamento17" (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])]))(animaJogo (Jogo(Jogador (0,1))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Direita)))
test27 = TestCase (assertEqual "Atropelamento18" (Jogo(Jogador (2,1))(Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])])) (Move Direita)))
test28 = TestCase (assertEqual "Atropelamento19" (Jogo(Jogador (3,1))(Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro,Carro]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (3,1))(Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])])) (Move Cima)))
test29 = TestCase (assertEqual "Atropelamento20" (Jogo(Jogador (0,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 3,[Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 3,[Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore])])) (Move Esquerda)))
test30 = TestCase (assertEqual "Atropelamento21" (Jogo(Jogador (2,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-3),[Nenhum,Nenhum,Carro]),(Relva, [Nenhum,Nenhum,Arvore])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada (-3),[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore])])) (Move Direita)))

-- Testes Movimento Tronco

test31 = TestCase (assertEqual "Tronco2" (Jogo(Jogador (1,2))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Baixo)))
test32 = TestCase (assertEqual "Tronco3" (Jogo(Jogador (3,1))(Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])])) (Move Direita)))
test34 = TestCase (assertEqual "Tronco4" (Jogo(Jogador (3,1))(Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])])) (Move Direita)))
test35 = TestCase (assertEqual "Tronco5" (Jogo(Jogador (3,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Direita)))
test36 = TestCase (assertEqual "Tronco6" (Jogo(Jogador ((-1),1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Esquerda)))
test37 = TestCase (assertEqual "Tronco7" (Jogo(Jogador (1,0))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima)))
test38 = TestCase (assertEqual "Tronco8" (Jogo(Jogador (1,1))(Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])]))(animaJogo (Jogo(Jogador (3,1))(Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])])) (Move Esquerda)))
test39 = TestCase (assertEqual "Tronco9" (Jogo(Jogador (1,1))(Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])]))(animaJogo (Jogo(Jogador (3,1))(Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])])) (Move Esquerda)))
test40 = TestCase (assertEqual "Tronco1" (Jogo(Jogador (1,0))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]))(animaJogo (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima)))



testsT3 = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,
                   test21,test22,test23,test24,test25,test26,test27,test28,test29,test30,test31,test32,test34,test35,test36,test37,test38,test39,test40]
