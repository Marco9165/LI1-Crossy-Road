{- |
Module      : Main
Description : Main do jogo
Copyright   : Marco António Fernandes Brito  <a104187 @alunos.uminho.pt>
              Rodrigo da Silva de Sousa <a104528@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}
module Main where

import LI12223
--import Tarefa1_2022li1g094 
import Tarefa2_2022li1g094 
import Tarefa3_2022li1g094 
--import Tarefa4_2022li1g094 
import Tarefa5_2022li1g094 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.IntMap.Internal (mapAccum)
import Data.Maybe
import Data.List


-- | Tipo de personagens que se pode jogar.
data Personagens
  = Galinha
  deriving (Show, Read, Eq, Enum)

-- | Menu do jogo.
data Menu = Opcoes Opcao
          | ModoJogo 
          | Pausa
          | PerdeuJogo

-- | Opções que se pode apresentar no 'Menu'.
data Opcao = Jogar
            | Sair

-- | Tempo do jogo.
type Tempo = Float

-- | Imagens do jogo.
type Texturas = [(Obstaculo, (Picture, (Float, Float)))]

-- | Personagem que se joga durante o jogo.
type Animal = [(Personagens, Picture)]

-- | Pontuação atingida durante o jogo.
type Pontos = Int

-- | Estado que o meu jogo apresenta durante todo o jogo.
type EstadoGloss = (Menu, Jogo, Texturas, Animal, Pontos, Tempo, Tempo)

---------------------------------------------------------------------------------

-- | A função 'fps1' é utilizada para modificar o frame rate do jogo.
fps1 :: Int
fps1 = 1

-- | A função 'altura' é utilizada para definir a que altura, a janela do jogo começa, com coordenadas definidas pelo Gloss.
altura :: Float
altura = 450

-- | A função 'comprimento' é utilizada para definir a que comprimento, a janela do jogo começa, com coordenadas definidas pelo Gloss.
comprimento :: Float 
comprimento = (-400)

-- | A função 'janela' é utilizada para definir o tamanho da janela do jogo.
janela :: Display
janela = --InWindow "Crossy Road" (900, 900) (-100,-100)
  FullScreen

-- | A função 'space' é utilizada para definir o espaçamento entre as imagens do jogo.
space :: Float
space = 70
 
----------------------------------------------------------------------

{- |

== Funcionalidade das funções estadoInicial e estadoGlossInicial :

   A função 'estadoInicial' recebe as coordenadas que o jogador começa no mapa previamente dado.
   A função 'estadoGlossInicial' recebe umas textura, que é uma lista de pares de obstaculos, com a respetiva 'Picture'. E entregará o desenho inicial do mapa e define os números que os 
   pontos, tempo e fps começam.
-}
estadoInicial :: Jogo
estadoInicial = (Jogo (Jogador (5,(-9))) (changeMapa mapa))

estadoGlossInicial :: Texturas -> Animal -> EstadoGloss
estadoGlossInicial texturas animal = (Opcoes Jogar,estadoInicial, texturas, animal, 0, 0, 0)

{- |

== Funcionalidade da função reageTempoGloss:

   A função 'reageTempoGloss' recebe um float, que será o tempo passado, e um EstadoGloss, que nos indica o estado que estamos atualmente no Jogo.
   Esta função serve para que ao longo do tempo , o mapa deslize, ou seja, crie novas linhas de mapa, ao longo ddo decorrer do tempo.
-}
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t (ModoJogo, (Jogo (Jogador (x,y)) mapa), texturas, animal,pontos, tempo, fps) 
                    | jogoTerminou (Jogo (Jogador (x,-y)) mapa)== True = (PerdeuJogo, (Jogo (Jogador (x,y)) mapa), texturas,animal,pontos, tempo,fps)
                    | somatempo > 0.05 = (ModoJogo, ((deslizaJogo (round (tempo)) (movimentosValidos (Jogo (Jogador (x,y)) (changeMapa mapa))(Parado)))), texturas, animal,pontos , tempo + t,0)
                    | otherwise = (ModoJogo, ((deslizaJogo (round (tempo)) ( (Jogo (Jogador (x,y)) (changeMapa mapa))))), texturas, animal,pontos +1 , tempo + t,somatempo)
          where  somatempo = fps +t
reageTempoGloss t (menu, (Jogo (Jogador (x,y)) mapa), texturas, animal,pontos, tempo,fps)= ((menu, (Jogo (Jogador (x,y)) mapa), texturas, animal,pontos, tempo, fps))

{- |

== Funcionalidade da função drawEstadoGloss:

   A função 'drawEstadoGloss' desenha-nos todo o jogo. Desde o Menu, durante o jogo, durante a pausa e quando o jogo termina.
-}
drawEstadoGloss :: EstadoGloss -> Picture
drawEstadoGloss (PerdeuJogo, (Jogo (Jogador (x,y)) mapa), texturas, animal,pontos, tempo,fps) 
  | pontos== 1 && (round tempo)==1  = Translate (-750) 0 $ Color orange $ scale 0.5 0.5 $ Text  ("O seu score foi de " ++ show (pontos) ++ " ponto em "++ show (round tempo) ++ " segundo" )
  | pontos== 1  = Translate (-750) 0 $ Color orange $ scale 0.5 0.5 $ Text  ("O seu score foi de " ++ show (pontos) ++ " ponto em "++ show (round tempo) ++ " segundos" )      
  | (round tempo)==1  = Translate (-750) 0 $ Color orange $ scale 0.5 0.5 $ Text  ("O seu score foi de " ++ show (pontos) ++ " pontos em "++ show (round tempo) ++ " segundo" )  
  | otherwise = Translate (-750) 0 $ Color orange $ scale 0.5 0.5 $ Text  ("O seu score foi de " ++ show (pontos) ++ " pontos em "++ show (round tempo) ++ " segundos" )                                                                                
drawEstadoGloss (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa), texturas, animal,pontos, tempo, fps) = Pictures [Color orange $ drawOption "Jogar", Translate 0 (-110) $ Color white $ drawOption "Sair"]
drawEstadoGloss (Opcoes Sair, (Jogo (Jogador (x,y)) mapa), texturas, animal,pontos, tempo, fps) = Pictures [Color white $ drawOption "Jogar", Color orange $ Translate 0 (-110) $ drawOption "Sair"]
drawEstadoGloss (Pausa, (Jogo (Jogador (x,y)) mapa), texturas, animal,pontos, tempo, fps) = Translate (-100) 0 $ Color orange $ scale 0.7 0.7 $ Text  ("Pausa" )
drawEstadoGloss (ModoJogo, (Jogo (Jogador (x,y)) mapa), texturas, animal,pontos, tempo, fps) = Pictures (desenho ++ [mostraPontuacao pontos])
  where
      desenhoDoMapa  = drawMapa comprimento altura (linhaDeObstaculos (changeMapa mapa)) texturas 
      desenhoJogador = drawPlayer (Jogador (x,y)) animal
      desenho        = desenhoDoMapa ++ [desenhoJogador] 

drawOption option = Translate (-100) 20 $ Scale (0.7) (0.7) $ Text option

{- |
== Funcionalidade da função mostraPontuacao:

   A função 'mostraPontuacao' recebe um inteiro  e devolve uma 'Picture'. Esta função é utilizada, como o próprio nome diz, mostrar a pontuação alcançada pelo jogador. 
-}
mostraPontuacao:: Int -> Picture
mostraPontuacao n = Translate 450 400 $ Color white $ scale 0.50 0.55 $ Text (show $ n)
   
{- |

== Funcionalidade da função drawMapa:

   A função 'drawMapa', como o próprio nome diz, têm como funcionalidade, desenhar o mapa, com a juda de funções auxiliares 'drawLinha'.
-}

drawMapa :: Float -> Float -> [[Obstaculo]] -> Texturas -> [Picture]
drawMapa x y (h:t) texturas = linha ++ restante
  where linha = drawLinha x y h texturas
        restante = drawMapa x (y-space) t texturas
drawMapa x y _ _ = []

{- |

== Funcionalidade da função drawLinha:

   A função 'drawLinha', como o próprio nome diz, têm como funcionalidade, desenhar a linha do mapa, com a ajuda de funções auxiliares 'drawObstaculo'.
-}

drawLinha :: Float -> Float -> [Obstaculo] -> Texturas -> [Picture]
drawLinha x y (h:t) texturas = obstaculo : restante
  where obstaculo = drawObstaculo x y h texturas
        restante     = drawLinha (x+space) y t texturas
drawLinha x y _ _ = []

{- |

== Funcionalidade da função drawObstaculo:

   A função 'drawObstaculo', como o próprio nome diz, têm como funcionalidade, desenhar os obstaculos.
   A mesma com que se desenvolva uma picture do Obstaculo pretendido, através... 
-}
drawObstaculo :: Float -> Float -> Obstaculo -> Texturas -> Picture
drawObstaculo x y obstaculo texturas = Translate realX realY textura
  where tuples  = (fromJust . retornSnd obstaculo) texturas
        textura = fst tuples
        realX   = ((+x) . fst . snd) tuples
        realY   = ((+y) . snd . snd) tuples   

{- |
== Funcionalidade da função reageTempoGloss:

   A função 'reageTempoGloss' recebe um float, que será o tempo passado, e um EstadoGloss, que nos indica o estado que estamos atualmente no Jogo.
   Esta função serve para que ao longo do tempo , o mapa deslize, ou seja, crie novas linhas de mapa, ao longo do decorrer do tempo.
-}

linhaDeObstaculos :: Mapa -> [[Obstaculo]]
linhaDeObstaculos (Mapa largura []) = []
linhaDeObstaculos (Mapa largura ((_, obs):t)) = obs : linhaDeObstaculos (Mapa largura t)

{- |
== Funcionalidade da função drawPlayer:

   A função 'drawPlayer' recebe um jogador, e um 'Animal' e entregará uma 'Picture' nas coordenadas corretas definidas do Gloss. Através da ajuda de funções auxiliares 'realPlayerY'
   e 'realPlaerY'.
-}

drawPlayer :: Jogador -> Animal -> Picture
drawPlayer (Jogador (x,y)) animal = (Translate (realPlayerX x) (realPlayerY y) image)
  where image = (fromJust . retornSnd Galinha) animal

{- |
== Funcionalidade da função realPlayerX:

   A função 'realPlayerX' irá receber Int, que será a abcissa do jogador e entregará um Float, que será a abcissa real do Gloss.
-}
realPlayerX :: Int -> Float
realPlayerX = (+ (comprimento)) . (*space). realToFrac

{- |
== Funcionalidade da função realPlayerY:

   A função 'realPlayerY' irá receber Int, que será a coordenada do jogador e entregará um Float, que será a coordenada real do Gloss.
-}
realPlayerY :: Int -> Float
realPlayerY = (+ (altura)) .(*space)  . realToFrac

{- |
== Funcionalidade da função retornSnd:

   A função 'retornSnd' retorna o segundo argumento, atendendo a uma condição dada pelo primeiro argumento.
-}
retornSnd :: Eq a => a -> [(a,b)] -> Maybe b
retornSnd _ [] = Nothing
retornSnd e ((a,b):t)
    | e == a = Just b
    | otherwise = retornSnd e t

{- |
== Funcionalidade da função changeCarRight e changeCarLeft e changeNenhunsRio:

   A função 'changeCarRight', caso no mapa dado, observa se carro lê-a um Nenhum, tranformará em um OnlyE. Caso lê-a um carro tranformará num CarroR.
   A função 'changeCarLeft', caso no mapa dado, observa se carro lê-a um Nenhum, tranformará em um OnlyE. Caso lê-a um carro tranformará num CarroL. 
   A função 'changeNenhunsRio', caso no mapa dado, observa se carro lê-a um Nenhum, tranformará em um OnlyR.
   O objetivo destas funções é para colocar as pictures corretas em cada lugar.
-}
changeCarRight :: [Obstaculo] -> [Obstaculo]
changeCarRight [] = []
changeCarRight (x:xs) | x == Nenhum = (OnlyE:changeCarRight xs)
                      | x == Carro = (CarroR:changeCarRight xs)
                      | otherwise = (x: changeCarRight xs)

changeCarLeft :: [Obstaculo] -> [Obstaculo]
changeCarLeft [] = []
changeCarLeft (x:xs) | x == Nenhum = (OnlyE:changeCarLeft xs)
                     | x == Carro = (CarroL:changeCarLeft xs)
                     | otherwise = (x: changeCarLeft xs)                        


changeNenhunsRio :: [Obstaculo] -> [Obstaculo]
changeNenhunsRio [] = []
changeNenhunsRio (x:xs) | x == Nenhum = (OnlyR:changeNenhunsRio xs)
                        | otherwise = (x: changeNenhunsRio xs)

{- |
== Funcionalidade da função changeNenhuns:
   
   A função 'changeNenhuns' trocará os nomes do mapa, anteriormente definidos, nos respetivos Terrenos.
-}
changeNenhuns :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
changeNenhuns [] = []
changeNenhuns ((Rio v,(x:xs)):t) = ((Rio v,changeNenhunsRio (x:xs)):changeNenhuns t)
changeNenhuns ((Estrada v,(x:xs)):t) | v>0 = ((Estrada v,changeCarRight (x:xs)): changeNenhuns t)
                                     | v<0 = ((Estrada v,changeCarLeft (x:xs)): changeNenhuns t)
                                     | otherwise= ((Estrada v,changeCarRight (x:xs)): changeNenhuns t)
changeNenhuns ((Relva,(x:xs)):t) = ((Relva,(x:xs)):changeNenhuns t)

{- |
== Funcionalidade da função  changeMapa:
   
   A função 'changeMapa' trocará os nomes do mapa, anteriormente definidos, nos respetivos Terrenos.
-}
changeMapa :: Mapa -> Mapa
changeMapa (Mapa l []) = (Mapa l [])
changeMapa (Mapa l ((Rio v,(x:xs)):t)) = (Mapa l ((Rio v,changeNenhunsRio (x:xs)): changeNenhuns t))
changeMapa (Mapa l ((Estrada v,(x:xs)):t))| v > 0 = (Mapa l ((Estrada v,changeCarRight (x:xs)):changeNenhuns t)) 
                                          | v < 0 = (Mapa l ((Estrada v,changeCarLeft (x:xs)):changeNenhuns t)) 
                                          | otherwise= (Mapa l ((Estrada v,changeCarRight (x:xs)):changeNenhuns t))
changeMapa (Mapa l ((Relva,(x:xs)):t)) = (Mapa l ((Relva,(x:xs)): changeNenhuns t)) 

{- |
== Funcionalidade da função reageEventoGloss:
   
   A função 'reageEventoGloss' recebrá um evento e um 'estadoGloss'. A objetivo desta função é fazer reagir, ao pressionar determinada tecla ou determina situação.
   Ou seja, por exemplo: Caso me encontre na tela do menu e na Opção Jogar, caso pressione a tecla Enter, o meu jogo mudará, para o Estado, ModoJogo, isto é, 
   quando começamos a jogar no mapa do jogo. 
-}
-- Parte do Menu
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, texturas,animal,pontos, tempo,fps) =
   (ModoJogo, jogo, texturas,animal,pontos, tempo,fps)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, texturas,animal,pontos, tempo,fps) =
   (Opcoes Sair, jogo, texturas,animal,pontos, tempo,fps)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, texturas,animal,pontos, tempo,fps) =
   (Opcoes Sair, jogo, texturas,animal,pontos, tempo,fps)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, texturas,animal,pontos, tempo,fps) =
   (Opcoes Jogar, jogo, texturas,animal,pontos, tempo,fps)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo, texturas,animal,pontos, tempo,fps) =
   (Opcoes Jogar, jogo, texturas,animal,pontos, tempo,fps)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, texturas,animal,pontos, tempo,fps) =
   error "Fim de Jogo"
-- Parte do Modo Jogo,com todos os movimentos possíveis.
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) mapa), texturas,animal,pontos, tempo,fps) 
   | movimentosValidos ((Jogo (Jogador (x,y)) mapa)) (Move Cima) == moverPersonagem ((Jogo (Jogador (x,y)) mapa)) (Move Cima) 
   = (ModoJogo,(movimentosValidos ((Jogo (Jogador (x,y)) mapa)) (Move Cima)), texturas,animal,pontos+1, tempo, fps)
   | otherwise = (ModoJogo,(movimentosValidos ((Jogo (Jogador (x,y)) mapa)) (Move Cima)), texturas,animal,pontos, tempo, fps)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) mapa), texturas,animal,pontos, tempo,fps)
   | movimentosValidos ((Jogo (Jogador (x,y)) mapa)) (Move Baixo) == moverPersonagem ((Jogo (Jogador (x,y)) mapa)) (Move Baixo) 
   = (ModoJogo,(movimentosValidos ((Jogo (Jogador (x,y)) mapa)) (Move Baixo)), texturas,animal,pontos-1, tempo, fps)
   | otherwise = (ModoJogo,(movimentosValidos ((Jogo (Jogador (x,y)) mapa)) (Move Baixo)), texturas,animal,pontos, tempo, fps)

reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) mapa), texturas,animal,pontos, tempo,fps)= 
  (ModoJogo,(movimentosValidos ((Jogo (Jogador (x,y)) mapa)) (Move Esquerda)), texturas,animal,pontos, tempo,fps)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo,(Jogo (Jogador (x,y)) mapa), texturas,animal,pontos, tempo,fps)= 
  (ModoJogo,(movimentosValidos ((Jogo (Jogador (x,y)) mapa)) (Move Direita)), texturas,animal,pontos, tempo,fps)
-- Continuar a jogar depois de vencer
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo, texturas,animal,pontos, tempo,fps) = estadoGlossInicial texturas animal
-- Parte da Pausar o mapa
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo, jogo, texturas,animal,pontos, tempo,fps) = (Pausa, jogo, texturas,animal,pontos, tempo,fps)
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) (Pausa, jogo, texturas,animal,pontos, tempo,fps) = (ModoJogo, jogo, texturas,animal,pontos, tempo,fps)
-- Identificar que perdeu o Jogo, mostou-me uma mensagem com o seu tempo.
reageEventoGloss _ (ModoJogo, (Jogo (Jogador (x,y)) mapa), texturas,animal,pontos, tempo,fps) 
      | jogoTerminou (Jogo (Jogador (x,-y)) mapa) == True = (PerdeuJogo, (Jogo (Jogador (x,y)) mapa), texturas,animal,pontos, tempo,fps)
reageEventoGloss _ e = e

{- |

==  Funcionalidade da função movimentosValidos:

    A função 'movimentosValidos' recebe um jogo e uma jogada.

    Caso exista uma Árvore na posição que queremos ir, qualquer que seja, a direção, o jogador ficará na mesma posição.

    Caso a jogada mova para cima, o jogador irá para a posição acima

    Caso a jogada mova para baixo, o jogador irá para a posição abaixo

    Caso a jogada mova para a esquerda, o jogador irá para a posição à esquerda

    Caso a jogada mova para a direita, o jogador irá para a posição à direita.

    Caso a jogada seja Parado, o jogador, caso estaja em cima do tronco, acompanhará em cima do tronco.
    
    Caso a jogada seja Parado, o jogador, permanecerá no mesmo sítio.
-}
movimentosValidos :: Jogo -> Jogada -> Jogo
movimentosValidos (Jogo (Jogador (x,y)) map@(Mapa l ((t,(h:hs)):ts))) j 
                                                                | j == Move Cima && (foraMapa (Jogo (Jogador (x,-y-1)) (map))
                                                || (verObst ( Jogo (Jogador (x,-y-1)) (map))) == (Relva,Arvore)) = (Jogo (Jogador (x,y)) (map))
                                                                | j == Move Cima  = Jogo (Jogador (x,y+1)) (map)

                                                                | j == Move Baixo && (foraMapa (Jogo (Jogador (x,-y+1)) (map))
                                                || (verObst ( Jogo (Jogador (x,-y+1)) (map))) == (Relva,Arvore)) = (Jogo (Jogador (x,y)) (map))
                                                                | j == Move Baixo = Jogo (Jogador (x,y-1)) (map)

                                                                | j == Move Esquerda && (foraMapa (Jogo (Jogador (x-1,-y)) (map))
                                                || (verObst ( Jogo (Jogador (x-1,-y)) (map))) == (Relva,Arvore)) = (Jogo (Jogador (x,y)) (map))
                                                                | j == Move Esquerda = Jogo (Jogador (x-1,y)) (map)

                                                                | j == Move Direita && (foraMapa (Jogo (Jogador (x+1,-y)) (map))
                                                || verObst ( (Jogo (Jogador (x+1,-y)) (map)) ) == (Relva,Arvore))  = (Jogo (Jogador (x,y)) (map))
                                                                | j == Move Direita = Jogo (Jogador (x+1,y)) (map)

                                                                | j == Parado && observarTerrenos (verObst (Jogo (Jogador (x,-y)) (moverMapa (Mapa l ((t,(h:hs)):ts))))) ==1
                                                             && snd (verObst (Jogo (Jogador (x,-y)) (Mapa l ((t,(h:hs)):ts))) )== Tronco
                                                             = (Jogo (Jogador ((x+ (observarVelRio ((verObst (Jogo (Jogador (x,-y)) (moverMapa (Mapa l ((t,(h:hs)):ts)))))))),y)) (moverMapa (map)))                                                                                                  
                                                                | j == Parado = Jogo (Jogador (x,y)) (moverMapa (map))

{- | 
= Explicação da função moverPersonagem. 

   A função 'moverPersonagem' é uma função auxiliar,para contar a pontuação de forma correta. A mesma move a Personagem sem qualquer tipo de restrição.
-}
moverPersonagem :: Jogo -> Jogada -> Jogo
moverPersonagem (Jogo (Jogador (x,y)) map@(Mapa l ((t,(h:hs)):ts))) j 
                                                                | j == Move Cima  = Jogo (Jogador (x,y+1)) (map)

                                                                | j == Move Baixo = Jogo (Jogador (x,y-1)) (map)

                                                                | j == Move Esquerda = Jogo (Jogador (x-1,y)) (map)

                                                                | j == Move Direita = Jogo (Jogador (x+1,y)) (map)
                                         
                                                                | j == Parado = Jogo (Jogador (x,y)) (moverMapa (map))

{- | 
= Explicação da função jogoTerminou:

   A função 'jogoTerminou' recebe um Jogo e entregue um Boleano.
   A função 'jogoTerminou' é formada por diversas funções, que constituem certas restrições, para que o Jogo termine. 
   Ou seja, indica se o jogador perdeu o jogo, onde True significa que sim. Para isso
   deve testar se o jogador se encontra fora do mapa, na  ́agua, ou “debaixo” de um carro (i.e. na mesma posição de um carro.)
   Caso não viole nenhuma restrição o Mapa será valido, isto é, irá entregar True.
-}

jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) | observarTerrenos (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts)))) ==1 
                                                          && snd (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))) )== OnlyR = True                                                       
                                                             | snd (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))))== CarroL
                                                         || snd (verObst (Jogo (Jogador (x,y)) (Mapa l ((t,(h:hs)):ts))))== CarroR = True
                                                             | foraMap (Jogo (Jogador (x,-y)) (Mapa l ((t,(h:hs)):ts))) ==True = True
                                                             | otherwise = False


{- | 
   A função 'foraMap' é uma função para determinar os limites da janela para detetar quando o jogador está fora do mapa.
-}
foraMap :: Jogo -> Bool
foraMap  (Jogo (Jogador (x,y)) (Mapa l b@((t,(h:hs)):ts)))  | x > (l-1) = True
                                                            | x < 0 = True
                                                            | (negate y) < 0 = True
                                                            | (abs y) > ((length (listTerr b))-1) = True
                                                            | otherwise= False

-- | A função 'moverMapa' fará com os obstáculos dos respetivos Terrenos se movam, conforme a sua velocidade do Terreno. 
moverMapa :: Mapa -> Mapa
moverMapa (Mapa l ((t,(h:hs)):ts)) = (Mapa l (movimentaVelocidade ((t,(h:hs)):ts))) 

-- | A função 'observarVelRio' é utilizada apenas para retirar a velocidade do Rio.
observarVelRio :: (Terreno,Obstaculo) -> Int
observarVelRio (Rio v, x) = v
observarVelRio (Estrada v, x)= 0
observarVelRio (Relva , x)= 0

-- | A função 'main' é a função principal do tipo IO, que é utilizada para o começo do jogo, onde estão definidas todas as funções para a construção do jogo.

main :: IO ()
main = do   
    arvore  <- loadBMP "Img/arvore2.bmp"
    onlyrelva  <- loadBMP "Img/relvaN.bmp"
    tronco  <- loadBMP "Img/tronco.bmp"
    onlyrio <- loadBMP "Img/rioN.bmp"
    onlyE   <- loadBMP "Img/estradaN.bmp"
    carroR <- loadBMP "Img/carrodireita.bmp"
    carroL <- loadBMP "Img/Carro_L.bmp"
    jogador <- loadBMP "Img/galinha.bmp"
    play janela  
      black     -- cor do fundo da janela
      fps1     
      (estadoGlossInicial
      [
       (Tronco,  ((scale 0.12 0.12 tronco), (0,0))),
       (Arvore,  ((scale 0.12 0.12 arvore), (0,0))),
       (OnlyE,   ((scale 0.12 0.12 onlyE), (0,0))),
       (Nenhum,  ((scale 0.12 0.12 onlyrelva),(0,0))),
       (OnlyR,   ((scale 0.12 0.12 onlyrio), (0,0))),
       (CarroR,  ((scale 0.12 0.12 carroR), (0,0))),
       (CarroL,  ((scale 0.12 0.12 carroL), (0,0)))
      ]
      [(Galinha, (scale 0.05 0.05 jogador)) ]
      )  -- define estado inicial do jogo
      drawEstadoGloss 
      reageEventoGloss    
      reageTempoGloss     

-- | Mapa inicial utilizado, para o começo do jogo.
mapa = Mapa 12 [(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Estrada (-3),[Carro,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum]),
               (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum, Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore]),
               (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
               (Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),
               (Rio (-1),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
               (Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]),
               (Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro]),
               (Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro]),
               (Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Arvore,Nenhum]),
               (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),
               (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),
               (Rio (-2),[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco]),
               (Rio 3,[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum])]
