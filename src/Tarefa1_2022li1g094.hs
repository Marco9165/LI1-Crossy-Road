{- |
Module      : Tarefa1_2022li1g094
Description : Validação de um mapa
Copyright   : Marco António Fernandes Brito  <a104187 @alunos.uminho.pt>
              Rodrigo da Silva de Sousa <a104528@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g094 where

import LI12223

{- | 
== Explicação da função mapaValido:
A função 'mapaValido' recebe um Mapa (representado por uma lista infinita de linhas e cada linha denota um tipo de terreno e os obstáculos) e entregue um Boleano.

A função 'mapaValido' é formada por diversas funções, que constituem certas restrições, para que o Mapa formado, não seja inválido, ou seja, 
  caso o Mapa, viole alguma das restrições estabelecidas pelo forma de funcionamento do jogo, o Mapa será inválido, 
  ou seja, irá entregar False. 

  Caso não viole nenhuma restrição o Mapa será valido, isto é, irá entregar True.


== Exemplos de utilização:

>>> (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])])))
True
>>> (mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Carro,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])])))
False  

-}
mapaValido :: Mapa -> Bool
mapaValido (Mapa l ((t, (obst:obst')):ts)) = obstaculosValidos_Mapa (Mapa l ((t, (obst:obst')):ts)) 
                                           && riosContiguos(observarRio (Mapa l ((t, (obst:obst')):ts)))
                                           && comprimentoValidoTronco (Mapa l ((t, (obst:obst')):ts)) 
                                           && comprimentoValidoCarro (Mapa l ((t, (obst:obst')):ts)) 
                                           && minimoUmNenhum (Mapa l ((t, (obst:obst')):ts)) 
                                           && larguraMapa (Mapa l ((t, (obst:obst')):ts)) 
                                           && limiteTerreno(listaInt(observarTerrenos (Mapa l ((t, (obst:obst')):ts))))

{- |
1ª Restrição- Não existem obstáculos em terrenos improprios, e.g. troncos em estradas ou relvas,  ́arvores em rios ou estradas, etc

== Explicação da função obstaculosValidos:

  A função auxiliar 'obstaculosValidos' recebe um lista de pares (Terreno,[Obstaculo]) e entrega um Boleano.

  A explicação desta função é: caso haja troncos em estradas ou relvas,  ́arvores em rios ou estradas, etc ('elem'), da lista dada, é igual a True. 

  E de seguida nego esse boleano pois, caso tenha um obstaculo invalido (True), equivale a False.
-}

obstaculosValidos :: (Terreno,[Obstaculo]) -> Bool
obstaculosValidos (Rio _,l) = not(elem Carro l || elem Arvore l)
obstaculosValidos (Estrada _, l) = not(elem Arvore l || elem Tronco l)
obstaculosValidos (Relva ,l) = not(elem Tronco l || elem Carro l)

{- |
== Explicação da função obstaculosValidos_Mapa:

  A função 'obstaculosValidos_Mapa' recebe uma mapa e entrega um Boleano.

  Em primeiro lugar,testamos para todos os pares (Terreno,[Obstáculos]),através da função 'map' (retorna uma lista construída aplicando uma função (o primeiro argumento) a todos os itens em uma lista passada como segundo argumento), 
  se existe ,('elem'), um False nessa lista. Caso haja, dará True, i.e, existe algum obstáculo num terreno impróprio .

  De seguida, utiliza a função 'not', para negar o Boleano, pois, se têm um obstáculo, o mapa é inválido, ou seja, (False).

== Exemplos de utilização:

>>> obstaculosValidos_Mapa (Mapa 3 [(Relva,[Arvore,Nenhum,Tronco])])
False
>>> obstaculosValidos_Mapa (Mapa 3 [(Estrada 2,[Arvore,Nenhum,Nenhum])])
False
>>> obstaculosValidos_Mapa (Mapa 3 [(Estrada 2,[Carro,Nenhum,Nenhum])])
True
-}

obstaculosValidos_Mapa :: Mapa -> Bool
obstaculosValidos_Mapa (Mapa l x) = not(elem False (map obstaculosValidos x)) 

{- |
2ª Restrição - Rios contíguos têm direções opostas.

== Explicação da função observarRio:

  A função auxiliar 'observarRio' recebe uma mapa e entrega uma lista de inteiros

  Esta função é uma função definida para fazer uma lista das velocidades dos terrenos, mas que apenas interessam as velocidades do Rio.

  Caso o Mapa tenha um rio, desenvolverá o Int da Velocidade. 

  Caso apareça outro terreno, desenvolverá um Int 0.

== Exemplos de utilização:

>>> observarRio (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Tronco,Nenhum,Nenhum]),(Rio 3 ,[Tronco,Nenhum,Nenhum])]))
[0,2,3]
>>> riosContiguos ( observarRio (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Tronco,Nenhum,Nenhum]),(Rio (-3) ,[Tronco,Nenhum,Nenhum])]))
[0,2,-3]
-}

observarRio :: Mapa -> [Int]
observarRio (Mapa l []) = []
observarRio (Mapa l ((Rio v, x):ts)) = v : (observarRio (Mapa l ts))
observarRio (Mapa l ((Estrada v, x):ts)) = 0 : (observarRio (Mapa l ts))
observarRio (Mapa l ((Relva, x):ts)) = 0 : (observarRio (Mapa l ts))


{- |
== Explicação da função riosContiguos:

  A função principal 'riosContiguos' recebe uma lista de inteiros e entrega um Boleano.

  Para que tenham direções opostas é necessário que as suas velocidades sejam, alternadamente, uma positiva e outro negativa.

  Para provar que isto acontece, utilza a soma da função 'signum' (retorna -1 para números negativos, 0 para zero e 1 para números positivos), das primeiras duas velocidades,
  de 'observarRio', e caso a igualdade seja 0, ou o primeiro ou segundo Int seja 0, a função dá True e continua para o resto da lista.


== Exemplos de utilização:
>>> riosContiguos ( observarRio (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Tronco,Nenhum,Nenhum]),(Rio 3 ,[Tronco,Nenhum,Nenhum])]))
False
>>> riosContiguos ( observarRio (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Tronco,Nenhum,Nenhum]),(Rio (-3) ,[Tronco,Nenhum,Nenhum])]))
True
-}

riosContiguos :: [Int] -> Bool
riosContiguos []  = True
riosContiguos [x] = True
riosContiguos (x1:x2:xs) | ((signum x1 + signum x2)==0 || x1==0 || x2==0) = True && riosContiguos (x2:xs)
                         | otherwise = False


{- |
3ª Restrição - Troncos têm, no máximo, 5 unidades de comprimento.

== Explicação da função listaTronco:

   A função auxiliar 'listaTronco' é identica á função group, em que, que agrupa elementos iguais e consecutivos de uma lista.
   Neste caso irá agrupar obstaculos iguais e consecutivos.

== Exemplos de utilização:

>>> ListaTronco  [Tronco,Nenhum,Nenhum,Tronco]
[[Tronco],[Nenhum,Nenhum],[Tronco]]
-}

listaTronco :: [Obstaculo] -> [[Obstaculo]] 
listaTronco [] = []
listaTronco [x] = [[x]]
listaTronco (x:xs) | (elem x (head y)) = (x : (head y)) : tail y
                                  | otherwise = [x] : y
                            where y = listaTronco xs

{- |

== Explicação da função comprimentoValidoTronco:

  A função principal 'comprimentoValidoTronco' recebe um mapa e entrega Boleano.

  Para que não existem 5 Troncos seguidos, temos que ter noção que o último obstáculo irá juntar com o primeiro, ou vise-versa, quando atravessa a largura do mapa.
  Algo parecido com um loop.

  Em primeiro lugar, observar se a head da primeira lista, é um Tronco ou não. 
  Caso se verifique o mesmo, verifica-se o comprimento dessa lista, e se for maior que 5 (Limite de troncos), o compriemnto é inválido (False).

  Outro caso, em que se verifica um Tronco na head primeira lista de listas e na head da ultima, soma-se o comprimento das mesmas e caso a soma seja maior que 5, será inválido (False)
  Caso, não se verifique nenhum Tronco, a função continua, recursivamente, para o resto da lista, até se obter uma lista vazia, que resultará num True.

== Exemplos de utilização:

>>> comprimentoValidoTronco (Mapa 7 [(Rio 2 ,[Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore])])
True
>>> comprimentoValidoTronco (Mapa 7 [(Rio 2 ,[Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore])])
False 
-}

comprimentoValidoTronco :: Mapa -> Bool
comprimentoValidoTronco (Mapa l ((Rio x, (obst:obst')):ts)) | Tronco ==  head(head(listaTronco (obst:obst'))) 
                                                        && length (head(listaTronco(obst:obst'))) >= 6 = False
                                                            |  Tronco ==  head(head(listaTronco (obst:obst'))) 
                                                        && Tronco == head(last(listaTronco (obst:obst')))
                                                        && (length (head(listaTronco (obst:obst')))) + (length(last(listaTronco (obst:obst')))) >= 6 = False
                                                            | Tronco /=  head(head(listaTronco (obst:obst'))) 
                                                        && comprimentoValidoTronco (Mapa l ((Rio x,(obst')):ts)) = True && comprimentoValidoTronco (Mapa l ts)
                                                            | otherwise = True && comprimentoValidoTronco (Mapa l ts)

comprimentoValidoTronco (Mapa l []) = True
comprimentoValidoTronco (Mapa l ((_, (obst:obst')):ts)) = True && comprimentoValidoTronco (Mapa l ts)

                          
{- |
4ª Restrição - Carros têm, no máximo, 3 unidades de comprimento.

== Explicação da função comprimentoValidoCarro:

   A explicção da função 'comprimentoValidoCarro' é exatamente igual À função anterior (comprimentoValidoTronco).
   A única diferença é que a função em vez de observar, se o obstáculo é um tronco, observa-se se é um Carro.

== Exemplos de utilização:
>>> comprimentoValidoCarro (Mapa 7 [(Rio 2 ,[Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro])])
True
>>> comprimentoValidoCarro (Mapa 7 [(Rio 2 ,[Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro])])
False 
-}

comprimentoValidoCarro :: Mapa -> Bool
comprimentoValidoCarro (Mapa l ((Estrada x, (obst:obst')):ts))  | Carro ==  head(head(listaCarros (obst:obst'))) 
                                                        && length (head(listaCarros(obst:obst'))) >= 4 = False
                                                            |  Carro ==  head(head(listaCarros (obst:obst'))) 
                                                        && Carro == head(last(listaCarros (obst:obst'))) 
                                                        && (length (head(listaCarros (obst:obst')))) + (length(last(listaCarros (obst:obst')))) >= 4 = False
                                                            | Carro /=  head(head(listaCarros (obst:obst'))) 
                                                        && comprimentoValidoCarro (Mapa l ((Estrada x,(obst')):ts)) = True && comprimentoValidoCarro (Mapa l ts)
                                                            | otherwise = True && comprimentoValidoCarro (Mapa l ts)
comprimentoValidoCarro (Mapa l []) = True
comprimentoValidoCarro (Mapa l ((_, (obst:obst')):ts)) = True && comprimentoValidoCarro (Mapa l ts)

listaCarros :: [Obstaculo] -> [[Obstaculo]] 
listaCarros [] = []
listaCarros [x] = [[x]]
listaCarros (x:xs) | (elem x (head y)) = (x : (head y)) : tail y
                                  | otherwise = [x] : y
                            where y = listaCarros xs
                            

{- |
5ª Restrição - Em qualquer linha existe, no m ́ınimo, um “obstáculo” Nenhum. Ou
seja, uma linha não pode ser composta exclusivamente por obstáculos,
precisando de haver pelo menos um espaço livre.

== Explicação da função minimoUmNenhum:

   A função principal 'minimoUmNenhum' recebe um mapa e entrega Boleano.

   Para o funcionamento desta função, observamos se existe um Nenhum, na lista de obstáculos de um determinado terreno.
   Caso haja, a função continua recursivamente a procurar um Nenhum em todos os terrenos colocados em Mapa.

   Se encontrar 'Nenhum' em todos os terrenos é True, caso contrário, False.


== Exemplos de utilização:

>>> minimoUmNenhum (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Tronco,Nenhum,Tronco])]))
True
>>> minimoUmNenhum (Mapa 3 [(Relva,[Arvore,Arvore,Arvore]),(Estrada 2 ,[Carro,Carro,Carro])])
False 
-}

minimoUmNenhum :: Mapa -> Bool
minimoUmNenhum (Mapa l []) = True
minimoUmNenhum (Mapa l ((t, x@(obst:obst')):ts)) | elem Nenhum x = True && minimoUmNenhum (Mapa l ts)
                                                 | otherwise = False

{- |
6ª Restrição - O comprimento da lista de obstáculos de cada linha corresponde exactamente à largura do mapa.

== Explicação da função larguraMapa:

   A função principal 'larguraMapa' recebe um mapa e entrega Boleano.

   Para o funcionamento desta função, observamos se o comprimento da lista da obstaculos é igual à largura.
   Caso seja igual, a função é 'True' e continua para o resto dos terrenos.
   Caso seja diferente, dá-nos um 'False'.

== Exemplos de utilização:
>>> larguraMapa (Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Nenhum,Nenhum,Tronco])])
True
>>> larguraMapa (Mapa 3 [(Relva,[Nenhum,Nenhum]),(Estrada 2 ,[Carro,Nenhum,Carro]),(Rio 2 , [Nenhum,Nenhum,Tronco])])
False 
-}

larguraMapa :: Mapa -> Bool
larguraMapa (Mapa l []) = True
larguraMapa (Mapa l ((t, x@(obst:obst')):ts)) | length x == l = True && larguraMapa (Mapa l ts)
                                              | otherwise = False

{- |
7ª Restrição - Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas ou relvas

== Explicação da função observarTerrenos.

  A função auxiliar 'observarTerrenos' recebe uma mapa e entrega uma lista de inteiros

  Esta função é uma função definida para costruir uma lista das velocidades dos terrenos.

  Caso o Mapa tenha um Rio, desenvolverá o Int 1.
  Caso o Mapa tenha uma Estrada ou Relva, desenvolverá o Int 2.   


== Exemplos de utilização:

>>> observarTerrenos (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 2 ,[Tronco,Nenhum,Nenhum]),(Rio 3 ,[Tronco,Nenhum,Nenhum])]))
[2,1,1]
-}

observarTerrenos :: Mapa -> [Int]
observarTerrenos (Mapa l []) = []
observarTerrenos (Mapa l ((Rio v, x):ts)) = 1 : (observarTerrenos (Mapa l ts))
observarTerrenos (Mapa l ((Estrada v, x):ts)) = 2 : (observarTerrenos (Mapa l ts))
observarTerrenos (Mapa l ((Relva, x):ts)) = 2 : (observarTerrenos (Mapa l ts))

{- |
== Explicação da função listaInt:

  A função auxiliar 'listaInt' é identica á função group, em que, que agrupa elementos iguais e consecutivos de uma lista.
  Neste caso irá agrupar inteiros iguais e consecutivos.

== Exemplos de utilização:

>>> listaInt [1,2,2,1,1]
[[1],[2,2],[1,1]]
-}

listaInt :: [Int] -> [[Int]] 
listaInt [] = []
listaInt [x] = [[x]]
listaInt (x:xs) | (elem x (head y)) = (x : (head y)) : tail y
                                  | otherwise = [x] : y
                            where y = listaInt xs

{- |
== Explicação da função limiteTerreno:

  A função 'limiteTerreno' recebe uma lista de listas de inteiros (que será o resultado da 'listInt') e entrega um Boleano

  Caso a primeira lista seja '1', ou seja, Rio, e o seu comprimento for menor que 4, está correta e irá testar para o resto da lista de listas.
  
  Caso a primeira lista seja '1', ou seja, Rio, e o seu comprimento for maior que 4 é Falso, pois não pode haver 4 rios seguidos.

  Caso a primeira lista seja '2', ou seja, Estrada ou Relva, e o seu comprimento for menor que 5, está correta e irá testar para o resto da lista de listas.

  Caso a primeira lista seja '2', ou seja, Estrada ou Relva, e o seu comprimento for maior que 5 é Falso, pois não pode haver 5 Estradas ou Relvas seguidas.

  Caso não se observe, nenhuma destas condições, o mapa é válido.

== Exemplos de utilização:

>>> limiteTerreno [[1],[2,2],[1,1]]
True
>>> limiteTerreno [[1],[2,2,2,2,2,2],[1,1]]
False 
>>> limiteTerreno [[1,1,1,1,1],[2,2,2],[1,1]]
False 
-}

limiteTerreno :: [[Int]] -> Bool
limiteTerreno [] = True
limiteTerreno (l@(x:xs):t) | x==1 && length l <= 4 = limiteTerreno t
                           | x==1 && length l > 4 = False
                           | x== 2 && length l <=5 = limiteTerreno t
                           | x== 2 && length l > 5 = False
                           | otherwise = True





