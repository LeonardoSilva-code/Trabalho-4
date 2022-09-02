-- Leonardo Silva de Abreu

-- função auxiliar para verificar se um número inteiro é multiplo de outro número inteiro
multiploDe :: Int -> Int -> Bool
multiploDe x y = mod y x == 0

{-
1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr devolva o fatorial de n.
-}
fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1 .. n]

{-
2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados.
-}
quadradoReal :: [Float] -> [Float]
quadradoReal n = map (^^2) n

{-
3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.
-}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras n = map length n

{-
4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29.
-}
maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum (filter (multiploDe 29) [0 .. 100000])

{-
5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.
-}
maiorMultiploDe :: Int -> Int
maiorMultiploDe n = maximum (filter (multiploDe n) [0 .. 100000])

{-
6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 12 + 22 + 32 + 42. . . +𝑛2.
-}
somaQuadrados :: [Int] -> Int
somaQuadrados n = foldr (+) 0 (map (^2) n)

{-
7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada
-}
acc :: Int -> n -> Int
acc x _ = x + 1

comprimento :: [a] -> Int
comprimento n = foldl(acc) 0 n

{-
8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.
-}
--A função flip inverte a ordem dos argumentos de uma função
maiorQue :: Int -> Int -> Bool
maiorQue x y = flip (<) x y

divisaoReversa :: Float -> Float -> Float
divisaoReversa x y = flip (/) x y

--A função max recebe dois argumentos e retorna o maior 
maiorInteiro :: Int -> Int -> Int
maiorInteiro x y = max x y


ordemAlfabetica :: Char -> Char -> Char
ordemAlfabetica a b = max a b

--A função min retorna o menor de dois argumentos
menorInteiro :: Int -> Int -> Int
menorInteiro x y = min x y

ordemAlfabeticaDescendente :: Char -> Char -> Char
ordemAlfabeticaDescendente a b = min a b

main = do
putStrLn "=== Trabalho 4 ==="

--fatorialn teste
let fatorialnInput = 5
let fatorialnResultado = fatorialn fatorialnInput
putStrLn ("Func. fatorialn: entrada:" ++ show fatorialnInput ++ "; resultado:" ++ show fatorialnResultado)

--quadradoReal teste
let quadradoRealInput = [2.0,3.0,-5.0,-9.0]
let quadradoRealResultado = quadradoReal quadradoRealInput
putStrLn ("Func. quadradoReal: entrada:" ++ show quadradoRealInput ++ "; resultado:" ++ show quadradoRealResultado)

--comprimentoPalavras teste
let comprimentoPalavrasInput = ["Haskell", " Is ", "fun", "!"]
let comprimentoPalavrasResultado = comprimentoPalavras comprimentoPalavrasInput
putStrLn ("Func. comprimentoPalavras: entrada:" ++ show comprimentoPalavrasInput ++ "; resultado:" ++ show comprimentoPalavrasResultado)

--maiorMultiploDe29 teste
let maiorMultiploDe29Resultado = maiorMultiploDe29
putStrLn ("Func. maiorMultiploDe29: entrada:[0 .. 100000]"  ++ "; resultado:" ++ show maiorMultiploDe29Resultado)

--maiorMultiploDe teste
let maiorMultiploDeInput = 33
let maiorMultiploDeResultado = maiorMultiploDe maiorMultiploDeInput
putStrLn ("Func. maiorMultiploDe: entrada:" ++ show maiorMultiploDeInput ++ "; resultado:" ++ show maiorMultiploDeResultado)

--somaQuadrados teste
let somaQuadradosInput = [1,2,3,4,5]
let somaQuadradosResultado = somaQuadrados somaQuadradosInput
putStrLn ("Func. somaQuadrados: entrada:" ++ show somaQuadradosInput ++ "; resultado:" ++ show somaQuadradosResultado)

--comprimento teste1
let comprimentoInput = ["a","bb","ccc","4"]
let comprimentoResultado = comprimento comprimentoInput
putStrLn ("Func. comprimento: entrada:" ++ show comprimentoInput ++ "; resultado:" ++ show comprimentoResultado)

--comprimento teste2
let comprimentoInput = [1,2,3,4,5]
let comprimentoResultado = comprimento comprimentoInput
putStrLn ("Func. comprimento: entrada:" ++ show comprimentoInput ++ "; resultado:" ++ show comprimentoResultado)

--flip teste
let maiorQueInput = 4
let maiorQueInput2 = 5
let maiorQueResultado = maiorQue maiorQueInput maiorQueInput2
putStrLn ("Func. maiorQue: entrada:" ++ show maiorQueInput ++ ", " ++ show maiorQueInput2 ++"; resultado:" ++ show maiorQueResultado)

let divisaoReversaInput = 1
let divisaoReversaInput2 = 2
let divisaoReversaResultado = divisaoReversa divisaoReversaInput divisaoReversaInput2
putStrLn ("Func. divisaoReversa: entrada:" ++ show divisaoReversaInput ++ ", " ++ show divisaoReversaInput2 ++"; resultado:" ++ show divisaoReversaResultado)

--max teste
let maiorInteiroInput = 4
let maiorInteiroInput2 = 5
let maiorInteiroResultado = maiorInteiro maiorInteiroInput maiorInteiroInput2
putStrLn ("Func. maiorInteiro: entrada:" ++ show maiorInteiroInput ++ ", " ++ show maiorInteiroInput2 ++"; resultado:" ++ show maiorInteiroResultado)

let ordemAlfabeticaInput = 'F'
let ordemAlfabeticaInput2 = 'G'
let ordemAlfabeticaResultado = ordemAlfabetica ordemAlfabeticaInput ordemAlfabeticaInput2
putStrLn ("Func. ordemAlfabetica: entrada:" ++ show ordemAlfabeticaInput ++ ", " ++ show ordemAlfabeticaInput2 ++"; resultado:" ++ show ordemAlfabeticaResultado)

--min teste
let menorInteiroInput = 4
let menorInteiroInput2 = 5
let menorInteiroResultado = menorInteiro menorInteiroInput menorInteiroInput2
putStrLn ("Func. menorInteiro: entrada:" ++ show menorInteiroInput ++ ", " ++ show menorInteiroInput2 ++"; resultado:" ++ show menorInteiroResultado)

let ordemAlfabeticaDescendenteInput = 'F'
let ordemAlfabeticaDescendenteInput2 = 'G'
let ordemAlfabeticaDescendenteResultado = ordemAlfabeticaDescendente ordemAlfabeticaDescendenteInput ordemAlfabeticaDescendenteInput2
putStrLn ("Func. ordemAlfabeticaDescendente: entrada:" ++ show ordemAlfabeticaDescendenteInput ++ ", " ++ show ordemAlfabeticaDescendenteInput2 ++"; resultado:" ++ show ordemAlfabeticaDescendenteResultado)