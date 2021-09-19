import Data.Typeable

a :: Formula
a = Var $ Nome "a"

b :: Formula
b = Var $ Nome "b"

main = printArvore (prova (Implicacao a (Implicacao a (Implicacao b a))))

data Variavel = Nome String

-- Estrutura de dados para uma formula
data Formula =
    Var Variavel |
    Not Formula |
    And Formula Formula |
    Or Formula Formula |
    Implicacao Formula Formula


-- Eu marco os Nodes como ramo fechado desde que todos os filhos dele estejam fechados
data Node = Node {
    formula :: Formula,
    ramoFechado :: Bool,
    processado :: Bool
}

-- NodeFechado serve basicamente para facilitar no print da árvore,
-- quando eu for fechar um ramo, vou inserir à esquerda dele NodeFechado
-- para printar carácter indicando ramo fechado
-- Null indica que não existe caminho por ali (por exemplo, p & q terá só um filho)
-- Todas as folhas serão Null ou NodeFechado
-- É possível dizer que essa é uma árvore de prova caso todos os Nodes tenham sido processados (regra aplicada) e existe alguma Node que ramoFechado = False
-- E, é possível dizer que é uma árvore de refutação com regra semelhante à anterior porém com todos Nodes ramoFechado = True
data Arvore =
    Null |
    NodeFechado |
    NodeIntermediario Node Arvore Arvore


implicacao :: Formula -> Formula -> Formula
implicacao formula1 formula2 = Or (Not formula1) formula2


demorgan :: Formula -> Formula
demorgan (And formula1 formula2) = Or (Not formula1) (Not formula2)

demorgan (Or formula1 formula2) = And (Not formula1) (Not formula2)

simplifica :: Formula -> Formula
simplifica (Not (Not formula)) = formula
simplifica (Not (And formula1 formula2)) = demorgan(And formula1 formula2)
simplifica (Not (Or formula1 formula2)) = demorgan(Or formula1 formula2)
simplifica (Implicacao formula1 formula2) = implicacao formula1(formula2)
simplifica (Not (Implicacao formula1 formula2)) = Not(implicacao formula1(formula2))
simplifica formula = formula  -- se cheguei aqui, já tá simplificado

-- temAbsurdo :: [Formula] -> Variavel -> Bool
-- temAbsurdo ([Var variavel])(Nome nome) = False
-- -- temAbsurdo ([Not formula])(Nome nome) = not temAbsurdo([formula])(nome)
-- temAbsurdo ([_])(Nome nome) = False


printVariavel :: Variavel -> IO()
printVariavel (Nome nome) = putStr nome

printFormula :: Formula -> IO()
printFormula (Var variavel) = printVariavel variavel

printFormula formula = putStr(formulaToStr formula)

printNode :: Node -> IO()
printNode node = putStr(nodeToStr node)

printArvore :: Arvore -> IO()
printArvore a =
    printArvoreAux a 0

arvoreStrPossuiNoNotNull :: String -> Bool -> Bool
arvoreStrPossuiNoNotNull (x:xs) haveNonParentese
    | haveNonParentese = True
    | otherwise = arvoreStrPossuiNoNotNull xs (not(isBracket x ))

arvoreStrPossuiNoNotNull [] haveNonParentese = haveNonParentese


isBracket :: Char -> Bool
isBracket '[' = True
isBracket ']' = True
isBracket x = False

printArvoreAux :: Arvore -> Int -> IO()
printArvoreAux a nivelDesejado = do
    if arvoreStrPossuiNoNotNull(arvoreNivelToStr a 0 nivelDesejado) False
    then do
        putStrLn(arvoreNivelToStr a 0 nivelDesejado)
        printArvoreAux a (nivelDesejado + 1)
    else
        putStr""

concatStrings :: String -> String -> String
concatStrings a b = a ++ b

arvoreNivelToStr :: Arvore -> Int -> Int -> String
arvoreNivelToStr (NodeIntermediario n e d) nivelAtual nivelDesejado
    | nivelAtual == nivelDesejado = arvoreNivelToStrAux(NodeIntermediario n e d)
    | nivelAtual < nivelDesejado = do
        concatStrings (arvoreNivelToStr e (nivelAtual + 1) nivelDesejado) (arvoreNivelToStr d (nivelAtual + 1) nivelDesejado)
    | otherwise = ""

arvoreNivelToStr NodeFechado nivelAtual nivelDesejado
    | nivelAtual == nivelDesejado = arvoreNivelToStrAux NodeFechado
    | otherwise = ""

arvoreNivelToStr Null nivelAtual nivelDesejado
    | nivelAtual == nivelDesejado = arvoreNivelToStrAux Null
    | otherwise = ""

variavelToStr :: Variavel -> String
variavelToStr(Nome nome) = nome

formulaToStr :: Formula -> String
formulaToStr(Var variavel) = variavelToStr variavel
formulaToStr(Not formula) = "~("++formulaToStr formula++")"
formulaToStr(And formula1 formula2) = "("++formulaToStr formula1++" & " ++ formulaToStr formula2 ++ ")"
formulaToStr(Or formula1 formula2) = "("++formulaToStr formula1++" | " ++ formulaToStr formula2 ++ ")"
formulaToStr(Implicacao formula1 formula2) = "("++formulaToStr formula1++" -> " ++ formulaToStr formula2 ++ ")"


nodeToStr :: Node -> String
nodeToStr(Node formula _ _) = formulaToStr formula

arvoreNivelToStrAux :: Arvore -> String
arvoreNivelToStrAux (NodeIntermediario node _ _) = "["++nodeToStr node++"]"

arvoreNivelToStrAux NodeFechado = "[X]"

arvoreNivelToStrAux Null = "[]"

-- Essa função vai estar inserindo em todas as folhas visto que devo aplicar a fórmula em todas as folhas
insereNode1 :: Arvore -> Node -> Arvore
insereNode1 a n = insereArvore a (NodeIntermediario n Null Null) Null

insereNode2 :: Arvore -> Node -> Node -> Arvore
insereNode2 a n1 n2 = insereArvore a (NodeIntermediario n1 Null Null) (NodeIntermediario n2 Null Null)

insereArvore :: Arvore -> Arvore -> Arvore -> Arvore
insereArvore (NodeIntermediario node Null Null) a1 a2 = NodeIntermediario node a1 a2
insereArvore Null a1 a2 = Null
insereArvore NodeFechado a1 a2 = NodeFechado
insereArvore (NodeIntermediario node e d) a1 a2 = NodeIntermediario node (insereArvore e a1 a2) (insereArvore d a1 a2)

fechaNodesAbaixo :: Arvore -> Arvore
-- se for "folha"
fechaNodesAbaixo (NodeIntermediario (Node formula ramoFechado processado) Null Null) = NodeIntermediario (Node formula True processado) NodeFechado Null
-- se for nó intermediário
fechaNodesAbaixo (NodeIntermediario (Node formula ramoFechado processado) e d) = NodeIntermediario (Node formula True processado) (fechaNodesAbaixo e)  (fechaNodesAbaixo d)
fechaNodesAbaixo Null = Null

-- esse caso pode ocorrer caso um ramo já esteja fechado anteriormente e o outro não
fechaNodesAbaixo NodeFechado = NodeFechado

variavelEqual :: Variavel -> Variavel -> Bool
variavelEqual (Nome nome1) (Nome nome2) = nome1 == nome2

formulaEqual :: Formula -> Formula -> Bool
formulaEqual (Var variavel1) (Var variavel2) = variavelEqual variavel1 variavel2
formulaEqual (Not formula1) (Not formula2) = formulaEqual formula1 formula2
formulaEqual (And formula1 formula2) (And formula3 formula4) = formulaEqual formula1 formula3 && formulaEqual formula2 formula4
formulaEqual (Or formula1 formula2) (Or formula3 formula4) = formulaEqual formula1 formula3 && formulaEqual formula2 formula4
formulaEqual (Implicacao formula1 formula2) (Implicacao formula3 formula4) = formulaEqual formula1 formula3 && formulaEqual formula2 formula4

formulaEqual _ _= False

prova :: Formula -> Arvore
prova formula = provaAux(NodeIntermediario (Node (Not formula) False False) Null Null)

provaAux :: Arvore -> Arvore
provaAux (NodeIntermediario (Node formula False False) e d)
    | not(formulaEqual formula (simplifica formula)) = 
        provaAux (insereNode1 a (Node (simplifica formula) False False)) where
            a = NodeIntermediario (Node formula False True) e d

provaAux (NodeIntermediario (Node (And formula1 formula2) False False) e d) = 
    provaAux(insereNode1 a1 (Node formula2 False False)) where
        a1 =insereNode1 a (Node formula1 False False) where
            a = NodeIntermediario (Node (And formula1 formula2) False True) e d

provaAux (NodeIntermediario (Node (Or formula1 formula2) False False) e d) =
    provaAux(insereNode2 a (Node formula1 False False) (Node formula2 False False)) where
        a = NodeIntermediario (Node (And formula1 formula2) False True) e d

provaAux (NodeIntermediario (Node formula False True) e d) = NodeIntermediario (Node formula False True) (provaAux e) (provaAux d)
provaAux arvore = arvore
