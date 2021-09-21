-- TODO: imprimir um ramo de cada vez

module Tableaux where

import Data.Typeable

data Variable = Name String

-- Estrutura de dados para uma formula
data Formula =
    Var Variable |
    Not Formula |
    And Formula Formula |
    Or Formula Formula |
    Implication Formula Formula


-- Eu marco os Nodes como ramo fechado desde que todos os filhos dele estejam fechados
data Node = Node {
    formula :: Formula,
    processed :: Bool
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

printVariable :: Variable -> IO()
printVariable (Name nome) = putStr nome

printFormula :: Formula -> IO()
printFormula (Var variavel) = printVariable variavel

printFormula formula = putStr(formulaToStr formula)

printArvore :: Arvore -> IO()
printArvore a =
    printArvoreAux a 0

printArvoreAux :: Arvore -> Int -> IO()
printArvoreAux a nivelDesejado = do
    if arvoreStrPossuiNoNotNull(arvoreNivelToStr a 0 nivelDesejado) False
    then do
        putStrLn(arvoreNivelToStr a 0 nivelDesejado)
        printArvoreAux a (nivelDesejado + 1)
    else
        putStr""

arvoreStrPossuiNoNotNull :: String -> Bool -> Bool
arvoreStrPossuiNoNotNull (x:xs) haveNonParentese
    | haveNonParentese = True
    | otherwise = arvoreStrPossuiNoNotNull xs (not(isBracket x ))

arvoreStrPossuiNoNotNull [] haveNonParentese = haveNonParentese


isBracket :: Char -> Bool
isBracket '[' = True
isBracket ']' = True
isBracket x = False

arvoreNivelToStr :: Arvore -> Int -> Int -> String
arvoreNivelToStr (NodeIntermediario n e d) nivelAtual nivelDesejado
    | nivelAtual == nivelDesejado = arvoreNivelToStrAux(NodeIntermediario n e d)
    | nivelAtual < nivelDesejado = do
        (arvoreNivelToStr e (nivelAtual + 1) nivelDesejado) ++ (arvoreNivelToStr d (nivelAtual + 1) nivelDesejado)
    | otherwise = ""

arvoreNivelToStr NodeFechado nivelAtual nivelDesejado
    | nivelAtual == nivelDesejado = arvoreNivelToStrAux NodeFechado
    | otherwise = ""

arvoreNivelToStr Null nivelAtual nivelDesejado
    | nivelAtual == nivelDesejado = arvoreNivelToStrAux Null
    | otherwise = ""

variavelToStr :: Variable -> String
variavelToStr(Name nome) = nome

formulaToStr :: Formula -> String
formulaToStr(Var variavel) = variavelToStr variavel
formulaToStr(Not formula) = "~("++formulaToStr formula++")"
formulaToStr(And formula1 formula2) = "("++formulaToStr formula1++" & " ++ formulaToStr formula2 ++ ")"
formulaToStr(Or formula1 formula2) = "("++formulaToStr formula1++" | " ++ formulaToStr formula2 ++ ")"
formulaToStr(Implication formula1 formula2) = "("++formulaToStr formula1++" -> " ++ formulaToStr formula2 ++ ")"


nodeToStr :: Node -> String
nodeToStr(Node formula p) = formulaToStr formula

arvoreNivelToStrAux :: Arvore -> String
arvoreNivelToStrAux (NodeIntermediario node _ _) = "["++nodeToStr node++"]"

arvoreNivelToStrAux NodeFechado = "[X]"

arvoreNivelToStrAux Null = "[*]"

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
fechaNodesAbaixo (NodeIntermediario node Null Null) = NodeIntermediario node NodeFechado Null
-- se for nó intermediário
fechaNodesAbaixo (NodeIntermediario node e d) = NodeIntermediario node (fechaNodesAbaixo e)  (fechaNodesAbaixo d)
fechaNodesAbaixo Null = Null

-- esse caso pode ocorrer caso um ramo já esteja fechado anteriormente e o outro não
fechaNodesAbaixo NodeFechado = NodeFechado

variavelEqual :: Variable -> Variable -> Bool
variavelEqual (Name nome1) (Name nome2) = nome1 == nome2

-- verifica se é Var var ou (Not Var var)
formulaIsSimple :: Formula -> Bool
formulaIsSimple (Var var) = True
formulaIsSimple (Not (Var var)) = True
formulaIsSimple _ = False

prova :: Formula -> Arvore
prova formula = provaAux(NodeIntermediario (Node (Not formula) False) Null Null)

provaAux :: Arvore -> Arvore
provaAux a = atualizaRamosFechados (provaAux1 a)

provaAux1 :: Arvore -> Arvore

-- v: a -> b ==> f: a / v: b
provaAux1 (NodeIntermediario (Node (Implication formula1 formula2) False) e d) =
    provaAux1(insereNode2 a (Node (Not formula1) False) (Node formula2 False)) where
        a = NodeIntermediario (Node (Implication formula1 formula2) True) e d

-- f: a -> b ==> v: a ; f: b
provaAux1 (NodeIntermediario (Node (Not (Implication formula1 formula2)) False) e d) =
    provaAux1(insereNode1 a1 (Node (Not formula2) False)) where
        a1 =insereNode1 a (Node formula1 False) where
            a = NodeIntermediario (Node (Not (Implication formula1 formula2)) True) e d

-- v: a & b ==> v: a ; v : b
provaAux1 (NodeIntermediario (Node (And formula1 formula2) False) e d) =
    provaAux1(insereNode1 a1 (Node formula2 False)) where
        a1 =insereNode1 a (Node formula1 False) where
            a = NodeIntermediario (Node (And formula1 formula2) True) e d

-- f: a & b ==> f: a / f : b
provaAux1 (NodeIntermediario (Node (Not (And formula1 formula2)) False) e d) =
    provaAux1(insereNode2 a (Node (Not formula1) False) (Node (Not formula2) False)) where
        a = NodeIntermediario (Node (Not (And formula1 formula2)) True) e d

-- v: a | b ==> v: a / v: b
provaAux1 (NodeIntermediario (Node (Or formula1 formula2) False) e d) =
    provaAux1(insereNode2 a (Node formula1 False) (Node formula2 False)) where
        a = NodeIntermediario (Node (Or formula1 formula2) True) e d

-- f: a | b ==> f: a ; f: b
provaAux1 (NodeIntermediario (Node (Not (Or formula1 formula2)) False) e d) =
    provaAux1(insereNode1 a1 (Node (Not formula2) False)) where
        a1 =insereNode1 a (Node (Not formula1) False) where
            a = NodeIntermediario (Node (Not (Or formula1 formula2)) True) e d

-- f: ~a ==> v: a
provaAux1 (NodeIntermediario (Node (Not (Not formula1)) False) e d) =
    provaAux1 (insereNode1 a (Node formula1 False)) where
        a = NodeIntermediario (Node (Not (Not formula1)) True) e d

-- Se a fórmula for uma variável ou a negação de uma variável, só considero como processada e sigo em frente
provaAux1 (NodeIntermediario (Node (Var var) False) e d) = provaAux1 (NodeIntermediario (Node (Var var) True) e d)
provaAux1 (NodeIntermediario (Node (Not (Var var)) False) e d) = provaAux1 (NodeIntermediario (Node (Not (Var var)) True) e d)

-- Se eu chamar a função para um nó que já foi processado, eu processo os seus ramos à esquerda ou direita
provaAux1 (NodeIntermediario (Node formula True) e d) = NodeIntermediario (Node formula True) (provaAux1 e) (provaAux1 d)

-- Só chega aqui se for Null ou NodeFechado, ou seja, nada a ser feito para eles
provaAux1 Null = Null
provaAux1 NodeFechado = NodeFechado

contradiz :: Formula -> Formula -> Bool
contradiz (Not (Var var1)) (Var var2) = variavelEqual var1 var2
contradiz (Var var2) (Not (Var var1)) = variavelEqual var1 var2
contradiz _ _ = False

atualizaRamosFechados :: Arvore -> Arvore

atualizaRamosFechados (NodeIntermediario (Node formula processed) e d) 
    | formulaIsSimple formula =
    NodeIntermediario (Node formula processed) (atualizaRamosFechados arvE) (atualizaRamosFechados arvD) where
        arvE = buscaContradicao formula e
        arvD = buscaContradicao formula d

atualizaRamosFechados (NodeIntermediario node e d) = NodeIntermediario node (atualizaRamosFechados e) (atualizaRamosFechados d)
atualizaRamosFechados Null = Null
atualizaRamosFechados NodeFechado = NodeFechado

buscaContradicao :: Formula -> Arvore -> Arvore
buscaContradicao formula (NodeIntermediario (Node formula1 processed) e d) 
    | formulaIsSimple formula1 && contradiz formula formula1 =
        fechaNodesAbaixo (NodeIntermediario (Node formula1 processed) e d)

buscaContradicao formula (NodeIntermediario node e d) = NodeIntermediario node (buscaContradicao formula e) (buscaContradicao formula d)

buscaContradicao formula Null = Null
buscaContradicao formula NodeFechado = NodeFechado

possuiNodeAberto :: Arvore -> Bool
possuiNodeAberto NodeFechado = False
possuiNodeAberto (NodeIntermediario node Null Null) = True
possuiNodeAberto (NodeIntermediario node Null d) = possuiNodeAberto d
possuiNodeAberto (NodeIntermediario node e Null) = possuiNodeAberto e
possuiNodeAberto (NodeIntermediario node e d) = possuiNodeAberto e || possuiNodeAberto d

printProva :: Formula -> IO()
printProva formula = do
    putStrLn "\n\n######################"
    putStr "Árvore de prova/refutação da fórmula "
    printFormula formula
    putStr ": \n\n"
    printArvore arvoreProva
    putStr "\n\n"
    if possuiNodeAberto arvoreProva
    then
        putStr "A fórmula é falsificável"
    else
        putStr "A fórmula é uma tautologia"
    putStr "\n"
    where
        arvoreProva = prova formula


