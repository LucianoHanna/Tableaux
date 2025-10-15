module Tableaux where

data Variable = Name String

-- Estrutura de dados para uma formula
data Formula =
    Var Variable |
    Not Formula |
    And Formula Formula |
    Or Formula Formula |
    Implication Formula Formula

data Node = Node {
    formula :: Formula,
    processed :: Bool
}

-- NodeFechado serve basicamente para facilitar no print da árvore,
-- quando eu for fechar um ramo, vou inserir à esquerda dele NodeFechado
-- para printar carácter indicando ramo fechado
-- Null indica que não existe caminho por ali (por exemplo, p & q terá só um filho)
-- Todas as folhas serão Null ou NodeFechado
-- É possível dizer que essa é uma árvore de refutação (ou seja, a fórmula é falsificável) caso todos os Nodes tenham sido processados (regra aplicada) e existe algum ramo que, o nó intermediário de maior nivel no ramo tenha Null à esquerda e à direita, ou seja, existe um ramo que não está fechado
-- E, é possível dizer que é uma árvore de prova com condição semelhante à anterior porém, com TODOS os nós intermediários de maior nivel no ramo com NodeFechado como filho, ou seja, todos os ramos estão fechados

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
    printArvoreAux a 0 "+"

printNiveis :: Int -> IO()
printNiveis 0 = putStr ""
printNiveis i = do
    putStr "|  "
    printNiveis (i - 1)

ramoEsquerda :: Arvore -> Arvore
ramoEsquerda (NodeIntermediario _ e _) = e
ramoEsquerda _ = Null

ramoDireita :: Arvore -> Arvore
ramoDireita (NodeIntermediario _ _ d) = d
ramoDireita _ = Null

printArvoreAux :: Arvore -> Int -> String -> IO()
printArvoreAux Null _ _ = putStr ""
printArvoreAux a nivel ramo = do
    printNiveis nivel
    putStr ramo
    putStr "- "
    putStrLn (arvoreNivelToStrAux a)
    printArvoreAux (ramoEsquerda a) (nivel+1) "+"
    printArvoreAux (ramoDireita a) (nivel+1) "`"


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
formulaToStr (Var variavel) = variavelToStr variavel
formulaToStr (Not (Var v)) = "￢" ++ variavelToStr v
formulaToStr (Not formula) = "￢("++formulaToStr formula++")"
formulaToStr (And formula1 formula2) = "("++formulaToStr formula1++" ∧ " ++ formulaToStr formula2 ++ ")"
formulaToStr (Or formula1 formula2) = "("++formulaToStr formula1++" ∨ " ++ formulaToStr formula2 ++ ")"
formulaToStr (Implication formula1 formula2) = "("++formulaToStr formula1++" → " ++ formulaToStr formula2 ++ ")"


nodeToStr :: Node -> String
nodeToStr(Node formula _) = formulaToStr formula

arvoreNivelToStrAux :: Arvore -> String
arvoreNivelToStrAux (NodeIntermediario node _ _) = nodeToStr node
arvoreNivelToStrAux NodeFechado = "X"

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
formulaIsAtomic :: Formula -> Bool
formulaIsAtomic (Var var) = True
formulaIsAtomic (Not (Var var)) = True
formulaIsAtomic _ = False

prova :: Formula -> Arvore
prova formula = provaAux(NodeIntermediario (Node formula False) Null Null)


provaAux :: Arvore -> Arvore
provaAux a = atualizaRamosFechados (provaAux1 a)

provaAux1 :: Arvore -> Arvore

-- v: a -> b ==> f: a / v: b
provaAux1 (NodeIntermediario (Node (Implication formula1 formula2) False) e d) =
    provaAux(insereNode2 a (Node (Not formula1) False) (Node formula2 False)) where
        a = NodeIntermediario (Node (Implication formula1 formula2) True) e d

-- f: a -> b ==> v: a ; f: b
provaAux1 (NodeIntermediario (Node (Not (Implication formula1 formula2)) False) e d) =
    provaAux(insereNode1 a1 (Node (Not formula2) False)) where
        a1 =insereNode1 a (Node formula1 False) where
            a = NodeIntermediario (Node (Not (Implication formula1 formula2)) True) e d

-- v: a & b ==> v: a ; v : b
provaAux1 (NodeIntermediario (Node (And formula1 formula2) False) e d) =
    provaAux(insereNode1 a1 (Node formula2 False)) where
        a1 =insereNode1 a (Node formula1 False) where
            a = NodeIntermediario (Node (And formula1 formula2) True) e d

-- f: a & b ==> f: a / f : b
provaAux1 (NodeIntermediario (Node (Not (And formula1 formula2)) False) e d) =
    provaAux(insereNode2 a (Node (Not formula1) False) (Node (Not formula2) False)) where
        a = NodeIntermediario (Node (Not (And formula1 formula2)) True) e d

-- v: a | b ==> v: a / v: b
provaAux1 (NodeIntermediario (Node (Or formula1 formula2) False) e d) =
    provaAux(insereNode2 a (Node formula1 False) (Node formula2 False)) where
        a = NodeIntermediario (Node (Or formula1 formula2) True) e d

-- f: a | b ==> f: a ; f: b
provaAux1 (NodeIntermediario (Node (Not (Or formula1 formula2)) False) e d) =
    provaAux(insereNode1 a1 (Node (Not formula2) False)) where
        a1 =insereNode1 a (Node (Not formula1) False) where
            a = NodeIntermediario (Node (Not (Or formula1 formula2)) True) e d

-- f: ~a ==> v: a
provaAux1 (NodeIntermediario (Node (Not (Not formula1)) False) e d) =
    provaAux (insereNode1 a (Node formula1 False)) where
        a = NodeIntermediario (Node (Not (Not formula1)) True) e d

-- Se a fórmula for uma variável ou a negação de uma variável, só considero como processada e sigo em frente
provaAux1 (NodeIntermediario (Node (Var var) False) e d) = provaAux (NodeIntermediario (Node (Var var) True) e d)
provaAux1 (NodeIntermediario (Node (Not (Var var)) False) e d) = provaAux (NodeIntermediario (Node (Not (Var var)) True) e d)

-- Se eu chamar a função para um nó que já foi processado, eu processo os seus ramos à esquerda ou direita
provaAux1 (NodeIntermediario (Node formula True) e d) = NodeIntermediario (Node formula True) (provaAux e) (provaAux d)

-- Só chega aqui se for Null ou NodeFechado, ou seja, nada a ser feito para eles
provaAux1 Null = Null
provaAux1 NodeFechado = NodeFechado

contradiz :: Formula -> Formula -> Bool
contradiz (Not (Var var1)) (Var var2) = variavelEqual var1 var2
contradiz (Var var2) (Not (Var var1)) = variavelEqual var1 var2
contradiz _ _ = False

-- objetivo dessa função é encontrar nós com fórmula atômica e procurar contradição no filho à esquerda e à direita 
atualizaRamosFechados :: Arvore -> Arvore

-- buscaContradicao retorna a árvore atualizada com o ramo devidamente fechado caso tenha encontrado uma contradição
atualizaRamosFechados (NodeIntermediario (Node formula processed) e d) 
    | formulaIsAtomic formula =
    NodeIntermediario (Node formula processed) (atualizaRamosFechados arvE) (atualizaRamosFechados arvD) where
        arvE = buscaContradicao formula e
        arvD = buscaContradicao formula d

atualizaRamosFechados (NodeIntermediario node e d) = NodeIntermediario node (atualizaRamosFechados e) (atualizaRamosFechados d)
atualizaRamosFechados Null = Null
atualizaRamosFechados NodeFechado = NodeFechado

buscaContradicao :: Formula -> Arvore -> Arvore
-- Caso eu encontre algum nó com fórmula atomica que contradiz à fórmula atomica que ele "herda", então eu fecho todos os ramos que derivam dele
buscaContradicao formula (NodeIntermediario (Node formula1 processed) e d) 
    | formulaIsAtomic formula1 && contradiz formula formula1 =
        fechaNodesAbaixo (NodeIntermediario (Node formula1 processed) e d)

-- Caso a fórmula não seja atomica, apenas prossegue
buscaContradicao formula (NodeIntermediario node e d) = NodeIntermediario node (buscaContradicao formula e) (buscaContradicao formula d)

buscaContradicao formula Null = Null
buscaContradicao formula NodeFechado = NodeFechado

possuiNodeAberto :: Arvore -> Bool
possuiNodeAberto NodeFechado = False
possuiNodeAberto (NodeIntermediario node Null Null) = True
possuiNodeAberto (NodeIntermediario node Null d) = possuiNodeAberto d
possuiNodeAberto (NodeIntermediario node e Null) = possuiNodeAberto e
possuiNodeAberto (NodeIntermediario node e d) = possuiNodeAberto e || possuiNodeAberto d
possuiNodeAberto Null = False

printFormulaContraProva :: Formula -> IO()
printFormulaContraProva (Var var) = putStr ((formulaToStr (Var var)) ++ ": Verdadeiro\n")
printFormulaContraProva (Not(Var var)) = putStr ((formulaToStr (Var var)) ++ ": Falso\n")

printFormulas :: [Formula] -> IO()
printFormulas (x:xs) = do
    printFormulaContraProva x
    printFormulas xs

printFormulas [] = putStr ""

printMotivoFalsificacao :: Arvore -> IO()
printMotivoFalsificacao a = printFormulas (motivoFalsificacao a [])

motivoFalsificacao :: Arvore -> [Formula] -> [Formula]
motivoFalsificacao (NodeIntermediario (Node formula _) Null Null) formulas
    | formulaIsAtomic formula = formulas ++ [formula]
    | otherwise = formulas

motivoFalsificacao (NodeIntermediario (Node formula _) e d) formulas
    | formulaIsAtomic formula && possuiNodeAberto e = motivoFalsificacao e (formulas ++ [formula])
    | formulaIsAtomic formula && possuiNodeAberto d = motivoFalsificacao d (formulas ++ [formula])
    | possuiNodeAberto e = motivoFalsificacao e formulas
    | possuiNodeAberto d = motivoFalsificacao d formulas

printProva :: Formula -> IO()
printProva formula = do
    putStrLn "\n\n######################"
    putStrLn "\nNotação: + indica ramo à esquerda e ` indica ramo à direita\n\n"
    putStr "Árvore de prova/refutação da fórmula "
    printFormula formula
    putStr ": \n\n"
    printArvore arvoreProva
    putStr "\n\n"
    if possuiNodeAberto arvoreProva
    then do
        putStr "A fórmula NÃO é consequência lógica das hipóteses (é falsificável)\n"
        putStr "\nContra-prova: \n"
        printMotivoFalsificacao arvoreProva
    else
        putStr "A fórmula é consequência lógica das hipóteses (todo ramo fecha)"
    putStr "\n"
    where
        arvoreProva = prova formula

provaSe :: [Formula] -> Formula -> IO ()
provaSe hipoteses conclusao =
    printProva (And (juntaHipoteses hipoteses) (Not conclusao))

juntaHipoteses :: [Formula] -> Formula
juntaHipoteses [] = error "Sem hipóteses"
juntaHipoteses [f] = f
juntaHipoteses (f:fs) = And f (juntaHipoteses fs)