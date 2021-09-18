import Data.Typeable

p :: Formula
p = Var $ Nome "p"

q :: Formula
q = Var $ Nome "q"

form :: Formula
form = p

arvore :: Arvore
arvore = NodeIntermediario (Node p False False) Null Null
arvore1 = insereNode1 arvore (Node q False False)
arvore2 = insereNode2 arvore1 (Node q False False) (Node p False False)
arvore3 = insereNode1 arvore2 (Node q False False)
-- main = prova(form)([p])
-- main = printFormula(Implicacao p q)
main = printArvore arvore3

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
    | otherwise = arvoreStrPossuiNoNotNull xs (not(isParentese x ))

arvoreStrPossuiNoNotNull [] haveNonParentese = haveNonParentese


isParentese :: Char -> Bool
isParentese '(' = True
isParentese ')' = True
isParentese x = False

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
arvoreNivelToStrAux (NodeIntermediario node _ _) = "("++nodeToStr node++")"

arvoreNivelToStrAux NodeFechado = "(X)"

arvoreNivelToStrAux Null = "()"

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

-- prova :: Formula -> [Formula] -> IO()
-- prova (Var variavel) ([]) = do
--     printVariavel(variavel)

-- prova (Var variavel) ([a]) = do
--     printVariavel(variavel)
--     if temAbsurdo([a])(variavel)
--     then
--         putStrLn("Fecha ramo")
--     else
--         putStrLn("Segue")
