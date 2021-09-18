import Data.Set (Set)
import Data.Typeable

p :: Formula
p = Var $ Nome "p"

q :: Formula
q = Var $ Nome "q"

form :: Formula
form = p

-- main = prova(form)([p])
main = printFormula(Implicacao p q)

data Variavel = Nome String

-- Estrutura de dados para uma formula
data Formula = 
    Var Variavel |
    Not Formula |
    And Formula Formula |
    Or Formula Formula |
    Implicacao Formula Formula

implicacao :: Formula -> Formula -> Formula
implicacao (formula1)(formula2) = Or (Not formula1) (formula2)


demorgan :: Formula -> Formula
demorgan (And formula1 formula2) = Or (Not formula1) (Not formula2)

demorgan (Or formula1 formula2) = And (Not formula1) (Not formula2)

simplifica :: Formula -> Formula
simplifica (Not (Not formula)) = formula
simplifica (Not (And formula1 formula2)) = demorgan(And formula1 formula2)
simplifica (Not (Or formula1 formula2)) = demorgan(Or formula1 formula2)
simplifica (Implicacao formula1 formula2) = implicacao(formula1)(formula2)
simplifica (formula) = formula  -- se cheguei aqui, já tá simplificado

temAbsurdo :: [Formula] -> Variavel -> Bool
temAbsurdo ([Var variavel])(Nome nome) = False
-- temAbsurdo ([Not formula])(Nome nome) = not temAbsurdo([formula])(nome)
temAbsurdo ([_])(Nome nome) = False


printVariavel :: Variavel -> IO()
printVariavel (Nome nome) = putStr(nome)

printFormula :: Formula -> IO()
printFormula (Var variavel) = do printVariavel(variavel)

printFormula (Not formula) = do
    putStr("~")
    putStr("(")
    printFormula(formula)
    putStr(")")

printFormula (And formula1 formula2) = do
    putStr("(")
    printFormula(formula1)
    putStr(" & ")
    printFormula(formula2)
    putStr(")")

printFormula (Or formula1 formula2) = do
    putStr("(")
    printFormula(formula1)
    putStr(" | ")
    printFormula(formula2)
    putStr(")")

printFormula (Implicacao formula1 formula2) = do
    putStr("(")
    printFormula(formula1)
    putStr(" -> ")
    printFormula(formula2)
    putStr(")")

prova :: Formula -> [Formula] -> IO()
prova (Var variavel) ([]) = do
    printVariavel(variavel)

prova (Var variavel) ([a]) = do
    printVariavel(variavel)
    if temAbsurdo([a])(variavel)
    then
        putStrLn("Fecha ramo")
    else
        putStrLn("Segue")
