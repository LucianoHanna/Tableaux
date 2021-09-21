import Tableaux

-- Variáveis definidas anteriormente por motivo de legibilidade

a :: Formula
a = Var $ Name "a"

b :: Formula
b = Var $ Name "b"

p :: Formula
p = Var $ Name "p"

q :: Formula
q = Var $ Name "q"

r :: Formula
r = Var $ Name "r"

-- printProva :: Formula -> IO()
-- Uso: printProva(And (Var $ Name "a") (Var $ Name "b")) exibe a árvore de prova/refutação de "a & b"


-- Uso de fórmulas: 
-- `Var $ Name "a"` representa variável com nome "a"
-- Not formula representa a negação da variável formula de tipo Formula
-- And formula1 formula2 representa (formula1 & formula2)
-- Or formula1 formula2 representa (formula1 | formula2)
-- Implication formula1 formula2 representa (formula1 -> formula2)

main = do
    printProva (Implication (Or (p) (And q r)) (And (Or p q) (Or p r)))
    printProva (Implication a (Implication a (Implication b a)))
    printProva (Implication b (And a (Or b a)))
