import Tableaux

-- Variáveis definidas anteriormente por motivo de legibilidade

a = Var $ Name "A"
b = Var $ Name "B"
c = Var $ Name "C"
d = Var $ Name "D"
e = Var $ Name "E"

-- Uso de fórmulas: 
-- `Var $ Name "a"` representa variável com nome "a"
-- Not formula representa a negação da variável formula de tipo Formula
-- And formula1 formula2 representa (formula1 & formula2)
-- Or formula1 formula2 representa (formula1 | formula2)
-- Implication formula1 formula2 representa (formula1 -> formula2)

main = do
    let hipoteses = [Implication (Not d) (Or (Not a) (Not c)), Implication (Or b e) a]
    let conclusao = Implication (Or a b) (Implication c d)
    provaSe hipoteses conclusao