# Tableaux

Programa para verificar se uma fórmula na lógica proposicional clássica é tautologia ou é falsificável

## Como usar

### Makefile

* `make compile` compila example.hs e gera executável ./example

* ` make run` compila e executa example

### Example

```hs
import Tableaux

-- Variáveis definidas anteriormente por motivo de legibilidade

a :: Formula
a = Var $ Name "a"

b :: Formula
b = Var $ Name "b"

-- printProva :: Formula -> IO()
-- Uso: printProva(And (Var $ Name "a") (Var $ Name "b")) exibe a árvore de prova/refutação de "a & b"


-- Uso de fórmulas: 
-- `Var $ Name "a"` representa variável com nome "a"
-- Not formula representa a negação da variável formula de tipo Formula
-- And formula1 formula2 representa (formula1 & formula2)
-- Or formula1 formula2 representa (formula1 | formula2)
-- Implication formula1 formula2 representa (formula1 -> formula2)

main = do
    printProva (a)                  -- Formula: a
    printProva (Not a)              -- Formula: ~a
    printProva (And a b)            -- Formula: a & b
    printProva (Or a b)             -- Formula: a || b
    printProva (Implication a b)    -- Formula: a -> b

    printProva (Implication a (And a b))    -- Formula: a -> (a & b)
    printProva (And a (Not a))              -- Formula: a & ~a
```

