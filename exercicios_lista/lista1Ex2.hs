--O erro da funcao, é que nao teve a condicao de parada, quando fat 0 = 1
--com esse problema, ocorrerá um looping infinito, indo para fatoriais negativos
--funcao corrigida:

fat::Int->Int
fat 0 = 1
fat x = x * fat(x-1)

