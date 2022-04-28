/* Retorne todos os registros dos funcionários com 2 filhos. */

SELECT *
FROM cap16."TB_FUNC"
WHERE numero_filhos = '2' --Aqui foi utilizado aspas simples porque o número estava como character

--Uma segunda solução para resolver esse problema seria:
SELECT *
FROM cap16."TB_FUNC"
WHERE CAST(numero_filhos AS integer) = 2

--Por fim, o correto seria corrigir a variável para número inteiro já que corrigimos os valores, removendo os "null"