/* Retorne a média de salário hora por estado */

SELECT ROUND(AVG(salario_hora)), cap16."TB_ENDERECO".estado
FROM cap16."TB_FUNC"
JOIN cap16."TB_ENDERECO" ON cap16."TB_FUNC".id = cap16."TB_ENDERECO".id_func
GROUP BY cap16."TB_ENDERECO".estado

--Outra forma de resolver ("JOIN" sem usar JOIN)
SELECT ROUND(AVG(TBF.salario_hora)), TBE.estado
FROM cap16."TB_FUNC" TBF, cap16."TB_ENDERECO" TBE
WHERE TBF.id = TBE.id_func
GROUP BY TBE.estado