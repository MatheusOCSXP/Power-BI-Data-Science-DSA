# Estatistica Basica com R

# Parte 2 - Medidas de Dispersão

# Definindo a pasta de trabalho
setwd("G:/Meu Drive/Matheus-PC/Ciência de Dados/Cursos/Curso Power BI - DSA/cap12")

# Confirmando se o caminho da pasta de trabalho foi importado corretamente
getwd()

# Carregando o dataset
vendas <- read.csv("Vendas.csv")

# Resumo do dataset
View(vendas) #visualiza o dataset importado
str(vendas) #resumo das variaveis do dataset
summary(vendas) #resumo estatistico de todas as variaveis do dataset
summary(vendas$Valor) #resumo estatistico de uma unica variavel

# Variância
var(vendas$Valor)

# Desvio Padrão
sd(vendas$Valor)

