# Estatistica Basica com R

# Parte 3 - Medidas de Posição Relativa

# Definindo a pasta de trabalho
setwd("G:/Meu Drive/Matheus-PC/Ciência de Dados/Cursos/Curso Power BI - DSA/cap12")

# Confirmando se o caminho da pasta de trabalho foi importado corretamente
getwd()

# Carregando o dataset
vendas <- read.csv("Vendas.csv")

# Resumo do dataset
head(vendas) #primeiras linhas do dataset
tail(vendas) #últimas linhas do dataset
View(vendas)

# Medidas de Tendência Central
summary(vendas$Valor)
summary(vendas[c('Valor', 'Custo')]) #trazer 2 variáveis juntas

# Explorando Variáveis Numéricas
mean(vendas$Valor)
median(vendas$Valor)
quantile(vendas$Valor) #1/2/3/4 quartis
quantile(vendas$Valor, probs = c(0.01, 0.99)) #percentis específicos
quantile(vendas$Valor, seq(from = 0, to = 1, by = 0.20)) #percentis de 0 a 10, com intervalos de 20%
IQR(vendas$Valor) #Intervalo interquartil Q3 e Q1
range(vendas$Valor) #intervalo entre o valor mínimo e máximo
diff(range(vendas$Valor)) #diferença entre o valor mínimo e máximo
