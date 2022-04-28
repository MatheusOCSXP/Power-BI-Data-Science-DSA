# Estatistica Basica com R

# Parte 1 - Medidas de Posicao

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

# Média
?mean #função utilizada para receber a documentação
mean(vendas$Valor)

# Média Ponderada
?weighted.mean
weighted.mean(vendas$Valor, w = vendas$Custo)

# Mediana
?median
median(vendas$Valor)

# Moda
# Criando uma função
moda <- function(v) {
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

# Obtendo a moda
moda(vendas$Valor)

# Criando gráfico de Média de Valor por Estado com ggplot2
install.packages("ggplot2")
library(ggplot2)

# Criando o gráfico
ggplot(vendas) + 
  stat_summary(aes(x = Estado,
                   y = Valor),
               fun = mean,
               geom = "bar",
               fill = "lightgreen",
               col = "grey50") +
  labs(title = "Média de Valor por Estado")
