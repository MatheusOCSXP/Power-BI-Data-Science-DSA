#Estatística Básica

#Parte 5  - Tabela de Frequência

#Definindo a pasta de trabalho
setwd("G:/Meu Drive/Matheus-PC/Ciência de Dados/Cursos/Curso Power BI - DSA/cap12")
getwd()

#Carregando dados
dados <- read.table("Usuarios.csv",
                    dec = ".",
                    sep = ",",
                    h = T, #indica que primeira linha contém cabeçalho
                    fileEncoding = "windows-1252"
  
)

#Visualizando e sumarizando os dados
View(dados)
names(dados) #visualiza o nome das colunas do dataset
str(dados) #resumo dos tipos de dados
summary(dados$salario)
summary(dados$grau_instrucao) #valor categórico
mean(dados$salario)
mean(dados$grau_instrucao) #erro, não é possível fazer média de valor categórico, retorna NA

#Tabela de Frequência Absoluta
freq <- table(dados$grau_instrucao)
View(freq)

#Tabela de Frequência Relativa
freq_rel <- prop.table(freq)
View(freq_rel) #valores decimais

#Transformando em porcentagem
p_freq_rel <- 100 * prop.table(freq)
View(p_freq_rel) #valores percentuais

#Adicionar uma linha de valor total
freq <- c(freq, sum(freq))
print(freq)
names(freq)[4] <- "Total"
View(freq)

#Tabela final com todos os valores
#Calculando frequência relativa e frequência proporcional
freq_rel <- c(freq_rel, sum(freq_rel))
print(freq_rel)
p_freq_rel <- c(p_freq_rel, sum(p_freq_rel))
print(p_freq_rel)

#Tabela final com todos os vetores
?cbind #usado para concatenar colunas
tabela_final <- cbind(freq,
                      freq_rel = round(freq_rel, digits = 2),
                      p_freq_rel = round(p_freq_rel, digits = 2))

View(tabela_final)
