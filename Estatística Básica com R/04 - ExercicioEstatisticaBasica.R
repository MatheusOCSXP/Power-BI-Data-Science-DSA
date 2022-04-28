# Em anexo você encontra um dataset contendo notas de diversos alunos em duas turmas
#diferentes. Usando a LinguagemR, resolva os itens abaixo:
# Exercício 1: Apresente um resumo de tipos de dados e estatísticasdo dataset.
# Exercício 2: Qual a média de cada turma?
# Exercício 3: Qual turma apresentou maior variabilidade de notas? Justifique sua resposta.
# Exercício 4: Calcule o coeficiente de variação das 2 turmas.
# Exercício 5: Qual nota apareceu mais vezes em cada turma?
#=================================================================================#

# Definindo a pasta de trabalho
setwd("G:/Meu Drive/Matheus-PC/Ciência de Dados/Cursos/Curso Power BI - DSA/cap12")

Notas <- read.csv("Notas.csv")

# Resposta 1
view(Notas)
str(Notas)
summary(Notas)

# Resposta 2
MediaTurmaA <- mean(Notas$TurmaA)
print(MediaTurmaA)

MediaTurmaB <- mean(Notas$TurmaB)
print(MediaTurmaB)

# Resposta 3
VarTurmaA <- var(Notas$TurmaA)
print(VarTurmaA)
sd(Notas$TurmaA)
#Var = 209.75

VarTurmaB <- var(Notas$TurmaB)
print(VarTurmaB)
sd(Notas$TurmaB)
#Var = 38.25

#A turma que apresenta a maior variabilidade das notas é a turma A
#O seu coeficiente de variação foi superior ao da turma B

# Resposta 4

CV_TurmaA <- (sd(Notas$TurmaA) / MediaTurmaA) * 100
print(CV_TurmaA)
#CV = 21.90

CV_TurmaB <- (sd(Notas$TurmaB) / MediaTurmaB) * 100
print(CV_TurmaB)
#CV = 10.03

# Resposta 5

# Criando a função para moda
moda <- function(v) {
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

ModaTurmaA <- moda(Notas$TurmaA)
print(ModaTurmaA)
#Moda: 82

ModaTurmaB <- moda(Notas$TurmaB)
print(ModaTurmaB)
#Moda: 60