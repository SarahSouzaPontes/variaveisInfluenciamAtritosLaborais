# Investigando variaveis que influenciam em atrito no local de trabalho.

getwd()

# Imports
library(caret)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)
library(caTools)
library(corrplot)
library(rpart)
library(rpart.plot)

# Carregando o dataset e transformando em uma dataframe
dados_rh <- fread('dados/dataset.csv')
#23058 linhas e 30 colunas
dim(dados_rh)
#Visualizando o banco
View(dados_rh)
#Tipos de variáveis
str(dados_rh)
#Resumo estatístico
summary(dados_rh)

##### Limpeza e Transformação ##### 

# Transformando variáveis categóricas para o tipo fator (qualitativa)
View(dados_rh)
dados_rh$Attrition                <- as.factor(dados_rh$Attrition)
dados_rh$BusinessTravel           <- as.factor(dados_rh$BusinessTravel)
dados_rh$Department               <- as.factor(dados_rh$Department)
dados_rh$Education                <- as.factor(dados_rh$Education)
dados_rh$EducationField           <- as.factor(dados_rh$EducationField)
dados_rh$'Employee Source'        <- as.factor(dados_rh$'Employee Source')
dados_rh$EnvironmentSatisfaction  <- as.factor(dados_rh$EnvironmentSatisfaction)
dados_rh$Gender                   <- as.factor(dados_rh$Gender)
dados_rh$JobInvolvement           <- as.factor(dados_rh$JobInvolvement)
dados_rh$JobLevel                 <- as.factor(dados_rh$JobLevel)
dados_rh$JobRole                  <- as.factor(dados_rh$JobRole)
dados_rh$JobSatisfaction          <- as.factor(dados_rh$JobSatisfaction)
dados_rh$MaritalStatus            <- as.factor(dados_rh$MaritalStatus)
dados_rh$OverTime                 <- as.factor(dados_rh$OverTime)
dados_rh$PerformanceRating        <- as.factor(dados_rh$PerformanceRating)
dados_rh$RelationshipSatisfaction <- as.factor(dados_rh$RelationshipSatisfaction)
dados_rh$StockOptionLevel         <- as.factor(dados_rh$StockOptionLevel)
dados_rh$WorkLifeBalance          <- as.factor(dados_rh$WorkLifeBalance)
str(dados_rh)

# Transformando variáveis numéricas para o tipo inteiro
View(dados_rh)
dados_rh$DistanceFromHome  <- as.integer(dados_rh$DistanceFromHome)
dados_rh$MonthlyIncome     <- as.integer(dados_rh$MonthlyIncome)
dados_rh$PercentSalaryHike <- as.integer(dados_rh$PercentSalaryHike)

# Drop dos níveis de fatores com 0 count
dados <- droplevels(dados_rh)
#Visualizando após transformacao
str(dados_rh)
#Resumo do dataset contagem em categorica e resumo esttaistico em quantitativa
summary(dados_rh)
#Visualizando dados
View(dados_rh)

##### Engenharia de Atributos ##### 

# Variável criada tempo de experiencia total menos experiencia na empresa:
dados_rh$PriorYearsOfExperience <- dados_rh$TotalWorkingYears - dados_rh$YearsAtCompany
#Visualizando banco de dados
View(dados_rh)

#Permanência/ duração em empresas
dados_rh$AverageTenure <- dados_rh$PriorYearsOfExperience / dados_rh$NumCompaniesWorked

#Infinito, pois divisão por 0
View(dados_rh)

# A estabilidade média produz valores como Inf devido à natureza de sua derivação
# Substituímos para zero.
summary(dados_rh$AverageTenure)
#Tudo diferente de infinito
dados_rh$AverageTenure[!is.finite(dados_rh$AverageTenure)] <- 0
summary(dados_rh$AverageTenure)
View(dados_rh)

# Retorna os funcionarios que nao foram demitidos, dividindo dataset:
dados_rh_1 <- dados_rh[dados_rh$Attrition != 'Termination']
dados_rh_1 <- droplevels(dados_rh_1)
dim(dados_rh_1)
summary(dados_rh_1)

# Memos filtro para demissao voluntaria:
dados_rh_2 <- dados_rh[dados_rh$Attrition != 'Voluntary Resignation']
dados_rh_2 <-droplevels(dados_rh_2)
dim(dados_rh_2)  
summary(dados_rh_2)

##### Análise Exploratória ##### 

# Plots de análise univariada
#Gráfico de Barra --> Genero (qualitativa)
ggplot(dados_rh) + geom_bar(aes(x = Gender))
#Gráfico de Dispersão --> Idade (quantitativa)
ggplot(dados_rh) + geom_density(aes(x = Age))
#Gráfico de Barra --> Atritos (qualitativa)
ggplot(dados_rh) + geom_bar(aes(x = Attrition))
#Gráfico de Barra --> Departamento 
ggplot(dados_rh) + geom_bar(aes(x = Department))
#Gráfico de Barra --> Funcao
ggplot(dados_rh) + geom_bar(aes(x = JobRole))
#Gráfico de Barra/ Face-grid --> Grau de Educação
ggplot(dados_rh) + geom_bar(aes(x = Education)) + facet_grid(~EducationField)

# Multiplot Grid - comparar variáveis em mais de um critério especifico (densidade = eixo y e eixo-x- anos na cia, anos na promocao, anos na funcao atual, gestor atual e a funcao)
p.TotalWorkingYears       <- ggplot(dados_rh) + geom_density(aes(TotalWorkingYears))
p.YearsAtCompany          <- ggplot(dados_rh) + geom_density(aes(YearsAtCompany))
p.YearsSinceLastPromotion <- ggplot(dados_rh) + geom_density(aes(YearsSinceLastPromotion))
p.YearsWithCurrManager    <- ggplot(dados_rh) + geom_density(aes(YearsWithCurrManager))
p.YearsInCurrentRole      <- ggplot(dados_rh) + geom_density(aes(YearsInCurrentRole))
p.PriorYearsOfExperience  <- ggplot(dados_rh) + geom_density(aes(PriorYearsOfExperience))

# Organiza no grid
grid.arrange(p.TotalWorkingYears, 
             p.YearsAtCompany, 
             p.YearsSinceLastPromotion, 
             p.YearsWithCurrManager, 
             p.YearsInCurrentRole, 
             p.PriorYearsOfExperience, 
             nrow = 2, 
             ncol = 3)

# Tempo de experiência anterior
# Categorizando faixa de tempo na empresa 1, 3, 5, 7, 10 anos:
#Proporcao
length(which(dados_rh$PriorYearsOfExperience < 1)) / length(dados_rh$PriorYearsOfExperience)  
length(which(dados_rh$PriorYearsOfExperience < 3)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 5)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 7)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 10)) / length(dados_rh$PriorYearsOfExperience)  


# 58% dos funcionários têm menos de 3 anos de experiência;
# Idade
length(which(dados_rh$Age < 30)) / length(dados_rh$Age)

# Apenas 22% dos funcionários têm menos de 30 anos:
#Educação
summary(dados_rh$Education)
#Graduaçao
length(which(dados_rh$Education == 3)) / length(dados_rh$Education)
#Mestrado
length(which(dados_rh$Education == 4)) / length(dados_rh$Education)

# Exemplo de insight:
# Cerca de 39% dos funcionários são graduados e 27% realizaram o mestrado.
# A busca pelo ensino superior pode ter levado a uma diminuição da experiência de trabalho.

# Boxplot mostrando a distribuição do salário mensal/satisfação do trabalho
#Removendo missing
ggplot(data = subset(dados_rh, !is.na(JobSatisfaction)), aes(JobSatisfaction, MonthlyIncome)) + 
  geom_boxplot()

# Possivel que salario mais alto nao leva a satisfacaono trabalho.

# Correlação/ apenas dados completos sem missing:
#anos de trab e n de anos na cia
cor(dados_rh$TotalWorkingYears, dados_rh$YearsAtCompany,          use = "complete.obs")
#correlacao positiva

#anos na empresa e total de anos na funcao atual
cor(dados_rh$YearsAtCompany,    dados_rh$YearsInCurrentRole,      use = "complete.obs")
#correlacao positiva

#promocao--> correlacao positiva
cor(dados_rh$YearsAtCompany,    dados_rh$YearsSinceLastPromotion, use = "complete.obs")

#tempo com o gerente atual--> correlacao positiva
cor(dados_rh$YearsAtCompany,    dados_rh$YearsWithCurrManager,    use = "complete.obs")

#tempo de exp/ salario --> correlacao positiva****
cor(dados_rh$TotalWorkingYears, dados_rh$MonthlyIncome,           use = "complete.obs")

#Temp de empresa -->correlacao******
cor(dados_rh$YearsAtCompany,    dados_rh$MonthlyIncome,           use = "complete.obs")  

# Scatterplots ---> das 2 ultimas variaveis
ggplot(dados_rh) + geom_point(aes(TotalWorkingYears, MonthlyIncome))
ggplot(dados_rh) + geom_point(aes(YearsAtCompany, MonthlyIncome))

# Relação entre vida pessoal e profissional e renda mensal
ggplot(data = subset(dados_rh, !is.na(WorkLifeBalance)), aes(WorkLifeBalance, MonthlyIncome)) + 
  geom_boxplot()


# Os funcionários que avaliaram o equilíbrio entre vida profissional e pessoal igual a 1 também têm renda média mensal.

# Salário x Gênero
ggplot(data = subset(dados_rh, !is.na(Gender)), aes(Gender, MonthlyIncome, fill = Gender)) +
  geom_boxplot() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)) +
  labs(x = "Gênero", y = "Salário Mensal", title = "Salário Mensal Entre Gêneros") +
  coord_flip()


# Função
ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, MonthlyIncome)) +
  ggtitle("Salário Mensal Por Função")

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, AgeStartedWorking)) +
  ggtitle("Idade Que Iniciou na Função")

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, Age)) +
  ggtitle("Idade Por Função")

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, YearsAtCompany)) +
  ggtitle("Tempo de Empresa (em anos)")

ggplot(data = na.omit(dados_rh)) + geom_bar(aes(JobRole, fill = Education), position = "fill") +
  ggtitle("Nível de Educação Por Função") + 
  ylab("Proporção")

# Plots de análise multivariada para variáveis normalmente usadas durante o processo de contratação
#DEPARTAMENTO X ROTATIVIDADE
ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = Education , fill = Attrition), position = 'fill') + 
  facet_grid(.~Department)

#FUNCAO
ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = Education , fill = Attrition), position = 'fill') + 
  facet_grid(.~JobRole)

#EDUCACAO
ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = EducationField , fill = Attrition), position = 'fill') + 
  facet_grid(.~JobRole) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

# Plots de análise multivariada para variáveis normalmente usadas após o processo de contratação
ggplot(dados_rh_1) + geom_bar(aes(x = DistanceFromHome, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = `Employee Source`, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = JobRole, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = MaritalStatus, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = AverageTenure, fill = Attrition), position = 'fill')

# Plots de análise multivariada entre algumas variáveis e o status do funcionário
ggplot(dados_rh_1) + geom_bar(aes(StockOptionLevel, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(EnvironmentSatisfaction, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(JobSatisfaction, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(JobInvolvement, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(RelationshipSatisfaction, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(WorkLifeBalance, fill = Attrition), position = 'fill')

##### __________________Modelagem Preditiva_________________ ##### 
#buscar relacionamento entre variaveis

# Modelo 
?glm
library(glm.deploy)
library(glm.predict)
library(glm2)



#__________________________________________________


modelo_v1 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender + 
                   Education + EducationField, 
                 family = binomial, 
                 data = dados_rh)
#______________________________________________________

summary(modelo_v1)
?vif
vif(modelo_v1)
#________________________________________________________________
# Vamos dividir os dados em treino e teste. Vamos trabalhar com os dados sem registros de demitidos.
set.seed(2004)
#____----> 30/70% Teste e Treino
index_treino <- sample.split(Y = dados_rh_1$Attrition, SplitRatio = 0.7)
#_________Index Treino T & F;
dados_rh_1_treino <- subset(dados_rh_1, train = T)
dados_rh_1_teste <- subset(dados_rh_1, train = F)
#_____________________sem as pessoas demitidas_______________#dados_rh_1

# Segunda versão do modelo com dados de treino
modelo_v2 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender + 
                   Education + EducationField, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v2)
vif(modelo_v2)
#____simplificando o modelo retirando variáveis que não há relevancia.


# Previsões
threshold <- 0.5
previsoes_v2 <- predict(modelo_v2, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v2 <- ifelse(previsoes_v2 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v2)

#REMOVER EDUCAÇAO --> DEPARTAMENTO PASSOU A TER MAIS INFLUENCIA
# Terceira versão do modelo com dados de treino e sem variáveis de educação
modelo_v3 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v3)
vif(modelo_v3)

#AVALIANDO RESULTADO
# Previsões
threshold <- 0.5
previsoes_v3 <- predict(modelo_v3, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v3 <- ifelse(previsoes_v3 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v3)
#MODELO ERRANDO MAIS, PORÉM MAIS SIMPLES.

#REMOVENDO MAIS UMA VARIÁVEL --> GENERO, SEM RELEVANCIA PARA O MODELO.
# Quarta versão do modelo com dados de treino e sem variáveis de educação e genero
modelo_v4 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v4)
vif(modelo_v4)
#DEIXANDO MODELO MAIS SIMPLES SEM PERDER O PODER PREDITIVO.

# Previsões
threshold <- 0.5
previsoes_v4 <- predict(modelo_v4, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v4 <- ifelse(previsoes_v4 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v4)



# Quinta versão do modelo com dados de treino e sem variáveis de educação, genero e outro algoritmo
?rpart
#Recursive Partitioning and Regression Trees
modelo_v5 <- rpart(Attrition ~ Age + Department + DistanceFromHome + JobRole + MaritalStatus + 
                     AverageTenure + PriorYearsOfExperience, 
                   method = "class", 
                   control = rpart.control(minsplit = 500, cp = 0),
                   data = dados_rh_1_treino)

summary(modelo_v5)
rpart.plot(modelo_v5)


# Conclusões: Idade é igual a maior de 34anos;

#I >= 34 a grande chance de continuar (62% probabilidade)
#I < 22 anos chance de pedir demissão;
#Função: Saúde e Cientista de Pesquisa - 1% de continuar e 2% pedir demissão;
#Não é maior que 22a -->Solteiro-->24% continuar na empresa.
#Estabilidade no cargo. 
#Anos de experiência anterior não entrou no modelo de arvore de decisão.
#Vrificar acurácia, métricas do modelo e avaliar a versão final.














