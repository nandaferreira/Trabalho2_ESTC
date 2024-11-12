#---------Exercício 1---------
library(ggplot2) #para plotar gráfico
library(rpart) #para fazer o modelo da árvore de decisão
library(rpart.plot) #plotar a árvore
library(randomForest)#fazer a floresta aleatória
library(dplyr)
library(class)

diabetes <- read.table(file = "diabetes.txt", header = TRUE, sep = ";") #ler o conjunto
#Como criar o conjunto de teste e treinamento?

diabetes
str(diabetes)
n <- round(0.8*nrow(diabetes)) 
indices_treino <- sample(1:nrow(diabetes), size = n, replace = FALSE)
diabetes <- diabetes[,-1] #Tirei uma coluna porque a primeira era o ID e ela não me retorna nenhuma informação relevante, é só pra identificar o paciente mesmo

#Treinamento: analiso o conjunto todo 
#Teste: 20% restante

treinamento <- diabetes[1:n,]
teste <- diabetes[-(1:n),]

#----(a)---------

summary(diabetes)
str(diabetes)
str(treinamento)

diabetes$Diabetic <- as.factor(diabetes$Diabetic) #agora aparece que diabetic tem 2 levels, foi só pra refatorar
str(diabetes)
diabetes$Diabetic #vetor lógico (0 = não tem, 1 tem a doença)

#A análise individual das variáveis categóricas não parece ser muito explicativa, então será para uma análisa mais superficial por hora

ggplot(data = treinamento, mapping = aes(x = Pregnancies, fill = factor(Diabetic))) +
  geom_histogram(color = "black") +
  theme_minimal() +
  labs(fill = "Diabete")

#A contagem de gravidezes parece ser um pouco maior em pacientes diabéticos.

ggplot(data = treinamento, aes(x = PlasmaGlucose, fill = factor(Diabetic)))+
  geom_histogram(color= "black")+
  theme_minimal()+
  labs(fill = "Diabete")
#Esse gráfico, como o esperado, indica que os números altos para diabetes estão diretamente relacionados com os níveis altos de glicose no sangue do paciente. 

ggplot(data = treinamento, aes(x = TricepsThickness, fill = factor(Diabetic)))+
  geom_histogram(color = "black")+
  theme_minimal()+
  labs(fill = "Diabete")

ggplot(data = treinamento, aes(x = SerumInsulin, fill = factor(Diabetic)))+
  geom_histogram(color = "black")+
  theme_minimal()+
  labs(fill = "Diabete")

ggplot(data = treinamento, aes(x = BMI, fill = factor(Diabetic)))+
  geom_histogram(color = "black")+
  theme_minimal()+
  labs(fill = "Diabete")

ggplot(data = treinamento, aes(x = DiabetesPedigree, fill = factor(Diabetic)))+
  geom_histogram(color = "black")+
  theme_minimal()+
  labs(fill = "Diabete")

ggplot(data = treinamento, aes(x = Age, fill = factor(Diabetic)))+
  geom_histogram(color = "black")+
  theme_minimal()+
  labs(fill = "Diabete")

#----(b)---------
#Usar só o conjunto treinamento !!

#Modelo de árvore de decisão
modelo.arvore <- rpart(formula = Diabetic~ ., data = treinamento, method = "class") #a formula é o diagnostico em função de tudo (Diabetic ~.,)

rpart.plot(modelo.arvore, extra = 101) #extra = 101 exibe a classe predita em cada nó da árvore, juntamente com o número de observações de treinamento que caem em cada nó.
previsao <- predict(modelo.arvore, newdata = treinamento, type = "class")

#Função diagnóstico
diagnostico_paciente <- function(Pregnancies, PlasmaGlucose, DiastolicBloodPressure, TricepsThickness, SerumInsulin, BMI, DiabetesPedigree, Age){
    
  if (Pregnancies < 2) {
    return("Negativo")
  } else {
    if (BMI < 22) {
      return("Negativo")
    } else {
      if (SerumInsulin < 52) {
        if (Age < 36) {
          return("Negativo")
        } else {
          return("Positivo")
        }
      } else {
        if (Age < 36) {
          return("Positivo")
        } else {
          if (PlasmaGlucose < 96) {
            if (BMI >= 33) {
              return("Positivo")
            } else {
              return("Negativo")
            }
          } else {
            if (Age >= 24) {
              return("Positivo")
            } else {
              if (Age < 27) {
                return("Negativo")
              } else {
                return("Positivo")
              }
            }
          }
        }
      }
    }
  }
}



teste$Diagnostico_Previsto <- mapply(diagnostico_paciente,
                                     teste$Pregnancies,
                                     teste$PlasmaGlucose,
                                     teste$DiastolicBloodPressure,
                                     teste$TricepsThickness,
                                     teste$SerumInsulin,
                                     teste$BMI,
                                     teste$DiabetesPedigree,
                                     teste$Age)

mostrar_diagnostico <-(teste[, c("PlasmaGlucose", "BMI", "DiastolicBloodPressure", "Age", "Diagnostico_Previsto")])

#Acurácia do modelo

teste$Diabetic <- factor(teste$Diabetic, levels = c(0, 1), labels = c("Negativo", "Positivo"))
acuracia <- mean(teste$Diagnostico_Previsto == teste$Diabetic)
round(acuracia*100,2)


#----(c)-----
indices_treino <- sample(1:nrow(diabetes), size = n, replace = FALSE)
treino <- diabetes[indices_treino,]
teste <- diabetes[-indices_treino,]

floresta <- randomForest(formula = Diabetic ~ ., data = treino, ntree = 200)
floresta
previsao.arvore <- predict(arvore, newdata = teste, type = "class")
mean(previsao.arvore == teste$diagnosis) #Acurárica do modelo tá dando NaN


