#Nome: Anna Karolyna Pereira Santos         Matrícula: 12221BCC046
#Nome: Fernanda Ferreira de Melo            Matrícula: 12211BCC043

#--------------- Exercício 1 ---------------
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
diabetes <- diabetes[,-1] #Tirei uma coluna porque a primeira era o ID e ela não me retorna nenhuma informação relevante, é só pra identificar o paciente mesmo
diabetes$Diabetic <- as.factor(diabetes$Diabetic) #agora aparece que diabetic tem 2 levels, foi só pra refatorar
n <- round(0.8*nrow(diabetes)) 
set.seed(246)
indices_treino <- sample(1:nrow(diabetes), size = n, replace = FALSE)

#Treinamento: analiso o conjunto todo 
#Teste: 20% restante

treinamento <- diabetes[indices_treino,]
teste <- diabetes[-indices_treino,]

#----(a)---------
summary(diabetes)
str(diabetes)
str(treinamento)
str(diabetes)
diabetes$Diabetic #vetor lógico (0 = não tem, 1 tem a doença)

ggplot(data = treinamento, mapping = aes(x = Pregnancies, fill = factor(Diabetic))) +
  geom_histogram(color = "black", bins = 30) +
  theme_minimal() +
  labs(x = "gravidezes", y = "contagem",fill = "diabetes")

#a contagem de gravidezes parece ser um pouco maior em pacientes diabéticos, o que pode ser significar uma correlação entre a quantidade de gravidezes e o risco de desenvolver diabetes, provavelmente porque
#durante a gestação o funcionamento metabólico da gestante é diferente.

ggplot(data = treinamento, aes(x = PlasmaGlucose, fill = factor(Diabetic))) +
  geom_histogram(color = "black", bins = 30) +
  theme_minimal() +
  labs(x = "glicose no sangue", y = "contagem", fill = "diabetes")
#esse gráfico, como o esperado, indica que os números altos para diabetes estão diretamente relacionados com os níveis altos de açúcar no sangue do paciente. 

ggplot(data = treinamento, aes(x = TricepsThickness, fill = factor(Diabetic)))+
  geom_histogram(color = "black", bins = 30)+
  theme_minimal()+
  labs(x = "espessura do tríceps", y = "contagem", fill = "diabetes")
# essa análise pode indicar que a espessura da pele do tríceps, que é também um indicativo de gordura corporal, 
#pode não ser um fator diretamente associado à presença da doença nesse banco de dados.

ggplot(data = treinamento, aes(x = SerumInsulin, fill = factor(Diabetic)))+
  geom_histogram(color = "black", bins = 30)+
  theme_minimal()+
  labs(x = "concentração de insulina", y = "contagem", fill = "diabetes")

#o plot da insulina mostra que tem uma variação significativa nos dois grupos, mas pode ser que níveis mais altos 
#de insulina estejam mais relacionados com resistência à insulina, que é uma condição comumente associada aos diabéticos de tipo 2

ggplot(data = treinamento, aes(x = BMI, fill = factor(Diabetic)))+
  geom_histogram(color = "black", bins = 30)+
  theme_minimal()+
  labs(x = "índice de massa corporal", y = "contagem", fill = "diabetes")

#aqui faz sentido o maior número de casos ser em pessoas com um IMC mais elevado pois ele é um fator de risco conhecido para 
#diabetes, por causa do excesso de peso que pode afetar a sensibilidade à insulina.


ggplot(data = treinamento, aes(x = DiabetesPedigree, fill = factor(Diabetic)))+
  geom_histogram(color = "black", bins = 30)+
  theme_minimal()+
  labs(x = "tendência ao desenvolvimento de diabetes", y = "contagem", fill = "diabetes")
#esse gráfico indica que pacientes diabéticos tem um nível mais aparente de históricos familiares que apresentam casos da doença.

ggplot(data = treinamento, aes(x = Age, fill = factor(Diabetic)))+
  geom_histogram(color = "black", bins = 30)+
  theme_minimal()+
  labs(x = "idade", y = "contagem", fill = "Diabetes")
#já que  a capacidade de resposta à insulina pode diminuir ao longo dos anos, a idade mostra que pacientes diabéticos
#tendem a ser mais velhos.

#Concluindo: os gráficos acima sugerem que fatores como níveis elevados de glicose no sangue, IMC mais alto, idade avançada 
#e histórico familiar de diabetes têm uma correlação mais clara com a presença de diabetes. Porém, variáveis 
#como espessura do tríceps parecem ter uma correlação menos evidente.

#----(b)---------
#Usar só o conjunto treinamento !!

#Modelo de árvore de decisão

modelo.arvore <- rpart(formula = Diabetic~ ., data = treinamento, method = "class") #a formula é o diagnostico em função de tudo (Diabetic ~.,)
print(modelo.arvore)
rpart.plot(modelo.arvore, extra = 101) #extra = 101 exibe a classe predita em cada nó da árvore, juntamente com o número de observações de treinamento que caem em cada nó.
previsao <- predict(modelo.arvore, newdata = teste, type = "class")
print(previsao)
#Função diagnóstico

diagnostico <- function(Pregnancies, PlasmaGlucose, DiastolicBloodPressure, TricepsThickness, SerumInsulin, BMI, DiabetesPedigree, Age){
  if(Pregnancies < 2){
    return("Negativo")
  }else{
    if(BMI < 22){
      return("Negativo")
    } else{
      if(SerumInsulin < 52){
        if(Age < 36){
          return("Negativo")
        }else{
          return("Positivo")
        }
      }else{
        if(Age <36){
          if(BMI >= 36 && PlasmaGlucose < 96){
            return("Negativo")
          } else if(BMI >= 36 && PlasmaGlucose > 96){
            return("Positivo")
          }
        }else{
          return("Negativo")
        }
      }
    }
  }
}


teste$Diagnostico_Previsto <- mapply(diagnostico,
                                     teste$Pregnancies,
                                     teste$PlasmaGlucose,
                                     teste$DiastolicBloodPressure,
                                     teste$TricepsThickness,
                                     teste$SerumInsulin,
                                     teste$BMI,
                                     teste$DiabetesPedigree,
                                     teste$Age)

mostrar_diagnostico <-(teste[, c("PlasmaGlucose", "BMI", "DiastolicBloodPressure", "Age", "Diagnostico_Previsto")])
print(mostrar_diagnostico)

#Acurácia do modelo
teste$Diabetic <- factor(teste$Diabetic, levels = c(0, 1), labels = c("Negativo", "Positivo"))
acuracia <- mean(teste$Diagnostico_Previsto == teste$Diabetic)
round(acuracia*100,2)


#----(c)-----
# Floresta aleatória
set.seed(806) 
indices_treino <- sample(1:nrow(diabetes), size = n, replace = FALSE)
treino <- diabetes[indices_treino,]
teste <- diabetes[-indices_treino,]

treino$Diabetic <- as.factor(treino$Diabetic)
teste$Diabetic <- as.factor(teste$Diabetic)
floresta <- randomForest(formula = Diabetic ~ ., data = treino, ntree = 200)
print(floresta)

previsao.floresta <- predict(floresta, newdata = teste, type = "class")
print(previsao.floresta)
acuracia_floresta <- mean(previsao.floresta == teste$Diabetic)
round(acuracia_floresta * 100, 2)

#----(d)-----
#Árvore
table(previsao, teste$Diabetic)
probabilidade_arvore <- mean(previsao == teste$Diabetic)
probabilidade_arvore

#Floresta
table(previsao.floresta, teste$Diabetic) 
probabilidade_floresta <- mean(previsao.floresta == teste$Diabetic)
probabilidade_floresta

#----(e)-----
#Com base nos resultados obtidos percebe-se que o modelo de floresta aleatória se saiu relativamente 
#melhor que o modelo de árvore de decisão. Isso porque na floresta obtivemos uma taxa de acerto 
#de "verdadeiros positivos" maior que na árvore, ou seja, ela foi mais capaz de identificar 
#os resultados corretos, além de que ela também uma obteve uma acurácia maior. Sendo assim,
#a Floresta Aleatória foi o mais eficaz na identificação correta de pacientes com diabetes e é 
#mais viavél de ser usada neste conjunto de dados. (porém se for apenas para interpretar as informações,
#a árvore de decisão é mais intuitiva)

#--------------- Exercício 2 ---------------
dados <- read.csv(file = "cerebelo.csv", header = TRUE, sep = ",")
dados

str(dados)
summary(dados)

# (a) 
library(ggplot2)

# Gráfico sem transformação
ggplot(data = dados, aes(x = Body_g, y = Cerebellum_g)) +
  geom_point(color = "blue") +
  labs(title = "Peso do Cerebelo em Função do Peso do Corpo",
       x = "Peso do Corpo (g)",
       y = "Peso do Cerebelo (g)") +
  theme_minimal()

# Gráfico com transformação logarítmica 
ggplot(data = dados, aes(x = Log_body, y = Log_cerebellum)) +
  geom_point(color = "red") +
  labs(title = "Log do Peso do Cerebelo em Função do Log do Peso do Corpo",
       x = "Log(Peso do Corpo)",
       y = "Log(Peso do Cerebelo)") +
  theme_minimal()


#Respondendo: No gráfico com os valores originais, os pontos estão bem espalhados e 
#parece difícil ver uma relação clara, especialmente nos pesos maiores. Já no gráfico 
#com os valores em log, os pontos se alinham de forma muito mais reta, mostrando uma 
#relação mais clara entre o peso do corpo e o peso do cerebelo. Isso indica que, na 
#escala original, a relação entre o peso do corpo e o cerebelo é não-linear. Porém, 
#ao usar o log, vemos uma correlação quase linear, o que sugere que o peso do cerebelo 
#cresce de forma proporcional ao peso do corpo em uma escala logarítmica.


# (b) 
cor_cerebelo_corpo <- cor(dados$Body_g, dados$Cerebellum_g)
print(paste("Coeficiente de correlação entre peso do corpo e peso do cerebelo:", round(cor_cerebelo_corpo, 4)))

# (c) 
cor_log_cerebelo_corpo <- cor(dados$Log_body, dados$Log_cerebellum)
print(paste("Coeficiente de correlação entre Log(peso do corpo) e Log(peso do cerebelo):", round(cor_log_cerebelo_corpo, 4)))

# (d) 
print("Comparação entre os coeficientes de correlação obtidos:")
if (cor_log_cerebelo_corpo > cor_cerebelo_corpo) {
  print("A correlação entre os valores logarítmicos é mais forte, indicando uma relação potencialmente linear entre o peso do corpo e o peso do cerebelo após a transformação.")
} else {
  print("A correlação entre os valores originais é mais forte, indicando uma relação mais linear nos dados não transformados.")
}

# (e) 
modelo <- lm(Log_cerebellum ~ Log_body, data = dados)
summary(modelo)

# Coeficientes da regressão
coeficientes <- coef(modelo)
print(paste("Equação da reta de regressão: Log(Peso do Cerebelo) =", round(coeficientes[1], 4), "+", round(coeficientes[2], 4), "* Log(Peso do Corpo)"))

# Análise dos resultados da regressão
print("Resultados dos testes de hipóteses para o modelo de regressão:")
if (summary(modelo)$coefficients[2, 4] < 0.05) {
  print("O coeficiente de Log(Peso do Corpo) é estatisticamente significativo, indicando que há uma relação significativa entre o peso do corpo e o peso do cerebelo (em escala logarítmica).")
} else {
  print("O coeficiente de Log(Peso do Corpo) não é estatisticamente significativo.")
}

# (e) 
ggplot(data = dados, aes(x = Log_body, y = Log_cerebellum)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Log do Peso do Cerebelo em Função do Log do Peso do Corpo com Reta de Regressão",
       x = "Log(Peso do Corpo)",
       y = "Log(Peso do Cerebelo)") +
  theme_minimal()

# (f) 
residuos <- residuals(modelo)
shapiro_test <- shapiro.test(residuos)
print(shapiro_test)

# Interpretação do teste 
if (shapiro_test$p.value > 0.05) {
  print("Os resíduos seguem uma distribuição normal (não rejeitamos a hipótese nula).")
} else {
  print("Os resíduos não seguem uma distribuição normal (rejeitamos a hipótese nula).")
}

# (g) 
peso_corpo_pred <- 100000
log_peso_corpo_pred <- log10(peso_corpo_pred)

# Calcular a previsão em escala logarítmica
log_peso_cerebelo_pred <- predict(modelo, newdata = data.frame(Log_body = log_peso_corpo_pred))

# Converter de volta para escala original 
peso_cerebelo_pred <- 10^log_peso_cerebelo_pred
print(paste("Peso previsto do cerebelo para uma espécie com peso corporal de 100.000 g:", round(peso_cerebelo_pred, 2), "g"))

#--------------- Exercício 3 ---------------
dados <- read.csv("olive.txt")
head(dados)

# (a) 
dados_num <- dados[, -1] 
dados_padronizados <- scale(dados_num)

# Verifica os dados padronizados
head(dados_padronizados)

# (b) 
set.seed(123) 
modelo_k3 <- kmeans(dados_padronizados, centers = 3)


dados$cluster_k3 <- as.factor(modelo_k3$cluster)

# Gráfico de barras 
library(ggplot2)
ggplot(dados, aes(x = cluster_k3, fill = region)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição dos Clusters por Região (k = 3)",
       x = "Cluster (k = 3)", y = "Contagem") +
  theme_minimal()

# (c) 
#Repetição para k = 4
set.seed(123)
modelo_k4 <- kmeans(dados_padronizados, centers = 4)
dados$cluster_k4 <- as.factor(modelo_k4$cluster)

ggplot(dados, aes(x = cluster_k4, fill = region)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição dos Clusters por Região (k = 4)",
       x = "Cluster (k = 4)", y = "Contagem") +
  theme_minimal()

# Repetição para k = 5
set.seed(123)
modelo_k5 <- kmeans(dados_padronizados, centers = 5)
dados$cluster_k5 <- as.factor(modelo_k5$cluster)

ggplot(dados, aes(x = cluster_k5, fill = region)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição dos Clusters por Região (k = 5)",
       x = "Cluster (k = 5)", y = "Contagem") +
  theme_minimal()
