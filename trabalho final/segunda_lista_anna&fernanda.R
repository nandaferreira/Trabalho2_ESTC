#Nome: Anna Karolyna Pereira Santos         Matrícula: 12221BCC046
#Nome: Fernanda Ferreira de Melo            Matrícula: 12211BCC043

#--------------- Exercício 1 ---------------




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
