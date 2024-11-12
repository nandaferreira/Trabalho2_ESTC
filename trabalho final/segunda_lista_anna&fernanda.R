#Nome: Anna Karolyna Pereira Santos         Matrícula: 12221BCC046
#Nome: Fernanda Ferreira de Melo            Matrícula: 12211BCC043

#--------------- Exercício 1 ---------------




#--------------- Exercício 2 ---------------
# Carregar o arquivo de dados
dados <- read.csv(file = "cerebelo.csv", header = TRUE, sep = ",")
dados

# Verificar a estrutura e resumo dos dados
str(dados)
summary(dados)

# (a) Gráfico de dispersão do peso do cerebelo (y) em relação ao peso do corpo (x)
library(ggplot2)

# Gráfico sem transformação
ggplot(data = dados, aes(x = Body_g, y = Cerebellum_g)) +
  geom_point(color = "blue") +
  labs(title = "Peso do Cerebelo em Função do Peso do Corpo",
       x = "Peso do Corpo (g)",
       y = "Peso do Cerebelo (g)") +
  theme_minimal()

# Gráfico com transformação logarítmica (log base 10)
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


# (b) Calcule o coeficiente de correlação entre as variáveis peso do cerebelo e peso do corpo
cor_cerebelo_corpo <- cor(dados$Body_g, dados$Cerebellum_g)
print(paste("Coeficiente de correlação entre peso do corpo e peso do cerebelo:", round(cor_cerebelo_corpo, 4)))

# (c) Calcule o coeficiente de correlação entre os valores logarítmicos do peso do cerebelo e do peso do corpo
cor_log_cerebelo_corpo <- cor(dados$Log_body, dados$Log_cerebellum)
print(paste("Coeficiente de correlação entre Log(peso do corpo) e Log(peso do cerebelo):", round(cor_log_cerebelo_corpo, 4)))

# (d) Comparar os resultados dos coeficientes de correlação
print("Comparação entre os coeficientes de correlação obtidos:")
if (cor_log_cerebelo_corpo > cor_cerebelo_corpo) {
  print("A correlação entre os valores logarítmicos é mais forte, indicando uma relação potencialmente linear entre o peso do corpo e o peso do cerebelo após a transformação.")
} else {
  print("A correlação entre os valores originais é mais forte, indicando uma relação mais linear nos dados não transformados.")
}

# (e) Regressão linear utilizando os valores transformados em logaritmo
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

# (e) Adicionar a reta de regressão ao gráfico de dispersão log-log
ggplot(data = dados, aes(x = Log_body, y = Log_cerebellum)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Log do Peso do Cerebelo em Função do Log do Peso do Corpo com Reta de Regressão",
       x = "Log(Peso do Corpo)",
       y = "Log(Peso do Cerebelo)") +
  theme_minimal()

# (f) Teste de hipótese para verificar a normalidade dos resíduos
residuos <- residuals(modelo)
shapiro_test <- shapiro.test(residuos)
print(shapiro_test)

# Interpretação do teste de Shapiro-Wilk
if (shapiro_test$p.value > 0.05) {
  print("Os resíduos seguem uma distribuição normal (não rejeitamos a hipótese nula).")
} else {
  print("Os resíduos não seguem uma distribuição normal (rejeitamos a hipótese nula).")
}

# (g) Previsão do peso do cerebelo para uma espécie com 100.000 g de peso corporal
peso_corpo_pred <- 100000
log_peso_corpo_pred <- log10(peso_corpo_pred)

# Calcular a previsão em escala logarítmica
log_peso_cerebelo_pred <- predict(modelo, newdata = data.frame(Log_body = log_peso_corpo_pred))

# Converter de volta para escala original (gramas)
peso_cerebelo_pred <- 10^log_peso_cerebelo_pred
print(paste("Peso previsto do cerebelo para uma espécie com peso corporal de 100.000 g:", round(peso_cerebelo_pred, 2), "g"))

#--------------- Exercício 3 ---------------
