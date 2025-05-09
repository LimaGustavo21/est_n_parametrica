# Criando os dados
tratamento <- c(rep("H1",5), rep("H2",5), rep("H3",5), rep("H4",5))
tempo <- c(3.43,3.68,3.98,4.42,4.89,
           3.53,3.60,3.87,4.22,4.27,
           3.07,3.13,3.40,3.40,3.46,
           2.65,2.77,2.82,3.10,3.18)

dados <- data.frame(tratamento, tempo)

# ANOVA
anova_result <- aov(tempo ~ tratamento, data = dados)
summary(anova_result)

# Obter resíduos
residuos <- residuals(anova_result)

# Histograma dos resíduos
hist(residuos, main="Histograma dos Resíduos", xlab="Resíduos")

# QQ-Plot (verificar normalidade)
qqnorm(residuos)
qqline(residuos, col="red")

# Teste de Shapiro-Wilk para normalidade
shapiro.test(residuos)

# Gráfico de Resíduos vs Valores Ajustados (para homogeneidade de variâncias)
plot(fitted(anova_result), residuos,
     main="Resíduos vs Valores Ajustados",
     xlab="Valores Ajustados", ylab="Resíduos")
abline(h=0, col="red")

# Teste de homogeneidade de variâncias (Teste de Bartlett)
bartlett.test(tempo ~ tratamento, data = dados)




