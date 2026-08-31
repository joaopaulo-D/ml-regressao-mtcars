library(ggplot2)

dados <- mtcars


dim(dados)                 # 32 observacoes x 11 variaveis
str(dados)                 # estrutura / tipos
summary(dados)             # estatisticas descritivas
colSums(is.na(dados))      # nenhum valor ausente

# Desvio-padrao das variaveis de interesse
sapply(dados[, c("mpg", "wt", "hp", "disp", "cyl")], sd)

# Matriz de correlacao
cor(dados[, c("mpg", "wt", "hp", "disp", "cyl")])


g1 <- ggplot(dados, aes(x = wt, y = mpg)) +
  geom_point(size = 3, colour = "#1f4e79", alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x,
              colour = "#c0392b", fill = "#c0392b", alpha = 0.13) +
  labs(
    x = "Peso do veiculo - wt (1000 lbs)",
    y = "Consumo - mpg (milhas/galao)"
  ) +
  theme_minimal(base_size = 12)

print(g1)
# ggsave("fig1_dispersao.png", g1, width = 6.1, height = 3.7, dpi = 200)

# Correlacao de Pearson entre mpg e wt
cor(dados$mpg, dados$wt)

modelo1 <- lm(mpg ~ wt, data = dados)
summary(modelo1)

coef(modelo1)
confint(modelo1)                    
summary(modelo1)$adj.r.squared      

# Diagnostico dos residuos
par(mfrow = c(2, 2)); plot(modelo1); par(mfrow = c(1, 1))


#mpg ~ wt + hp
modelo2 <- lm(mpg ~ wt + hp, data = dados)
summary(modelo2)

summary(modelo2)$adj.r.squared

# O ganho do modelo multiplo sobre o simples e estatisticamente significativo?
anova(modelo1, modelo2)

modelo3 <- lm(mpg ~ wt + hp + disp + cyl, data = dados)
summary(modelo3)

AIC(modelo1, modelo2, modelo3)

set.seed(123)

id <- sample(
  1:nrow(dados),
  size = 0.7 * nrow(dados)     # 22 observacoes (70%)
)

treino <- dados[id, ]          # 22 carros
teste  <- dados[-id, ]         # 10 carros

dim(treino); dim(teste)

# --- Treino ---
modelo_treino <- lm(mpg ~ wt + hp, data = treino)
summary(modelo_treino)

# --- Prever ---
pred <- predict(modelo_treino, newdata = teste)

# --- Avaliar ---
rmse <- sqrt(mean((teste$mpg - pred)^2))
mae  <- mean(abs(teste$mpg - pred))

rmse
mae

rmse / mean(teste$mpg)

# Comparacao: o mesmo split usando apenas wt
modelo_treino_s <- lm(mpg ~ wt, data = treino)
pred_s <- predict(modelo_treino_s, newdata = teste)
sqrt(mean((teste$mpg - pred_s)^2))
mean(abs(teste$mpg - pred_s))

# Tabela observado x predito
comparacao <- data.frame(
  carro     = rownames(teste),
  observado = teste$mpg,
  predito   = round(pred, 3),
  residuo   = round(teste$mpg - pred, 3)
)
print(comparacao, row.names = FALSE)

# Grafico observado x predito
g3 <- ggplot(comparacao, aes(x = predito, y = observado)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "#c0392b") +
  geom_point(size = 3, colour = "#1f4e79", alpha = 0.85) +
  coord_fixed(xlim = c(7, 27), ylim = c(7, 27)) +
  labs(x = "mpg predito", y = "mpg observado") +
  theme_minimal(base_size = 12)

print(g3)