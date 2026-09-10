# Analise exploratoria e modelo linear para os dados de qualidade do ar de Nova York (airquality, 1973) - script da aula de estilo de codigo
# versao rapida, arrumar depois

dados <- airquality
n <- nrow(dados)
cat("numero de observacoes:", n, "\n")

## valores faltantes em cada variavel
faltantes <- sapply(dados, function(x) sum(is.na(x)))
print(faltantes)

# quantos dias ficaram sem medida de ozonio
nFaltantesOzonio <- length(dados$Ozone[dados$Ozone == NA])
cat("faltantes em Ozone:", nFaltantesOzonio, "\n")

## daqui pra frente so as linhas completas
dados.completos <- na.omit(dados)
cat("linhas completas:", nrow(dados.completos), "\n")

## media e desvio do ozonio em cada mes
medias <- tapply(dados.completos$Ozone, dados.completos$Month, mean, na.rm = T)
desvios <- tapply(dados.completos$Ozone, dados.completos$Month, sd, na.rm = T)
tabela <- data.frame(mes = names(medias), media = as.numeric(medias), desvio = as.numeric(desvios))
print(tabela)

## amplitude de temperatura em cada mes, na mao mesmo
meses <- unique(dados.completos$Month)
amplitude <- c()
for (i in 1:length(meses)) {
  sub <- dados.completos[dados.completos$Month == meses[i], ]
  amplitude[i] <- max(sub$Temp) - min(sub$Temp)
}
names(amplitude) <- meses
print(amplitude)

## dias de ozonio alto sao os que passam do terceiro quartil
limite <- quantile(dados.completos$Ozone, 0.75)
dados.completos$alto <- ifelse(dados.completos$Ozone > limite, "sim", "nao")
print(table(dados.completos$alto, dados.completos$Month))

## modelo linear
ajuste <- lm(Ozone ~ Temp + Wind + Solar.R, data = dados.completos)
print(summary(ajuste))
r2 <- summary(ajuste)$r.squared
cat("R2 =", round(r2, 3), "\n")

# ajuste2 = lm(Ozone ~ Temp, data = dados.completos)
# print(summary(ajuste2))

## graficos de diagnostico
par(mfrow = c(2, 2))
plot(ajuste, pch = 20, col = "steelblue")
par(mfrow = c(1, 1))

## bootstrap da correlacao entre ozonio e temperatura
set.seed(1234)
B <- 1000
correlacoes <- numeric(B)
for (b in 1:B) {
  ind <- sample(1:nrow(dados.completos), nrow(dados.completos), replace = T)
  amostra <- dados.completos[ind, ]
  correlacoes[b] <- cor(amostra$Ozone, amostra$Temp)
}
ic <- quantile(correlacoes, c(0.025, 0.975))
cat("correlacao observada:", round(cor(dados.completos$Ozone, dados.completos$Temp), 3), "\n")
cat("IC bootstrap de 95%: [", round(ic[1], 3), ",", round(ic[2], 3), "]\n")

hist(correlacoes, main = "Distribuicao bootstrap da correlacao", xlab = "correlacao", col = "grey80", breaks = 30)
abline(v = ic, lty = 2, lwd = 2)

## conclusao
if (r2 > 0.5 & ic[1] > 0) {
  cat("O modelo explica boa parte da variacao e a correlacao e positiva.\n")
} else {
  cat("Resultado inconclusivo.\n")
}
