reg3 = read.table(".../reg3.txt", header=TRUE)
reg3$dens = reg3$pop / reg3$area
attach(reg3)

#Análise descritiva
# Medidas de tendência central
medias <- sapply(reg3[, 2:10], mean)
medianas <- sapply(reg3[, 2:10], median)

# Medidas de dispersão
desvios_padrao <- sapply(reg3[, 2:10], sd)
variancias <- sapply(reg3[, 2:10], var)
intervalos <- sapply(reg3[, 2:10], function(x) max(x) - min(x))

# Medidas de forma
library(e1071)
assimetria <- sapply(reg3[, 2:10], function(x) skewness(x))
curtose <- sapply(reg3[, 2:10], function(x) kurtosis(x))

# Tabela com todas as estatísticas
estatisticas <- data.frame(
  Media = medias,
  Mediana = medianas,
  Desvio_Padrao = desvios_padrao,
  Intervalo = intervalos,
  Assimetria = assimetria,
  Curtose = curtose
)
library(xtable)
estatisticas = round(estatisticas,1)
print(xtable(estatisticas, type="latex"))



# Boxplots
par(mfrow = c(2, 3))
boxplot(percap, main = "Renda Per Capita")
boxplot(analf, main = "Proporção de Analfabetos")
boxplot(crime, main = "Taxa de Criminalidade")
boxplot(estud, main = "Percentual de Estudantes")
boxplot(ndias, main = "Dias Abaixo de Zero")
boxplot(dens, main = "Densidade Populacional")


# Boxplot e Densidade da expectativa de vida
par(mfrow= c(1,2))
hist(expvida, main="Histograma da Expectativa de Vida", freq=FALSE,
     xlab="Espectativa de Vida")
lines(density(expvida), col="red")
boxplot(expvida, main="Boxplot da Expectativa de Vida")

# Histograma e densidade para cada variável
par(mfrow = c(2, 3))
for (col in c(3,4,6,7,8,10)) {
  hist(reg3[, col], main = names(reg3)[col], freq = FALSE, xlab="")
  lines(density(reg3[, col]), col = "red")
}


# Diagramas de dispersão com tendências
par(mfrow = c(2, 3))
plot(percap, expvida, main = "Renda Per Capita vs. Expectativa de Vida", xlab="")
abline(lm(expvida ~ percap), col = "red")
plot(analf, expvida, main = "Analfabetismo vs. Expectativa de Vida", xlab="")
abline(lm(expvida ~ analf), col = "red")
plot(crime, expvida, main = "Criminalidade vs. Expectativa de Vida", xlab="")
abline(lm(expvida ~ crime), col = "red")
plot(estud, expvida, main = "Estudantes vs. Expectativa de Vida", xlab="")
abline(lm(expvida ~ estud), col = "red")
plot(ndias, expvida, main = "Dias Frios vs. Expectativa de Vida", xlab="")
abline(lm(expvida ~ ndias), col = "red")
plot(dens, expvida, main = "Densidade vs. Expectativa de Vida", xlab="")
abline(lm(expvida ~ dens), col = "red")


# Matriz de correlação
var_corr = reg3[, 3:8]
var_corr$dens = reg3[,10]
matriz_correlacao <- cor(var_corr[,])
print(matriz_correlacao)

# Gráfico da matriz de correlação
library(corrplot)
par(mfrow=c(1,1))
corrplot(matriz_correlacao, method = "color", type='lower', tl.col='black',
         cl.ratio = 0.2, tl.srt = 45)



# Função para identificar outliers com o teste de Tukey
identificar_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  limite_inferior <- q1 - 1.5 * iqr
  limite_superior <- q3 + 1.5 * iqr
  outliers <- x[x < limite_inferior | x > limite_superior]
  return(outliers)
}

# Identificar outliers em cada variável
outliers <- lapply(reg3[, 2:10], identificar_outliers)
print(outliers)


#Criando o modelo:

modelo = lm(expvida ~ percap + analf + crime + estud + ndias + dens)

print(xtable(summary(modelo)))

modelo_final = step(modelo, direction='both')

print(xtable(summary(modelo_final)))


# Analise de diagnostico


#teste de normalidade dos residuos
print(shapiro.test(residuals(modelo_final)))
#residuos sao normais

# envelope da normal
par(mfrow=c(1,1))
library(car)
qqPlot(modelo_final, ylab= "Resíduo Studentizado", "Quantis da N(0,1)")


# 
# 
# Verificando homocedasticidade
# 
# residuos studentizados  x valores ajustados
par(mfrow=c(1,1))
res_stud = rstudent(modelo_final)
plot(res_stud, xlab="Índice", ylab="Resíduos Studentizados" , ylim=c(-3, 3), pch=16)
abline(h=2, lty = 2)
abline(h=-2, lty = 2)

library(lmtest)
bptest(modelo_final)
# nao rejeitamos homocedasticidade




#grafico de alavanca
h = lm.influence(modelo_final)$hat

plot(h, xlab="Índice", ylab="Medida H", pch=16)
abline(h=0.5, lty=2)


#distancia de cook
dist_cook = cooks.distance(modelo_final)
N = 50
k = 9
lim_dist_cook = 4/(N - k - 1)
plot(dist_cook, xlab="ìndice", ylab="Distância de Cook", ylim=c(0,0.3), pch=16)
abline(h=lim_dist_cook,lty=2)

