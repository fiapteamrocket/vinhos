# limpar memória do R
rm(list=ls(all=TRUE))

# mostrar até 2 casas decimais
options("scipen" = 2)

# Ler arquivo csv

vinhos = read.csv2("C:/Temp/BaseWine_Red_e_White.csv", row.names=1, sep=";")

fix(vinhos)

attach(vinhos)

#Verificando o formato das variáveis
str(vinhos)
vinhos$Vinho <- as.numeric(vinhos$Vinho)
vinhos$quality <- as.numeric(vinhos$quality)
str(vinhos)


summary(vinhos)


#Histogramas
hist(vinhos$fixedacidity)
hist(vinhos$volatileacidity)
hist(vinhos$citricacid)
hist(vinhos$residualsugar)
hist(vinhos$chlorides)
hist(vinhos$freesulfurdioxide)
hist(vinhos$totalsulfurdioxide)
hist(vinhos$density)
hist(vinhos$pH)
hist(vinhos$sulphates)
hist(vinhos$alcohol)

#Bar
barplot(table(vinhos$Vinho))

#Boxplot
boxplot(vinhos$fixedacidity)
boxplot(vinhos$volatileacidity)
boxplot(vinhos$citricacid)
boxplot(vinhos$residualsugar)
boxplot(vinhos$chlorides)
boxplot(vinhos$freesulfurdioxide)
boxplot(vinhos$totalsulfurdioxide)
boxplot(vinhos$density)
boxplot(vinhos$pH)
boxplot(vinhos$sulphates)
boxplot(vinhos$alcohol)

#Tranforma para coluna numerica para coluna do tipo do vinho
vinhos$Vinho <- as.numeric(vinhos$Vinho)
str(vinhos)

#Correlacao
matcor = cor(vinhos)

install.packages("corrgram")
library(corrgram)

corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

install.packages("corrplot")
library(corrplot)

corrplot::corrplot(matcor, method="circle", order="hclust")


# Opções de gráficos: Gráfico de dispersao com o plotly
#TODO
#Fazer grafico de dispersao para todas as colunas

#Gráfico de dispersao para a associação entre área m2 e valor
plot (x = vinhos$fixedacidity, y = vinhos$quality,
      main = "Gráfico de dispersão",
      xlab = "fixedacidity",
      ylab = "quality")

install.packages("plotly")
library(plotly)
plot_ly ( x=vinhos$fixedacidity, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$density, y=vinhos$quality  , type="bar")
 

#Gráfico de dispersao com o ggplot2
install.packages("ggplot2")

library(ggplot2)
ggplot (data= vinhos, aes(x=vinhos$chlorides, y=vinhos$quality )) + 
  geom_point(size=0.4) +
  geom_smooth(method="lm", color ="red", linetype=2) +
  labs(title = "Gráfico de dispersãoo, Valor e Área", x="Área", y="Valor")


#Funcao da professora
panel.cor <- function(x, y, digits=2, prefix ="", cex.cor,
                      ...)  {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y , use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits) [1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor))
    cex <- 0.8/strwidth(txt)
  # abs(r) é para que na saída as correlações ficam proporcionais
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
#Outra forma de ver correlacao
pairs(vinhos, lower.panel=panel.smooth, upper.panel=panel.cor)
str(vinhos)

attach(vinhos)

#PCA
# padr_vinhos <- scale(vinhos)
# 
# fix(padr_vinhos)
# summary(padr_vinhos)
# 
# acpcor <- prcomp(padr_vinhos, scale = TRUE) 
# summary(acpcor)
# 
# plot(1:ncol(padr_vinhos), acpcor$sdev^2, type = "b", xlab = "Componente",
#      ylab = "Variância", pch = 20, cex.axis = 0.8, cex.lab = 0.8)

#Funcao da professora para verificar erro quadratico medio
measures <- function(x) {
  L <- list(
    npar = length(
      coef(x)
    ),
    dfres = df.residual(x),
    nobs = length(
      fitted(x)),
    RMSE = summary(x)$sigma,
    R2 = summary(x)$r.squared,
    R2adj = summary(x)$adj.r.squared,
    PRESS = press(x),
    logLik = logLik(x),
    AIC = AIC(x),
    BIC = BIC(x)
  )
  unlist(L)
}

#Regressao Linear
modelo1 <- lm(quality ~ fixedacidity+volatileacidity+citricacid+residualsugar+
                chlorides+freesulfurdioxide+totalsulfurdioxide+density+pH+sulphates+alcohol+
                Vinho)

summary(modelo1)

# selecionando variáveis por método automático

stepwise<-step(modelo1,direction="both")

stepwise
summary(stepwise)

#Modelo sugerido usando stepwise
modelo_fim = lm(formula = quality ~ fixedacidity + volatileacidity + residualsugar + 
                  chlorides + freesulfurdioxide + totalsulfurdioxide + density + 
                  pH + sulphates + alcohol + Vinho)

Val_pred <- predict(modelo_fim,interval = "prediction", level = 0.95) 

fix(Val_pred)

# intervalo de confianca - grafico para media
fit <- Val_pred[,1] # valores preditos
lower <- Val_pred[,2] # limite inferior
upper <- Val_pred[,3] # limite superior


mse <- mean((vinhos$quality - fit)^2)
sqrt(mse)

erro_usando_media <- mean((vinhos$quality - mean(vinhos$quality))^2)
sqrt(erro_usando_media)


# grafico residuo
rs <- resid(modelo_fim)
plot(predict(modelo_fim), rs, xlab = "Preditor linear",ylab = "Residuos")
abline(h = 0, lty = 2)


#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal

qqnorm(residuals(modelo_fim), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(modelo_fim))

#Teste de Normalidade de Shapiro Wilk

# sE Valor P do teste é pequeno, rejeita-se a hipótese de normalidade dos resíduos e,
# por consequência, conclui-se que os erros não são normalmente distribuídos.

#TODO para o teste de shapiro precisamos tirar uma amostra da base total
shapiro.test(residuals(modelo_fim))

#TODO Fazer os testes de residuos

## Árvore de Regressão

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

modelo_Valor_tree = rpart (quality ~ alcohol+chlorides+citricacid+density+fixedacidity+
                              freesulfurdioxide+pH+residualsugar+sulphates+
                              totalsulfurdioxide+Vinho+volatileacidity, data=vinhos, 
                            cp = 0.001,minsplit = 15,maxdepth=30)

# Faz o Gráfico
rpart.plot(modelo_Valor_tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE, 
           snip=FALSE)

Val_pred_tree = predict(modelo_Valor_tree,interval = "prediction", level = 0.95) 
str(Val_pred_tree)

mse_tree = mean((vinhos$quality - Val_pred_tree)^2)
sqrt(mse_tree)

# Erro usando media
erro_usando_media = mean((vinhos$quality - mean(vinhos$quality))^2)
sqrt(erro_usando_media)


rs_tree <- Val_pred_tree- vinhos$quality
rs_tree_padron <-scale(rs_tree )
plot(predict(modelo_Valor_tree), rs_tree_padron, xlab = "Com Árvore de Regressão",ylab = "Residuos")
abline(h = 0, lty = 2)
