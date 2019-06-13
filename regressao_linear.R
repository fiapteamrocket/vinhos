# limpar memória do R
rm(list=ls(all=TRUE))

# mostrar até 2 casas decimais
options("scipen" = 2)

#set seed
set.seed(1)

# Ler arquivo csv

vinhos = read.csv2("C:/Temp/BaseWine_Red_e_White.csv", row.names=1, sep=";")

fix(vinhos)

attach(vinhos)

#Verificando o formato das variáveis
str(vinhos)
vinhos$Vinho <- as.numeric(vinhos$Vinho)

vinhos$cor <- ifelse(vinhos$Vinho == "WHITE", "blue", "red")

vinhos$quality <- as.numeric(vinhos$quality)
str(vinhos)


summary(vinhos)

#Falar sobre a quantidade de tipo de vinho
#Falar sobre a media da qualidade


#Histogramas
hist(vinhos$quality)

#Falar que nao eh normal.
library(ggplot2)
install.packages("GGally")
library(GGally)

#TODO Soh comparar a qualidade entre os tipos de vinho
qplot(quality, data = vinhos, fill = cor, binwidth = 1) +
  scale_x_continuous(breaks = seq(3,10,1), lim = c(3,10)) +
  scale_y_sqrt()

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

#Remove a coluna de cor
vinhos$cor = NULL

#Correlacao
matcor = cor(vinhos)

install.packages("corrgram")
library(corrgram)

corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

install.packages("corrplot")
library(corrplot)

#Quanto mais chlorides menor a qualidade
#Quanto mais vinhos$freesulfurdioxide maior o vinhos$totalsulfurdioxide
#Quanto menor a desinty menor a quantidade de alcool
#QUanto menor a volatieacidity menor a qualidade
#Quanto maior a acides fixa maior a densidade

corrplot::corrplot(matcor, method="circle", order="hclust")


# Opções de gráficos: Gráfico de dispersao com o plotly
#TODO
#Fazer grafico de dispersao para todas as colunas

#Gráfico de dispersao para a associação entre área m2 e valor
install.packages("plotly")
library(plotly)
plot_ly ( x=vinhos$fixedacidity, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$volatileacidity, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$citricacid, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$residualsugar, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$chlorides, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$freesulfurdioxide, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$totalsulfurdioxide, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$density, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$pH, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$sulphates, y=vinhos$quality  , type="scatter")
plot_ly ( x=vinhos$alcohol, y=vinhos$quality  , type="scatter")


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

#PValue ta bom!

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


#Pega a media da qualidade menos o fit elevado ao quadrado
mse <- mean((vinhos$quality - fit)^2)
sqrt(mse)

#Pega o erro usando a media
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
                            cp = 0.001,minsplit = 15,maxdepth=50)

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
