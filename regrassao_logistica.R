# limpar memória do R
rm(list=ls(all=TRUE))

# mostrar até 2 casas decimais
options("scipen" = 2)

#set seed
set.seed(1)

# Ler arquivo csv

vinhos = read.csv2("C:/Temp/BaseWine_Red_e_White.csv", row.names=1, sep=";")

install.packages("scorecard")
library(scorecard)

dt_list = split_df(vinhos, ratio = 0.75, seed = 66)
train = dt_list$train
test = dt_list$test

attach(train)
train$classificacao <- ifelse(quality >= 6, "bom", "ruim")

table(train$quality)

prop.table(table(train$quality))

#comando para gerar em 4 linhas e duas colunas os plots
par (mfrow=c(2,3))
plot(as.factor(train$fixedacidity), as.factor(train$quality),main='fixedacidity')
#TODO fazer para todas as propriedades

plot(Titanic_train$Sex, as.factor(Titanic_train$Survived),main='Sex')
plot(as.factor(Titanic_train$Age), as.factor(Titanic_train$Survived),main='Age')
plot(Titanic_train$Embarked, as.factor(Titanic_train$Survived),main='Embarked')
plot(as.factor(Titanic_train$SibSp), as.factor(Titanic_train$Survived),main='SibSp')
plot(as.factor(Titanic_train$Parch), as.factor(Titanic_train$Survived), main='Parch')

# column percentages 
# O melhor o vinho branco
# a maioria dos vinhos branco tiram nota ruim
prop.table(table(as.factor(train$Vinho),as.factor(train$quality)),2)


boxplot(alcohol ~ classificacao, main='Vinho')


train$classificacao <- ifelse(quality >= 6, T, F)

#TODO: Ve se precisa disso
aggregate(train,
          by = list(train$classificacao),
          FUN = mean)

# Carrega o pacote: árvore de decisão
library(rpart) 
library(rpart.plot) 

attach(train)


# informaçoes dos Parãmetros do Modelo
modelo01 <- rpart (as.factor(classificacao) ~ fixedacidity+volatileacidity+citricacid+residualsugar+
                          chlorides+freesulfurdioxide+totalsulfurdioxide+density+pH+sulphates+alcohol+
                          Vinho, maxdepth=15, train)


# Faz o Gráfico (type=0 a 4) (extra=0 a 9)
rpart.plot(modelo01, type=4, extra=104, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,
           digits=2, varlen=-8, faclen=10,
           cex=0.6, tweak=1,
           compress=TRUE,
           snip=FALSE)

summary(modelo01)


## Predict com tipo 'classe' retorna se sobreviveu ou não.

previsto.com.modelo<-predict(modelo01, train, type='class')

matriz.de.confusão<-table(train$classificacao, previsto.com.modelo)
matriz.de.confusão

diagonal <- diag(matriz.de.confusão)
Acc <-  sum(diagonal)/sum(matriz.de.confusão)
Acc

previsto.valid<-predict(modelo01, test , type='class')

test$previsto=previsto.valid
test$classificacao <- ifelse(test$quality >= 6, T, F)
test$errou = ifelse(test$previsto != test$classificacao, 1, 0)

#70%
sum(test$errou) / length(test)
