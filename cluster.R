# limpar memória do R
rm(list=ls(all=TRUE))

# mostrar até 2 casas decimais
options("scipen" = 2)

#set seed
set.seed(1)

# Ler arquivo csv

vinhos = read.csv2("C:/Temp/BaseWine_Red_e_White.csv", row.names=1, sep=";")


#comando para gerar em 4 linhas e duas colunas os histogramas
#Gerar histogramas

vinhos$Vinho <- as.numeric(vinhos$Vinho)

#remove a qualiadade, por que a nota nao eh uma caracteristica do vinho
vinhos$quality = NULL

matcor <- cor(vinhos)
print(matcor, digits = 2)

install.packages("corrgram")
library(corrgram)
corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)


# Padroniza
padr_vinho <- scale(vinhos)

fix(padr_vinho)


summary(padr_vinho)

#método hierarquico

hier_cluster<-hclust(dist(padr_vinho),method='ward.D2')
d <- dist(padr_vinho, method = "euclidean") # distance matrix
plot(hier_cluster, ylab='distancia', cex=0.6)

groups <- cutree(hier_cluster, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=5, border="red") 

groups <- cutree(hier_cluster, k=6) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=6, border="blue") 

# Outros métodos que podem ser usados são: "ward", "single", "complete", "average", "mcquitty", "median" ou "centroid".
# A definição de qual método usar varia com o objetivo do estudo e com o tipo de matriz de distância usada.


#método não hierarquico

# Determine number of clusters - Elbow method
wss <- (nrow(padr_vinho )-1)*sum(apply(padr_vinho ,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(padr_vinho ,iter.max=100,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


# utilizando uma forma gráfica
install.packages("tclust")
library(tclust)
clus_teste <- tkmeans(padr_vinho , k = 5, alpha = 0.01)
plot(clus_teste)

# Gerando a quantidade de cluster com Kmeans
attach(padr_vinho)
set.seed(333)
output_cluster<-kmeans(padr_vinho,5,iter=200)
output_cluster

# quantas entidade dentro de cada cluster
segmento<-output_cluster$cluster
table (segmento)

# quais características  de cada cluster
centros<-output_cluster$centers
centros

# quantas rodadas até chegar nos clusters
Qte_iter<-output_cluster$iter
Qte_iter


# Mostrando Resultados
aggregate(vinhos,by=list(segmento),FUN=mean)
# Existe um grupo de vinhos vermelhos doces


# Mostrando Resultados em gráficos

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
install.packages("cluster")

library(cluster)
clusplot(padr_vinho, output_cluster$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0 , cex=0.75)

# Centroid Plot against 1st 2 discriminant functions
install.packages("fpc")
library(fpc)
plotcluster(padr_vinho, output_cluster$cluster) 


# junta os arquivos em colunas
matriz<-cbind(vinhos,padr_vinho,segmento)
fix(matriz)

# append cluster assignment
#no arquivo que eu tenho essa variavel arq01 nao existe alem daqui
matriz<- data.frame(arq01,padr_vinho, segmento) 
fix(matriz)


# Componentes Principais.
acpcor <- prcomp(padr_vinho, scale = TRUE) 
summary(acpcor)

plot(1:ncol(padr_vinho), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Variância", pch = 20, cex.axis = 0.8, cex.lab = 0.8)

sum(acpcor$sdev^2)

#Escolhe 6 dos 12
acpcor$rotation[, 1:6]

biplot(acpcor, xlab = "CP1", ylab = "CP2",cex.lab = 1.0, cex.axis = 1.0)

acpcor <- prcomp(padr_vinho, scale = TRUE, retx = TRUE)

escore1 <- acpcor$x[, 1]
print(escore1)
hist(escore1)

escore2 <- acpcor$x[, 2]

par (mfrow=c(1,2))
hist(escore1)
hist(escore2)
par (mfrow=c(1,1))

attach(vinhos)
vinhos_cpa <-cbind(escore1,escore2)




# usar os escores em uma segmentação, por exemplo.

#método hierarquico

hier_cluster<-hclust(dist(vinhos_cpa),method='ward.D2')
d <- dist(vinhos_cpa, method = "euclidean") # distance matrix
plot(hier_cluster, ylab='distancia', cex=0.6)

groups <- cutree(hier_cluster, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=5, border="blue") 

# Outros métodos que podem ser usados são: "ward", "single", "complete", "average", "mcquitty", "median" ou "centroid".
# A definição de qual método usar varia com o objetivo do estudo e com o tipo de matriz de distância usada.




#método não hierarquico

# Determine number of clusters
wss <- (nrow(vinhos_cpa )-1)*sum(apply(vinhos_cpa ,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(vinhos_cpa ,iter.max=100,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 



attach(vinhos_cpa)

set.seed(333)
output_cluster<-kmeans(vinhos_cpa,5,iter=100)
output_cluster

centros<-output_cluster$centers
centros

clus_vinhos<-output_cluster$cluster
table (clus_vinhos)

matriz_cpa<-cbind(vinhos,vinhos_cpa,clus_vinhos)

matriz_juntos<-cbind(vinhos,segmento,clus_vinhos)

table(segmento,clus_vinhos)

aggregate(vinhos,by=list(clus_vinhos),FUN=mean)


aggregate(padr_vinho,by=list(clus_vinhos),FUN=mean)


# Mostrando Resultados
aggregate(matriz_cpa,by=list(clus_vinhos),FUN=mean)


# Mostrando Resultados em gráficos

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph'
install.packages("cluster")
library(cluster)
clusplot(padr_vinho, output_cluster$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0 , cex=0.75)