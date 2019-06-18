
vinhos = read.csv2("/Users/letticianicoli/Desktop/Projects/Fiap/vinhos/BaseWine_Red_e_White.csv", row.names=1, sep=";")

summary(vinhos)
str(vinhos)
any(is.na.data.frame(vinhos))
unique(vinhos$quality)


#Ignora id do vinho
vinhos = vinhos[2:14]


#Altera virgula por ponto
vinhos$fixedacidity = as.numeric(gsub("\\,", ".", vinhos$fixedacidity))
vinhos$volatileacidity = as.numeric(gsub("\\,", ".", vinhos$volatileacidity))
vinhos$citricacid = as.numeric(gsub("\\,", ".", vinhos$citricacid))
vinhos$residualsugar = as.numeric(gsub("\\,", ".", vinhos$residualsugar))
vinhos$chlorides = as.numeric(gsub("\\,", ".", vinhos$chlorides))
vinhos$freesulfurdioxide = as.numeric(gsub("\\,", ".", vinhos$freesulfurdioxide))
vinhos$totalsulfurdioxide = as.numeric(gsub("\\,", ".", vinhos$totalsulfurdioxide))
vinhos$density = as.numeric(gsub("\\,", ".", vinhos$density))
vinhos$pH = as.numeric(gsub("\\,", ".", vinhos$pH))
vinhos$alcohol = as.numeric(gsub("\\,", ".", vinhos$alcohol))
vinhos$sulphates = as.numeric(gsub("\\,", ".", vinhos$sulphates))
vinhos$quality <- as.numeric(vinhos$quality)
vinhos$Vinho <- as.numeric(vinhos$Vinho)

#Avaliar quais colunas são maiores que 1
str(vinhos)

#By looking at data we can say that Columns 1, 4, 6, 7, 9, 11, 12 can cause issues, 
#we will try to normalize them so that there value are in 0 to 1 range.
norm_data <- sapply(vinhos[,c(1,4,6,7,9,11,12)], function(x) (x - min(x))/(max(x) - min(x)))
norm_data <- data.frame(norm_data)  
head(norm_data)

#Binding the normalised data with other data
wineData_norm <- cbind(vinhos[,c(2,3,5,8,10,13)],norm_data)
head(wineData_norm)

#Ignora tipo do vinho
vinhos_noColor <- vinhos[1:12]
#Pode-se utilizar o scale para normalizacao tambem
wineData_scaled <- scale(vinhos_noColor)
head(wineData_scaled)
class(wineData_scaled)
wineData_scaled_df <- as.data.frame(wineData_scaled)

wineData_norm$Vinho <- as.numeric(wineData_norm$Vinho)
#Lets find correlation using cor()
corr_norm <- round(cor(wineData_norm),1)
corr_norm

install.packages("ggcorrplot")
library(ggplot2)

ggcorrplot(corr_norm, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Wine Data", 
           ggtheme=theme_dark)

corr_scaled <- round(cor(wineData_scaled_df),1)

##Lets plot same for Scaled data
ggcorrplot(corr_scaled, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Wine Data", 
           ggtheme=theme_dark)

# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 10 cluster centers
for (i in 1:10) {
  km.out <- kmeans(wineData_norm, centers = i)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}
wss
# Plot total within sum of squares vs. number of clusters
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

km <- kmeans(wineData_norm, 2, iter.max = 140 , algorithm="Lloyd", nstart=100)
km

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph'
install.packages("cluster")
library(cluster)
clusplot(wineData_norm, km$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0 , cex=0.75)
