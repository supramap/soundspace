library(dplyr)
library(readxl)
library(reshape2)
#setwd("~/Research/Ward_Language")

#Read in file
bantu <- read_excel("BantuBantoid_frequency_redone.xls")

#Transpose the matrix to get languages as columns and sounds as rows
tbantu <- as.data.frame(t(bantu))
colnames(tbantu) <- as.character(unlist(tbantu[1,]))
tbantu <- tbantu[-1,]

#Perform K-Means Clustering
bantuclust <- (kmeans(
                tbantu,
                     3,
                     iter.max = 10000,
                     algorithm = "Lloyd"
                     )
               )$cluster
hist(bantuclust)  #Check cluster distribution

tbantu$cluster <- factor(as.character(bantuclust))  #Join cluster information back to matrix

#Pivot the data
meltbantu <- melt(tbantu, id = "cluster", measured = c(colnames(tbantu)))
meltbantu$value <- as.numeric(meltbantu$value) #Fix for value being characterized
pivotbantu <- as.data.frame(acast(meltbantu, variable~cluster, mean))
colnames(pivotbantu) <- c("Cluster1", "Cluster2", "Cluster3")
pivotbantu$Language <- row.names(pivotbantu)

#Output the data
#library(openxlsx)
#write.xlsx(pivotbantu, file = "BantuCluster.xlsx",colNames = TRUE, rowNames = TRUE)
#write.csv(pivotbantu, file = "BantuCluster.csv", row.names = TRUE, col.names = TRUE)


##CLEAN UP
rm(bantu,meltbantu,tbantu)

#Join Language Group Information
groupinfo <- read_excel("BantuLanguageInformation.xlsx")
plotdata <- merge(groupinfo,pivotbantu, by.x = "LanguageName", by.y = "Language") 
plotdata$`Zone and Name` <- factor(plotdata$`Zone and Name`)

#Plot the data
library(plotly)
plot_ly(
  plotdata,
  x = ~Cluster2,
  y = ~Cluster3,
  # Hover text:
  text = ~paste0("Language: ", Language, "<br>Group: ", Group),
  color = ~`Zone and Name`
) %>%
layout(
  title = "Bantu Language Clusters",
  xaxis = list(title = "Cluster 2"),
  yaxis = list(title = "Cluster 3")
  )
