library(dplyr)
library(readxl)
library(reshape2)
#setwd("~/DARPALanguageViz_ColbyFord-1-")

#Read in file
data <- read_excel("Bantu/BantuBantoid_frequency_redone.xls") #for Bantu
data <- read_excel("Uto-Aztecan/UT-sound-freqs.xlsx") #for Uto-Aztecan
data <- read_excel("Combined/BantuAndUto-Aztecan_Frequencies.xlsx") #for the combined set

#Transpose the matrix to get languages as columns and sounds as rows
tdata <- as.data.frame(t(data))
colnames(tdata) <- as.character(unlist(tdata[1,]))
tdata <- tdata[-1,]

#Perform K-Means Clustering
dataclust <- (kmeans(
                tdata,
                3,
                iter.max = 10000,
                algorithm = "Lloyd"
                 )
               )$cluster
hist(dataclust)  #Check cluster distribution

tdata$cluster <- factor(as.character(dataclust))  #Join cluster information back to matrix

#see the cluster assignments
soundclusters <- data.frame(sound = rownames(tdata),cluster = tdata$cluster)
#write.csv(soundclusters, file = "DIR/soundassignments.csv", row.names = TRUE, col.names = TRUE)

#Pivot the data
meltdata <- melt(tdata, id = "cluster", measured = c(colnames(tdata)))
meltdata$value <- as.numeric(meltdata$value) #Fix for value being characterized
pivotdata <- as.data.frame(acast(meltdata, variable~cluster, mean))
colnames(pivotdata) <- c("Cluster1", "Cluster2", "Cluster3")
pivotdata$Language <- row.names(pivotdata)

#Output the data
#library(openxlsx)
#write.xlsx(pivotdata, file = "####Cluster.xlsx",colNames = TRUE, rowNames = TRUE)
#write.csv(pivotdata, file = "DIR/####Cluster.csv", row.names = TRUE, col.names = TRUE)


##CLEAN UP
rm(data,meltdata,tdata, soundclusters)

#Join Language Group Information
#groupinfo <- read_excel("BantuLanguageInformation.xlsx")

#plotdata <- merge(groupinfo,pivotdata, by.x = "LanguageName", by.y = "Language") 
#plotdata$`Zone and Name` <- factor(plotdata$`Zone and Name`)

#Plot the data
#library(plotly)
#plot_ly(
#  plotdata,
#  x = ~Cluster2,
#  y = ~Cluster3,
#  # Hover text:
#  text = ~paste0("Language: ", Language, "<br>Group: ", Group),
#  color = ~`Zone and Name`
#) %>%
#layout(
#  title = "Bantu Language Clusters",
#  xaxis = list(title = "Cluster 2"),
#  yaxis = list(title = "Cluster 3")
#  )
