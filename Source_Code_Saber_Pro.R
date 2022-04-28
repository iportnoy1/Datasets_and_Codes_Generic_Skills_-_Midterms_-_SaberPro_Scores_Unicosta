library(vegan)
library(mdatools)
library(psych)
library(resample)
library(igraph)
library(reshape2)
library(qgraph)
library(psych)
library(corrr)
library(ggplot2)
library(dplyr)
library(GGally)
library(stats)
library(dgof)
library("neuralnet")
setwd("C:/Users/idpdl/Desktop/Paper Jessica/Codes and Datasets")

##Loading Dataset
X_SP_GS <- read.csv('Dataset_II.csv', 
                    header = T, dec = '.')
ind <- unlist(lapply(1:nrow(X_SP_GS), function(i){
  sum(is.na(X_SP_GS[i,])) == 0
}))
X_SP_GS <- X_SP_GS[ind,]
X_SP_GS$Period <- as.character(X_SP_GS$Period)

ggplot(X_SP_GS, aes(x=Period, y=Global_SP, fill=Period)) +
  geom_boxplot()+ scale_fill_brewer(palette="RdBu")

# Splitting Data
temp1 <- grepl("2020", X_SP_GS$Period)
temp2 <- grepl("2021", X_SP_GS$Period)
temp <- unlist(lapply(1:length(temp1), function(b){
  temp1[b] || temp2[b]
}))

X_SP_GS$COVID <- temp
X_COVID <- X_SP_GS[X_SP_GS$COVID==1,]
X_No_COVID <- X_SP_GS[X_SP_GS$COVID==0,]
temp1 <- as.data.frame(X_COVID[,c(3:8)])
temp2 <- as.data.frame(X_No_COVID[,c(3:8)])

# Testing Normality
NormalityTests_No_COVID <- as.data.frame(matrix(rep(0,length(temp1[1,])),1,6))
colnames(NormalityTests_No_COVID) <- colnames(temp1)
NormalityTests_COVID <- NormalityTests_No_COVID

for (i in 1:length(temp1[1,])) {
  NormalityTests_No_COVID[1,i] <- shapiro.test(temp2[,i])
  NormalityTests_COVID[1,i] <- shapiro.test(temp1[,i])
}

#Differential Analysis: Testing mean and std. dev. equality
#Hereby we use the Welch's t-test and the Fisher's F-test
pvals_means <- as.data.frame(matrix(rep(0,length(temp1[1,])),1,6))
colnames(pvals_means) <- colnames(temp1)
pvals_std_devs <- pvals_means
for (i in 1:length(temp1[1,])) {
  temp <- t.test(temp1[,i],temp2[,i],alternative="two.sided",var.equal=F)
  pvals_means[1,i] <- temp$p.value
  temp <- var.test(temp1[,i], temp2[,i], alternative = "two.sided")
  pvals_std_devs[1,i] <- temp$p.value
}