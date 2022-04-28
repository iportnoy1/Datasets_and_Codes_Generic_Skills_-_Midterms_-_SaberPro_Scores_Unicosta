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

#Loading Data
X <- read.csv('Dataset_I.csv',header = T, dec = '.')

#Descriptive Statistics Visualization

#Period-wise statistics
ggplot(X, aes(x=Period, y=Average, fill=Period)) +
  geom_boxplot()+ scale_fill_brewer(palette="RdBu")

##Skill-wise statistics]
ggplot(X, aes(x=Period, y=RC, fill=Period)) +
  geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=QR, fill=Period)) +
  geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=CS, fill=Period)) +
  geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=EP, fill=Period)) +
  geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=WC, fill=Period)) +
  geom_boxplot()+ scale_fill_brewer(palette="RdBu")
ggplot(X, aes(x=Period, y=MidtermsAverage, fill=Period)) +
  geom_boxplot()+ scale_fill_brewer(palette="RdBu")

# Splitting Data
temp1 <- grepl("2020", X$Period)
temp2 <- grepl("2021", X$Period)
temp <- unlist(lapply(1:length(temp1), function(b){
  temp1[b] || temp2[b]
}))

X$COVID <- temp
X_COVID <- X[X$COVID==1,]
X_No_COVID <- X[X$COVID==0,] 
temp1 <- as.data.frame(X_COVID[,c(3:7,9)])
temp2 <- as.data.frame(X_No_COVID[,c(3:7,9)])

# Testing Normality
NormalityTests_No_COVID <- as.data.frame(matrix(rep(0,length(temp1[1,])),1,6))
colnames(NormalityTests_No_COVID) <- colnames(temp1)
NormalityTests_COVID <- NormalityTests_No_COVID

for (i in 1:length(temp1[1,])) {
  NormalityTests_No_COVID[1,i] <- shapiro.test(temp2[sample(length(temp2[,i]), 5000, replace = F),i])
  NormalityTests_COVID[1,i] <- shapiro.test(temp1[sample(length(temp1[,i]), 5000, replace = F),i])
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

## Correlating Saber Pro scores with Generic Skills scores
X_SP_GS <- read.csv('Dataset_II.csv', 
                    header = T, dec = '.')
ind <- unlist(lapply(1:nrow(X_SP_GS), function(i){
  sum(is.na(X_SP_GS[i,])) == 0
}))
X_SP_GS <- X_SP_GS[ind,]
X_SP_GS$Period <- as.character(X_SP_GS$Period)
c = cor(X_SP_GS$Global_SP, X_SP_GS$Average_G)
c
