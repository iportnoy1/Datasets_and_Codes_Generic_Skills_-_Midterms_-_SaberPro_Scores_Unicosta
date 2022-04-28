library(corrplot)
library(GGally)

setwd("C:/Users/idpdl/Desktop/Paper Jessica/Codes and Datasets")

##Loading Dataset
X_SP_GS <- read.csv('Dataset_II.csv', 
                    header = T, dec = '.')
ind <- unlist(lapply(1:nrow(X_SP_GS), function(i){
  sum(is.na(X_SP_GS[i,])) == 0
}))
X_SP_GS <- X_SP_GS[ind,]
X_SP_GS$Period <- as.character(X_SP_GS$Period)

# Splitting Data
temp1 <- grepl("2020", X_SP_GS$Period)
temp2 <- grepl("2021", X_SP_GS$Period)

temp <- unlist(lapply(1:length(temp1), function(b){
  temp1[b] || temp2[b]
}))

X_SP_GS$COVID <- temp
X_COVID <- X_SP_GS[X_SP_GS$COVID==1,]
X_No_COVID <- X_SP_GS[X_SP_GS$COVID==0,]

# Correlation Heatmaps
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

corMat=cor(X_No_COVID[,c(3:6,8:12)])
p.mat <- cor.mtest(X_No_COVID[,c(3:6,8:12)])
corrplot(corMat, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.05)
corMat2=cor(X_COVID[,c(3:6,8:12)])
p.mat <- cor.mtest(X_COVID[,c(3:6,8:12)])
corrplot(corMat2, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.05)
