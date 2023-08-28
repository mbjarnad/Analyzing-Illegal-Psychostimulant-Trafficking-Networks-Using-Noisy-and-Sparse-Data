# Compute conc_rate and do QAP tests for conc_rate and correlations
# Infered matrices are from folder '0228': 'Corrs_0203_tau_20_kappa_10_TV_100_LB_5_UB_1000_MinObs_15',
# 'Corrs_0211_tau_3_kappa_10_TV_100_LB_5_UB_1000_MinObs_15' and 'Corrs_0211_tau_60_kappa_10_TV_100_LB_5_UB_1000_MinObs_15'

# Consider both 1-1 and 0-0 pairs and recompute conc rate, correlation remains the same
library(readxl)
#setwd('/Users/hepengfei/Documents/RA')
timestart <- Sys.time()
mat.mul <- function(mat1, mat2){
  mat <- mat1 %*% mat2
  m <- matrix(0, nrow(mat), ncol(mat))
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      if(i != j && mat[i,j] > 0){
        m[i,j] = 1
      }
    }
  }
  return(m)
}

mat.exp <- function(mat, k){
  if(k == 0){
    return(mat)
  }
  if(k == 1){
    return(mat)
  }
  s = mat
  for(i in 1:(k-1)){
    s = mat.mul(s, mat)
  }
  return(s)
}

trans.path <- function(mat, rep = 1){
  m <- matrix(0, nrow(mat), ncol(mat))
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      if(i != j && mat[i,j] > 0){
        m[i,j] = 1
      }
    }
  }
  return(m)
}

sum.path <- function(mat, k){
  if(k ==1 ){
    return(mat)
  }
  s = mat
  for(i in 2:k){
    s = s + mat.exp(mat, i)
  }
  return(trans.path(s))
}

#State list
state.board <- read.csv('state board.csv')
st1 <- c()
st2<- c()
for(s in state.board$ST1ST2){
  st1 = c(st1, strsplit(s, split='-')[[1]][1])
  st2 = c(st2, strsplit(s, split='-')[[1]][2])
}
state <- union(st1, st2)
n.mat <- length(state)

df2mat <- function(df, stateList){
  n.mat <- length(stateList)
  newmat <- matrix(0, n.mat, n.mat)
  for(i in 1:nrow(df)){
    newmat[which(state == df$start_state[i]), which(state == df$end_state[i])] = 1
  }
  return(newmat)
}

mat2list1 <- function(mat, state){
  l = c()
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      if(mat[i,j]==1){
        l = c(l, paste(state[i], state[j], sep = '-'))
      }
    }
  }
  return(l)
}

mat2list0 <- function(mat, state){
  l = c()
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      if(mat[i,j]==0 && i!=j){
        l = c(l, paste(state[i], state[j], sep = '-'))
      }
    }
  }
  return(l)
}

summary.df <- function(inf.mat, ane.mat, state){
  inf.list1 <- mat2list1(inf.mat, state)
  inf.list0 <- mat2list0(inf.mat, state)
  ane.list1 <- mat2list1(ane.mat, state)
  ane.list0 <- mat2list0(ane.mat, state)
  len_u <- length(union(union(inf.list0, ane.list0), union(inf.list1, ane.list1)))
  len_i <- length(intersect(inf.list1, ane.list1)) + length(intersect(inf.list0, ane.list0))
  conc.rate <- len_i/len_u
  QAP <- cor(c(inf.mat), c(ane.mat))
  df <- data.frame("concordance rate" = conc.rate,"QAP" = QAP)
  return(df)
}

#Ane matrix from map
routes_map <- read.csv("Cocaine transportation routes.csv", header = T)
routes_map$LinkType <- 1
colnames(routes_map) <- c('start_state', 'end_state', 'link')
ane.mat <- df2mat(routes_map, state)


# Inf matrix from threshold
foldername <- '0228' #put your own folder path
filelist <- list.files(foldername)
for(filename in filelist){
  corres <- read_excel(paste(foldername,filename, sep='/'))
  routes_corres <- corres[, c('start_state', 'end_state', 'Corr_Price_med_pure')]
  n <- nrow(routes_corres)
  for(iter in 1:8){
    analysis <- data.frame(matrix(ncol = 9, nrow = 100))
    colnames(analysis) <- c("Correlation_threshold", "concordance rate",
                            "concordance rate lower", "concordance rate higher", "concordance rate p-value", 
                            "QAP", "QAP lower", "QAP higher", "QAP p-value")
    for(j in 1:100){
      ###generate Ane matrix
      thres <- j/100
      for(i in 1:n){
        if(routes_corres$Corr_Price_med_pure[i] > thres){
          routes_corres$link[i] = 1
        }
        else{
          routes_corres$link[i] = 0
        }
      }
      routes_data <- routes_corres[, c('start_state', 'end_state', 'link')]
      routes_data <- routes_data[which(routes_corres$link == 1),]
      inf.mat <- df2mat(routes_data, state)
      ane.iter <- sum.path(ane.mat,iter)
      ### compute observed statistics
      observed <- summary.df(inf.mat, ane.iter, state)
      QAP.list <- c()
      conc.list <- c()
      ### randomized test
      for(sampled in 1:1000){
        sample.id <- sample(1:49)
        sampled.ane <- ane.iter[sample.id, sample.id]
        sampled.df <- summary.df(inf.mat, sampled.ane, state)
        QAP.list <- c(QAP.list, sampled.df$QAP)
        conc.list <- c(conc.list, sampled.df$concordance.rate)
      }
      QAP.p <- sum(QAP.list>observed$QAP)/1000
      QAP.l <- as.numeric(quantile(QAP.list, prob=c(0.05, 0.95), na.rm = TRUE)[1])
      QAP.h <- as.numeric(quantile(QAP.list, prob=c(0.05, 0.95), na.rm = TRUE)[2])
      QAP.d <- (QAP.h-QAP.l)/2
      conc.p <- sum(conc.list>observed$concordance.rate)/1000
      conc.l <- as.numeric(quantile(conc.list, prob=c(0.05, 0.95), na.rm = TRUE)[1])
      conc.h <- as.numeric(quantile(conc.list, prob=c(0.05, 0.95), na.rm = TRUE)[2])
      conc.d <- (conc.h-conc.l)/2
      analysis$Correlation_threshold[j] <- thres
      analysis$`concordance rate`[j] <- observed$concordance.rate
      analysis$`concordance rate p-value`[j] <- conc.p
      analysis$`concordance rate lower`[j] <- observed$concordance.rate - conc.d
      analysis$`concordance rate higher`[j] <- observed$concordance.rate + conc.d
      analysis$QAP[j] <- observed$QAP
      analysis$`QAP p-value`[j] <- QAP.p
      analysis$`QAP lower`[j] <- observed$QAP - QAP.d
      analysis$`QAP higher`[j] <- observed$QAP + QAP.d
      analysis[is.na(analysis)] <- 0
    }
    name_split <- strsplit(filename, split='_')
    savename <- paste("tests_iter", iter, name_split[[1]][2], name_split[[1]][3], name_split[[1]][4], name_split[[1]][5], name_split[[1]][6], name_split[[1]][7], 
                      name_split[[1]][8], name_split[[1]][9], name_split[[1]][10], name_split[[1]][11], name_split[[1]][12], 
                      name_split[[1]][13], paste(strsplit(name_split[[1]][14], split="[.]")[[1]][1],".csv", sep = ""), sep="_")
    write.csv(analysis, file = paste("newconc", savename, sep="/"), quote = F, row.names = F)
  }
}
timeend <- Sys.time()
print(timeend-timestart)

