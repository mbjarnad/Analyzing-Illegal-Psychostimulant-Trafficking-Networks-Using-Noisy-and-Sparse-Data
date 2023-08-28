# Compute conc_rate, correlation for all the data in folder 'alldata' which contains all 25 xlsx files.
library(readxl)
setwd('/Users/hepengfei/Documents/RA')

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
Infer <- read.csv('Cocaine transportation routes.csv')
state.board <- read.csv('state board.csv')
st1 <- c()
st2<- c()
for(s in state.board$ST1ST2){
  st1 = c(st1, strsplit(s, split='-')[[1]][1])
  st2 = c(st2, strsplit(s, split='-')[[1]][2])
}
state <- union(st1, st2)
n.mat <- length(state)
Inf.mat <- matrix(0, n.mat, n.mat)
for(i in 1:nrow(Infer)){
  Inf.mat[which(state == Infer$Sending.State[i]), which(state == Infer$Receiving.State[i])] = 1
}

mat2list <- function(mat, state){
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
filelist <- list.files('alldata')
for(m in 2:8){
  inf <- data.frame(sum.path(Inf.mat,m))
  rownames(inf) <- state
  colnames(inf) <- state
  pair_in_map <- mat2list(inf, state)
  for(filename in filelist){
    corres <- read_excel(paste('alldata',filename, sep='/'))
    routes_corres <- corres[, c('start_state', 'end_state', 'Corr_Price_med_pure')]
    corres_thres <- data.frame(matrix(ncol = 6, nrow = 100))
    colnames(corres_thres) <- c("Correlation_threshold", "Concordance_Rate", "Size_Union", "Size_Intersection", "Size_Inferred", "Size_Anecdotal")
    n <- nrow(routes_corres)
    for(j in 1:100){
      thres <- j/100
      corres_thres$Correlation_threshold[j] <- thres
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
      pair_in_corres <- paste(routes_data$start_state, routes_data$end_state, sep ="-")
      len_u <- length(union(pair_in_corres, pair_in_map))
      len_i <-length(intersect(pair_in_corres, pair_in_map))
      corres_thres$Size_Union[j] <- len_u
      corres_thres$Size_Intersection[j] <- len_i
      corres_thres$Concordance_Rate[j] <- len_i/len_u
      corres_thres$Size_Inferred[j] <- length(pair_in_map)
      corres_thres$Size_Anecdotal[j] <- length(pair_in_corres)
    }
    name_split <- strsplit(filename, split='_')
    savename <- paste("Concord",name_split[[1]][2], "TV", name_split[[1]][4], "LB", name_split[[1]][6], "UB", 
                      name_split[[1]][8], "MinObs", paste("iter", m, sep = ''), paste(strsplit(name_split[[1]][10], split="[.]")[[1]][1], ".csv", sep = ""), sep="_")
    write.csv(corres_thres, file = paste("thres_iter", savename, sep="/"), quote = F, row.names = F)
  }
}



