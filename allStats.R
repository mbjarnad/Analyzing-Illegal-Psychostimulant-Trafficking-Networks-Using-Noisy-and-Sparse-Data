#compute all stats for every file
library(readxl)
library(MASS)
#setwd('/Users/hepengfei/Documents/RA')
timestart = Sys.time()
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

summary.df <- function(inf.mat, ane.mat, state){
  inf.list <- mat2list(inf.mat, state)
  ane.list <- mat2list(ane.mat, state)
  len_u <- length(union(inf.list, ane.list))
  len_i <- length(intersect(inf.list, ane.list))
  len_inf <- length(inf.list)
  len_ane <- length(ane.list)
  conc.rate <- len_i/len_u
  if(sd(c(inf.mat))==0){
    correlation = cov(c(inf.mat), c(ane.mat))/sd(c(ane.mat))
  }
  else{
    correlation = cor(c(inf.mat), c(ane.mat))
  }
  df <- data.frame("anecdotal links" = len_ane, "inferred links" = len_inf, "concordance links" = len_i, 
                   "conc" = conc.rate,"corr" = correlation)
  return(df)
}

#Ane matrix from map
routes_map <- read.csv("Cocaine transportation routes.csv", header = T)
routes_map$LinkType <- 1
colnames(routes_map) <- c('start_state', 'end_state', 'link')
ane.mat <- df2mat(routes_map, state)


# Inf matrix from threshold
filelist <- list.files('alldata')
for(filename in filelist){
  #filename <- filelist[1]
  #print(filename)
  stats.df = data.frame(matrix(ncol = 6, nrow = 100))
  colnames(stats.df) = c('threshold', 'anecdotal links', 'inferred links', 
                         'concordance links', 'conc', 'cor')
  corres <- read_excel(paste('alldata',filename, sep='/'))
  routes_corres <- corres[, c('start_state', 'end_state', 'Corr_Price_med_pure')]
  n <- nrow(routes_corres)
  iter = 8
  for(j in 1:100){
    ###generate Ane matrix
    #j=1
    thres <- j/100
    stats.df$threshold[j] = thres
    #print(thres)
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
    #print(sum(inf.mat))
    ane.iter <- sum.path(ane.mat,iter)
    ### compute observed statistics
    observed <- summary.df(inf.mat, ane.iter, state)
    stats.df$`anecdotal links`[j] = observed$anecdotal.links
    stats.df$`inferred links`[j] = observed$inferred.links
    stats.df$`concordance links`[j] = observed$concordance.links
    stats.df$conc[j] = observed$conc
    stats.df$cor[j] = observed$corr
  }
  name_split <- strsplit(filename, split='')
  name_split <- name_split[[1]]
  name_split <- name_split[1:(length(name_split)-4)]
  savename= paste('allStatIter8/statsIter8_', paste(name_split, collapse = ""), '.csv', sep = '')
  write.csv(stats.df, file = savename, quote = F, row.names = F)
}
timeend = Sys.time()
print(timeend-timestart)
