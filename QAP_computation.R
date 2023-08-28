# compute conc_rate vs correlation
library(readxl)
library(sna)
setwd('/Users/hepengfei/Documents/RA')
times = Sys.time()

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

g <- array(dim=c(2, n.mat, n.mat))

#Infer graph
routes_map <- read.csv("Cocaine transportation routes.csv", header = T)
routes_map$LinkType <- 1
colnames(routes_map) <- c('start_state', 'end_state', 'link')
mat_map <- df2mat(routes_map, state)
g[1,,] <- mat_map

#Ane graphs
filelist <- list.files('alldata')
for(filename in filelist){
#filename <- filelist[1]
corres <- read_excel(paste('alldata',filename, sep='/'))
routes_corres <- corres[, c('start_state', 'end_state', 'Corr_Price_med_pure')]
corres_thres <- data.frame(matrix(ncol = 2, nrow = 100))
colnames(corres_thres) <- c("Correlation_threshold", "QAP")
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
  mat_data <- df2mat(routes_data, state)
  g[2,,] <- mat_data
  QAPtest <- qaptest(g, gcor, g1=1, g2=2)
  if(!is.na(summary(QAPtest)$testval)){
    corres_thres$QAP[j] <- summary(QAPtest)$testval
  }
  else{
    corres_thres$QAP[j] <- 0
  }
}
name_split <- strsplit(filename, split='_')
savename <- paste("QAP",name_split[[1]][2], "TV", name_split[[1]][4], "LB", name_split[[1]][6], "UB", 
                  name_split[[1]][8], "MinObs", paste(strsplit(name_split[[1]][10], split="[.]")[[1]][1],".csv", sep = ""), sep="_")
write.csv(corres_thres, file = paste("QAP", savename, sep="/"), quote = F, row.names = F)
}
timee = Sys.time()
print(timee-times)