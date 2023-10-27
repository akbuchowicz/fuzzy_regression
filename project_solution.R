### set-up ###

rm(list=ls()); graphics.off()
library(SAFD); library(FuzzyNumbers); library(fuzzyreg); library(EM.Fuzzy); library(psych)
#source("utilities.R")

load("data_restaurant2.RData")
str(data_restaurant2)
n <- NROW(data_restaurant2)


datax_QF = array(data = NA,dim = c(n,4,5)) #store QF data into a 'units x trpz paramaters x num of QF items' array
for(j in 1:5){
  datax_QF[,,j] = as.matrix(data_restaurant2[,grep(x = colnames(data_restaurant2),pattern = paste0("QF",j))])
}

datax_QR = array(data = NA,dim = c(n,4,8)) #store QR data into a 'units x trpz paramaters x num of QR items' array 
for(j in 1:8){
  datax_QR[,,j] = as.matrix(data_restaurant2[,grep(x = colnames(data_restaurant2),pattern = paste0("QR",j))])
}

datax_QP = array(data = NA,dim = c(n,4)) #store data into a 'units x trpz paramaters' array
datax_QP = as.matrix(data_restaurant2[,grep(x = colnames(data_restaurant2),pattern = paste0("QP",1))])


### PART 1: checking fuzziness ###

is_fuzzy<- function(num){
  if (num[1]>num[2]) return(FALSE)
  if(num[2]>num[3]) return(FALSE)
  if(num[3]>num[4]) return(FALSE)
  return(TRUE)
}

to_drop = c()
for(j in 1:5){
  to_drop = append(to_drop, which(apply(datax_QF[,,j], MARGIN=1, FUN=is_fuzzy)==FALSE))
  to_drop = append(to_drop, which(apply(datax_QR[,,j], MARGIN=1, FUN=is_fuzzy)==FALSE))
}

to_drop = append(to_drop, which(apply(datax_QP, MARGIN=1, FUN=is_fuzzy)==FALSE))
to_drop = unique(to_drop)

datax2_QF <- datax_QF[-to_drop,,]
datax2_QR <- datax_QR[-to_drop,,]
datax2_QP <- datax_QP[-to_drop,]

### PART 2: aggregation ###

JF = length(datax2_QF[1,1,])

