### set-up ###

rm(list=ls()); graphics.off()
library(SAFD); library(FuzzyNumbers); library(fuzzyreg); library(EM.Fuzzy); library(psych)
source("utilities.R")

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
}

for (j in 1:8) {
  to_drop = append(to_drop, which(apply(datax_QR[,,j], MARGIN=1, FUN=is_fuzzy)==FALSE))
}

to_drop = append(to_drop, which(apply(datax_QP, MARGIN=1, FUN=is_fuzzy)==FALSE))
to_drop = unique(to_drop)

datax2_QF <- datax_QF[-to_drop,,]
datax2_QR <- datax_QR[-to_drop,,]
datax2_QP <- datax_QP[-to_drop,]

n <- dim(datax2_QF)[1]
### PART 2: aggregation ###

JF = length(datax2_QF[1,1,])

# creating covariance matrices for QF and QR fuzzy variables

# due to lack of understanding of SAFD representation of fuzzy variables
# placeholder values for ro will be used used

ro_QF <- 1/4
ro_QR <- 1/4
# ^ placeholder values

# creating aggregated fuzzy variable qf
qf <- list()
for(i in 1:n){
  s=TrapezoidalFuzzyNumber(a1 = datax2_QF[i,1,1],a4 = datax2_QF[i,4,1],a2 = datax2_QF[i,2,1],a3 = datax2_QF[i,3,1])
  for(j in 2:5){
    s = s + TrapezoidalFuzzyNumber(a1 = datax2_QF[i,1,j],a4 = datax2_QF[i,4,j],a2 = datax2_QF[i,2,j],a3 = datax2_QF[i,3,j])
  }
  qf[[i]] = ro_QF * s + (1-ro_QF)*(1/n) * s
}
qf

# creating aggregated fuzzy variable qr
qr <- list()
for(i in 1:n){
  s=TrapezoidalFuzzyNumber(a1 = datax2_QR[i,1,1],a4 = datax2_QR[i,4,1],a2 = datax2_QR[i,2,1],a3 = datax2_QR[i,3,1])
  for(j in 2:8){
    s = s + TrapezoidalFuzzyNumber(a1 = datax2_QR[i,1,j],a4 = datax2_QR[i,4,j],a2 = datax2_QR[i,2,j],a3 = datax2_QR[i,3,j])
  }
  qr[[i]] = ro_QR * s + (1-ro_QR)*(1/n) * s
}

# not sure if to use n = 500 (before assessing fuzziness) or n = 495 (after)
