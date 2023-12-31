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

# translating to SAFD
# QF
qf_safd <- list()
for (i in 1:5) {
  var <- list()
  for (j in 1:n) {
    row <- unlist(datax2_QF[j, , i], use.names = FALSE)
    var[j] <- list(translator(X = data.frame(x = row, alpha = c(0, 1, 1, 0)), nl = 2))
  }
  qf_safd[[i]] <- var
}

Cmat_qf = matrix(data = NA,nrow = 5,ncol = 5);
for (i in 1:5) {
  for (j in 1:5) {
    Cmat_qf[i,j]=Bcov(qf_safd[[i]][1:n],qf_safd[[j]][1:n])
  }
}

# QR
qr_safd <- list()
for (i in 1:8) {
  var <- list()
  for (j in 1:n) {
    row <- unlist(datax2_QR[j, , i], use.names = FALSE)
    var[j] <- list(translator(X = data.frame(x = row, alpha = c(0, 1, 1, 0)), nl = 2))
  }
  qr_safd[[i]] <- var
}

Cmat_qr = matrix(data = NA,nrow = 8,ncol = 8);
for (i in 1:8) {
  for (j in 1:8) {
    Cmat_qr[i,j]=Bcov(qr_safd[[i]][1:n],qr_safd[[j]][1:n])
  }
}

# calculating ro
ro_QF <- psych::alpha(Cmat_qf)$total$raw_alpha

ro_QR <- psych::alpha(Cmat_qr)$total$raw_alpha
ro_QR

# creating aggregated fuzzy variable qf
qf <- list()
for(i in 1:n){
  s=TrapezoidalFuzzyNumber(a1 = datax2_QF[i,1,1],a4 = datax2_QF[i,4,1],a2 = datax2_QF[i,2,1],a3 = datax2_QF[i,3,1])
  for(j in 2:5){
    s = s + TrapezoidalFuzzyNumber(a1 = datax2_QF[i,1,j],a4 = datax2_QF[i,4,j],a2 = datax2_QF[i,2,j],a3 = datax2_QF[i,3,j])
  }
  qf[[i]] = ro_QF * s + (1-ro_QF)*(1/n) * s
}

# creating aggregated fuzzy variable qr
qr <- list()
for(i in 1:n){
  s=TrapezoidalFuzzyNumber(a1 = datax2_QR[i,1,1],a4 = datax2_QR[i,4,1],a2 = datax2_QR[i,2,1],a3 = datax2_QR[i,3,1])
  for(j in 2:8){
    s = s + TrapezoidalFuzzyNumber(a1 = datax2_QR[i,1,j],a4 = datax2_QR[i,4,j],a2 = datax2_QR[i,2,j],a3 = datax2_QR[i,3,j])
  }
  qr[[i]] = ro_QR * s + (1-ro_QR)*(1/n) * s
}

# creating fuzzy variable qp
qp <- list()
for(i in 1:n){
  qp[[i]]=TrapezoidalFuzzyNumber(a1 = datax2_QP[i,1],a4 = datax2_QP[i,4],a2 = datax2_QP[i,2],a3 = datax2_QP[i,3])
}

# not sure if to use n = 500 (before assessing fuzziness) or n = 495 (after)


### PART 3: fuzzy regression models ###

# creating final regression dataset
qr_leftS = qr_rightS = qr_core1 = qr_core2 = rep(NA,n)
for(i in 1:n){
  qr_core1[i] = core(qr[[i]])[1]                #trpz parameter: a1
  qr_core2[i] = core(qr[[i]])[2]                #trpz parameter: a2
  qr_leftS[i] = qr_core1[i]-supp(qr[[i]])[1]    #trpz parameter: a3
  qr_rightS[i] = -qr_core2[i]+supp(qr[[i]])[2]   #trpz parameter: a4
}
# we added minus sign before qr_core2 to have the right spread be a positive number
qr_defuzz = unlist(lapply(qr,value))            
qp_defuzz = unlist(lapply(qp,value))
qf_defuzz = unlist(lapply(qf,value))


data_reg = data.frame(qr=qr_defuzz,sex=as.factor(data_restaurant2$sex[-to_drop]),
                      qp=qp_defuzz,qf=qf_defuzz,
                      qr_core1=qr_core1,qr_core2=qr_core2,qr_leftS=qr_leftS,qr_rightS=qr_rightS,
                      qr_coreAvg=(qr_core1+qr_core2)/2)

data_reg = data_reg[complete.cases(data_reg),]

# splitting data into train and test sets
set.seed(21)

sample <- sample(c(TRUE, FALSE), nrow(data_reg), replace=TRUE, prob=c(0.6,0.4))
train  <- data_reg[sample, ]
test   <- data_reg[!sample, ]

# ogólnie ponoć można to zrobić robiąc dwa modele, gdzie za wierzchołki zamiast
# średniej bierze się core'y ale troche nie wiem co wtefy z left i right wiec na razie niech tak zostanie
# non-interactive fuzzy regression model
# we transformed trapezoidal fuzzy varaible to triangular by using average of the cores of the qr variable
 
out_plrs <- fuzzylm(formula = qr~qp+qf+sex, data = train,method = "plrls")
summary(out_plrs)

# interactive fuzzy regression model
X <- out_plrs$x
J <- ncol(X)-1

res <- optim(fn = flr1,par = rep(1,J+3),X,m=train$qr_coreAvg,l=train$qr_leftS,r=train$qr_rightS,X=X,J=J)
beta_est <- res$par
print(beta_est)

# predictions
test_proc <- fuzzylm(formula = qr~qp+qf+sex, data = test,method = "plrls")$x
# ^ wydaje mi sie ze powinno to sie dac rade zrobic lepiej niz w taki sposob

Ypred_non <- test_proc%*%out_plrs$coef
head(Ypred_non)
Ypred_interactive <- flr1_predict(test_proc,beta_est)
head(Ypred_interactive)
head(test[,5:9])

### Part 4: Calculating errors
r2(Ypred_interactive[,1], Ypred_interactive[,2], Ypred_interactive[,3],test$qr_coreAvg, test$qr_leftS, test$qr_rightS)
r2(Ypred_non[,1], Ypred_non[,2], Ypred_non[,3],test$qr_coreAvg, test$qr_leftS, test$qr_rightS)
# it is not symmetrical
r2(test$qr_coreAvg, test$qr_leftS, test$qr_rightS, Ypred_interactive[,1], Ypred_interactive[,2], Ypred_interactive[,3])
r2(test$qr_coreAvg, test$qr_leftS, test$qr_rightS,Ypred_non[,1], Ypred_non[,2], Ypred_non[,3])

# there may be something wrong with the function calculating r2 and rss
rss(Ypred_interactive[,1], Ypred_interactive[,2], Ypred_interactive[,3],test$qr_coreAvg, test$qr_leftS, test$qr_rightS)
rss(Ypred_non[,1], Ypred_non[,2], Ypred_non[,3],test$qr_coreAvg, test$qr_leftS, test$qr_rightS)

# interactive model seems to be better

