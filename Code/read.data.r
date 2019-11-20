## Import and Format Qualtrics Design Data

## Read Design Data
desmat = read.csv("coded.design.v2.csv",header=TRUE)
desmat = desmat[,-1]

## Read Choice Data
XX = read.csv("homie.v1.csv",header=TRUE)

var.names = c("X0.Version",
              "Q4.2",	
              "Q4.3",	
              "Q4.4",	
              "Q4.5",	
              "Q4.6",	
              "Q4.7",	
              "Q4.8",	
              "Q4.9",	
              "Q4.10",	
              "Q4.11",	
              "Q4.12",	
              "Q4.13")
choice.dat = XX[,var.names]

## Compute sample characteristics
nobs = nrow(choice.dat)
ntask = 12
nalt = 3
natt = ncol(desmat) - 3 

regdata = NULL

for(i in 1:nobs){
  X = array(double(ntask*nalt*natt),dim=c(nalt,natt,ntask))
  Y = as.vector(choice.dat[i,2:13])
  Y = as.vector(as.matrix(Y))
  nver = choice.dat[i,"X0.Version"]
  tmp = desmat[which(desmat[,1]==nver),]
  for(j in 1:ntask){
    xtmp = as.matrix(tmp[which(tmp[,2]==j),4:(ncol(desmat))])
    X[,,j] = xtmp
  }
  regdata[[i]] = list(X = X, Y = Y)
}

#Z = cbind(1,XX$Non.Cust,XX$Non.Cust.Aware,XX$Non.Cust.NotAware)

# Z = choice.dat[,14:ncol(choice.dat)]
# 
# ## mean center Z
# Zp = Z
# 
# for(i in 1:nrow(Zp)){
#   Zp[i,] = Zp[i,] - apply(Z,2,mean)
# }
# 
# Zp = cbind(1,Zp)

Zp = matrix(1,ncol=1,nrow=nobs)
Z = Zp
source("rhierMNL.r")
library(compiler)
crmnl = cmpfun(rmnl)
#out = crmnl(regdata,Z,1000,1,.4)
out = crmnl(regdata,Z,100000,100,.4)

save.image("homie.v1.rdata")

plot.ts(apply(out$llkeep,1,sum))
matplot(out$bbardraw,t="l")

cbind(
  apply(out$bbardraw[600:1000,],2,mean),
  t(apply(out$bbardraw[600:1000,],2,quantile,probs=c(.025,.975)))
)

# t(matrix(apply(out$bbardraw[600:1000,],2,mean),byrow=FALSE,ncol=ncol(Z)))
# t(matrix(apply(out$bbardraw[600:1000,],2,quantile,probs=c(.025)),byrow=FALSE,ncol=ncol(Z)))
# t(matrix(apply(out$bbardraw[600:1000,],2,quantile,probs=c(.975)),byrow=FALSE,ncol=ncol(Z)))


## Export individual level utilities

# bmat = matrix(double(natt*nobs),ncol=natt)
# 
# for(i in 1:nobs){
#   bmat[i,] = apply(out$betadraw[600:1000,,i],2,mean)
# }
# 
# write.csv(bmat,"bmat.lineaer.csv")