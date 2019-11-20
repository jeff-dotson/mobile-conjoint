## Analyze Conjoint Data from Peanut Butter CBC Exercise

data = read.csv("purple2018.csv",header=TRUE)
desmat = read.csv("coded.design.csv",header=FALSE)
var.names = c("X0.Version",
             "Q62",
             "Q64",
             "Q66",
             "Q68",
             "Q70",
             "Q72",
             "Q74",
             "Q76")
YY = data[,var.names]
#Z = matrix(unlist(data[,32:37]),ncol=6)

## 14 choice tasks, 4 alternatives per task, 1 label per alternative, 5 attributes

N = nrow(desmat)
lgtdata = NULL

for(i in 1:N){
  xtemp = desmat[i,]  
  X = matrix(as.numeric(xtemp),ncol=6,byrow=TRUE)
  X = X[,-1]
  Xmat = NULL
  for(k in 1:nrow(X)){
    rtmp = NULL
    for(j in 1:5){
      tmp = max(X[,j])
      xx = rep(0,t=tmp)
      xx[X[k,j]] = 1
      rtmp = c(rtmp,xx)
    }
    Xmat=rbind(Xmat,rtmp)
  }

  ind = 1
  Xmat = Xmat[,-c(1,7,10,11,14)]
  XX = array(dim=c(4,ncol(Xmat),14))
  for(h in 1:14){
    XX[,,h] = Xmat[ind:(ind+3),]
    ind = ind + 4
  }
  
  Y = matrix(unlist(YY[i,]),ncol=1)
  lgtdata[[i]] = list(X = XX, Y=Y)
}

## Recode price as a linear attribute

for(i in 1:length(lgtdata)){
  XX.temp = array(double(4*14*10),dim=c(4,10,14))
  for(j in 1:14){
    XX = lgtdata[[i]]$X[,,j]
    price = c(3,3.5,4,4.5,5,5.5)
    Xnew = as.vector(XX[,10:15]%*%price)
    Xnew[which(Xnew==0)] = 2.5
    Xtemp = cbind(XX[,-c(10:15)],Xnew)    
    XX.temp[,,j] = Xtemp     
  }
  
  lgtdata[[i]]$X = XX.temp  
}



