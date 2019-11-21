
genDesign = function(n.att = NULL, att.vec = NULL, att.label = NULL, n.task = NULL, 
                     n.alt = NULL, n.version = NULL)
  
{

##generate a full factorial design:

library(AlgDesign)
dat <- gen.factorial(att.vec,n.att,center=FALSE,factors = "all",
                   varNames=att.label)

#desT <- optFederov(~.,dat,nTrials = n.task)

## create a design by sampling from the full factorial design

X.des = matrix(double(n.task*n.alt*n.version*(ncol(dat)+3)),ncol=(ncol(dat)+3))
des.tmp = array(0,dim = c(n.task*n.alt,n.att,n.alt))

ii = 1
for(i in 1:n.version){
  fedDes = optFederov(~.,dat,nTrials = n.task*n.alt)
  desT <- matrix(unlist(fedDes$design),ncol=n.att)
  d.eff = fedDes$D
  des.tmp = NULL
  for(jj in 1:n.alt){
    des.tmp[[jj]] = rep(1:(n.task*n.alt))
  }
  for(j in 1:n.task){
    for(k in 1:n.alt){
      ind = sample(des.tmp[[k]], 1)
      X.des[ii,1] = i
      X.des[ii,2] = j
      X.des[ii,3] = k
      X.des[ii,-c(1:3)] = desT[ind,]
      des.tmp[[k]] = des.tmp[[k]][-ind]
      ii = ii + 1
    }
  }
}

## Check to see if this algorithm has the potential to create duplicates in a choice set

colnames(X.des) = c("Version","Task","Concept",colnames(dat))

## Create Dummy Coded Design Matrix
dat.tmp = as.data.frame(X.des)

for(i in 4:ncol(dat.tmp)){
  dat.tmp[,i] = as.factor(dat.tmp[,i])
}

dat.tmp$y = matrix(rnorm(nrow(dat.tmp)),ncol=1)
out = lm(y~.,dat.tmp[,-c(1:3)],x=TRUE)
desmat = out$x[,-1]

# write.csv(desmat,"coded.design.csv")
# 
# write.csv(X.des,"conjoint.design.csv")

return(list(desmat = desmat, d.eff = d.eff))

}


