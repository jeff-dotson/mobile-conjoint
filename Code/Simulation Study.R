## Simulate data according to choice sets of size 1-4.  Examine parameter recovery.
library(here)

## Simulate Design Matrix
n.att = 5
att.vec = c(3,3,3,3,3)
att.label = c("att1","att2","att3","att4","att5")
n.task = 10
n.alt = 3
n.version = 100
nhh = 100

source(here("Code","design.R"))
simData = genDesign(n.att = n.att, att.vec = att.vec, att.label = att.label,
                    n.task = n.task, n.alt = n.alt, n.version = n.version)

XX = simData$desmat

## Simulate model parameters and generate data

n.beta = sum(att.vec) - 3
bbar = runif(n.beta,-1,2)

bmat = matrix(double(n.beta*nhh),ncol=n.beta)
for(i in 1:nrow(bmat)){
  bmat[i,] = bbar + rnorm(length(bbar),0,.1)
}

## Format data for use in HMNL estimation

## Compare output

