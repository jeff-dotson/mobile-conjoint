## Simulate data according to choice sets of size 1-4.  Examine parameter recovery.
library(here)

## Simulate Data
n.att = 5
att.vec = c(3,3,3,3,3)
att.label = c("att1","att2","att3","att4","att5")
n.task = 10
n.alt = 1
n.version = 100

source(here("Code","design.R"))
simData = genDesign(n.att = n.att, att.vec = att.vec, att.label = att.label,
                    n.task = n.task, n.alt = n.alt, n.version = n.version)

XX = simData$desmat
