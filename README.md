# DEEBtrajs

A S3 class Trajs is defined. It stores data from multiple trajectories (depending on time) of a dynamical system that depends.

Example:
```r
library(DEEBtrajs)
trajs <- makeTrajs(
  time = 1:3,
  state = rbind(c(1,0),c(3,1),c(7,2))
)
normalization <- calculateNormalization(trajs)
normalizedTrajs <- normalization$normalize(trajs)
writeTrajs(normalizedTrajs, "trajsOut.csv")
```
