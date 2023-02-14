# for Latin hypercube sampling
library(lhs)

# Function for Latin hypercube sampling using the ranges of each of the variables
LatinHypercubeSampling <- function(n, k = 2){
  # n: number of samples to draw
  # k: number of variables over which to sample
  
  # LHS with n samples over k variables
  A <- randomLHS(n = n, k = k)
  
  # transform samples to represent the parameter space
  # create empty matrix to hold parameter space samples
  B <- matrix(nrow = nrow(A), ncol = ncol(A))
  # round uniform distribution between 1 and 99 for 'tree density'
  B[,1] <- round(qunif(A[,1], min = 1, max = 99), digits = 0)
  # binomial distribution for '4 vs. 8 directions'
  B[,2] <- qbinom(p = A[,2], size = 1, prob = 0.5)
  B[, 2] <- B[, 2] * 4 + 4
  
  return(B)
}

# create an empty list to hold each of the LHSs
samples <- list()
# loop through the sample sizes and obtain LHS for each
for(i in 1:length(n)){
  samples[[i]] <- LatinHypercubeSampling(n = n[i], k = 3)
}