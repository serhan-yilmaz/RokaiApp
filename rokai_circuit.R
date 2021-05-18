rokai_circuit <- function(I, W) {
 # library(Matrix)
  n = length(I)
  tau = 1e8
  r = 1
  In = as.numeric(!is.na(I))
  Rd = In + 1/(r*tau) * (1 - In)
  Rdd = Rd + rowSums(W)/r
  
  Rs = sparseMatrix(
    i = 1:n,
    j = 1:n, 
    x = Rdd,
    dims = c(n, n)
  )
  
  C = I
  C[is.na(C)] <- 0
  
  R = Rs - W / r

  V <- solve(R, C)
  
  return (list("V" = V, "R" = R))
}