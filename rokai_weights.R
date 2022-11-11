rokai_weights <- function(I, W, R) {
  library(Matrix)
  
  Cin = I
  Rin = R
  
  # Rokai Weights - Inner function starts
  relevantNodes = colSums(W) > 0
  mapping = which(relevantNodes); 
  
  Cin = Cin[relevantNodes]
  Rin = Rin[relevantNodes, relevantNodes]
  indices = !is.na(Cin)
  nodeIndices = mapping[indices]
  
  Ra = Rin[indices, indices]
  Rb = Rin[indices, !indices]
  Rc = Rin[!indices, indices]
  Rd = Rin[!indices, !indices]
  Ainv = solve(Ra)
  
  MA = Rd - Rc%*%Ainv%*%Rb 
  #S = (Rb/MA);
  S = t(solve(t(MA), t(Rb)))
  F = Ainv + Ainv %*% S %*% Rc %*% Ainv
  # Rokai Weights - Inner function ends
  
  #browser()
  Mrelevant2allnodes = sparseMatrix(
    i = 1:length(nodeIndices),
    j = nodeIndices, 
    x = TRUE,
    dims = c(length(nodeIndices), length(I))
  )
  
  Fout = t(Mrelevant2allnodes) %*% F %*% Mrelevant2allnodes
  remainingIdentifiedSites = setdiff(which(!is.na(I)), nodeIndices)
  
  Fdiag = sparseMatrix(
    i = remainingIdentifiedSites,
    j = remainingIdentifiedSites, 
    x = TRUE,
    dims = c(length(I), length(I))
  )
  
  Fout = Fout + Fdiag
  return (Fout)
}