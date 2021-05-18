rokai_kinase_weights <- function(Xv, Wkin2site, F) {
  indA = !is.na(Xv)
  indB = indA & (colSums(Wkin2site) > 0);
  
  Wk2s = Wkin2site[,indB]
  Fq = F[indB, indA]
  
  Fk = (Wk2s %*% Fq) / rowSums(Wk2s)
  nodeIndices = which(indA)
  
  Mrelevant2allnodes = sparseMatrix(
    i = 1:length(nodeIndices),
    j = nodeIndices, 
    x = TRUE,
    dims = c(length(nodeIndices), length(Xv))
  )
  Fk = Fk %*% Mrelevant2allnodes
  
  return (Fk)
}