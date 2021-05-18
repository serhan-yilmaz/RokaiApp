rokai_core <- function(Xv, Sx, Wk2s, Wk2k, Ws2s) {
 # source("rokai_circuit.R")
 # source("rokai_weights.R")
  
  # [Ws2s Wk2s'; Wk2s Wkin2kin];
  
  nKin = nrow(Wk2s)
  nSite = ncol(Wk2s)
  
  if(is.null(Wk2k)){
    Wk2k = Rs = sparseMatrix(
      i = c(),
      j = c(), 
      x = T,
      dims = c(nKin, nKin)
    )
  }
  
  W = rbind(cbind(Ws2s, t(Wk2s)), cbind(Wk2s, Wk2k))
  I = rbind(as.matrix(Xv), as.matrix(rep(NA, nrow(Wk2k))))
  
  nSite = nrow(Ws2s)
  site_indices = 1:nSite
  
  rc <- rokai_circuit(I, W)
  Xs <- rc$V[1:length(Xv)]
  
  Fout <- rokai_weights(I, W, rc$R)
  F = Fout[site_indices, site_indices]
  
  S = Sx
  SE = sqrt((F^2)%*%(S^2)) / rowSums(F) # standard error
  Xstderr = SE
  
  return (list("Xs" = Xs, "Xstderr" = Xstderr, "F" = F))
}