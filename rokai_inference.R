rokai_inference <- function(X, S, Fkin2site) {
  indA = !is.na(X)
  Fk = Fkin2site[,indA]
  X = X[indA]
  S = S[indA]
  
  Akin = (Fk %*% X) / rowSums(Fk)
  Skin = sqrt((Fk^2)%*%(S^2)) / rowSums(Fk) # standard error
  Zkin = Akin / Skin
  
  return (list("A" = as.matrix(Akin), "S" = as.matrix(Skin), "Z" = as.matrix(Zkin)))
}