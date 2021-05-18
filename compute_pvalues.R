compute_pvalues <- function(zscores) {
  valids = !is.na(zscores);
  
  Z = zscores[valids]
  
  ncdf = pnorm(q=Z, lower.tail=FALSE)
  pvals = 2 * pmin(ncdf, 1 - ncdf)
  qvals = p.adjust(pvals, method = "BH")
  
  P = rep(NA, length(zscores))
  Q = rep(NA, length(zscores))
  P[valids] = pvals
  Q[valids] = qvals
  
  res = list("PValues" = P, "QValues" = Q)
  return (res)
}