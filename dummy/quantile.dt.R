# Internal Function, do not export

quantile.dt <- function(x, probs, na.rm = T){
  require(data.table)
  
  y <- quantile(x = x, probs = probs, na.rm = na.rm)
  
  res <- as.data.table(t(as.matrix(y)))
  names(res) <- gsub(pattern = "%", replacement = "", x = sprintf("q.%s", names(res)), fixed = T)
  
  return(res)
}
