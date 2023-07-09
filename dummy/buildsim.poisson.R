# Internal Function:  Do not export

buildsim.poisson <- function(the.formula, the.variable, n, num.experiments){
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]
  
  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. poisson(3).")
  }
  
  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. poisson(3).")
  }
  
  close.parens <- which(the.chars == ")")
  last.close.paren <- close.parens[length(close.parens)]
  
  the.mean <- as.numeric(trimws(paste(the.chars[(first.open.paren + 1):(last.close.paren -1)], collapse = "")))
  
  the.values <- rpois(n = n * num.experiments, lambda = the.mean)
  
  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)
  
  return(res)
}
