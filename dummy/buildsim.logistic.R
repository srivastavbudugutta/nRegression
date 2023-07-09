# Internal Function:  Do not export

buildsim.logistic <- function(dat, the.formula, the.variable, n, num.experiments = 1){
  
  is.dt <- is.data.table(dat)
  
  if(is.dt == FALSE){
    require(data.table)
    setDT(dat)
  }
  
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]
  
  closed.parens <- which(the.chars == ")")
  last.closing.paren <- closed.parens[length(closed.parens)]
  
  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. logistic(0.5 * X + 1.2 * Y).")
  }
  
  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. logistic(0.5 * X + 1.2 * Y).")
  }
  
  if(last.closing.paren < first.open.paren){
    stop("Error:  Distribution must end with a closing ), e.g. logistic(0.5 * X + 1.2 * Y)")
  }
  
  rhs <- trimws(paste(the.chars[(first.open.paren + 1):(last.closing.paren-1)], collapse = ""))
  
  bx <- dat[, eval(parse(text = rhs))]
  
  the.values <- runif(n = dat[, .N]) < (exp(bx)/(1+exp(bx)))
  
  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)
  
  if(is.dt == FALSE){
    setDF(dat)
  }
  
  return(res)
}
