# Internal Function:  Do not export

buildsim.lm <- function(dat, the.formula, the.variable, n, num.experiments = 1){
  
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
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. lm(0.5 * X + 1.2 * Y + N(0,2)).")
  }
  
  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. logistic(0.5 * X + 1.2 * Y).")
  }
  
  if(last.closing.paren < first.open.paren){
    stop("Error:  Distribution must end with a closing ), e.g. logistic(0.5 * X + 1.2 * Y)")
  }
  
  rhs <- paste(the.chars[(first.open.paren + 1):(last.closing.paren-1)], collapse = "")
  
  pieces.rhs <- trimws(strsplit(x = rhs, split = "+", fixed = T)[[1]])
  
  num.pieces.rhs <- length(pieces.rhs)
  
  if(num.pieces.rhs < 2){
    stop("Error:  The right side of the linear regression formula must include fixed terms and a specification of the residual error, such as lm(0.5 * X + 1.2 * Y + N(0,1))")
  }
  
  fixed.component <- paste(pieces.rhs[1:(num.pieces.rhs - 1)], collapse = " + ")
  
  bx <- dat[, eval(parse(text = fixed.component))]
  
  the.residuals <- identify.distribution(dat = dat, the.step = sprintf("residual ~ %s", pieces.rhs[num.pieces.rhs]), n = n, num.experiments = num.experiments)$residual
  
  the.values <- bx + the.residuals
  
  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)
  
  if(is.dt == FALSE){
    setDF(dat)
  }
  
  return(res)
}
