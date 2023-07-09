# Internal Function:  Do not export

buildsim.sample <- function(the.formula, the.variable, n, num.experiments, value.split = ",", symbol.open.paren = "(", symbol.close.paren = ")"){
  
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  open.parens <- which(the.chars == symbol.open.paren)

  if(length(open.parens) < 3){
    stop("The sample function must be specified like sample(('Red', 'Green', 'Blue'), (0.5, 0.3, 0.2)), filling in possible values for the first set of parentheses and probabilities for the second set.  There must be at least 3 open paren characters ( in the statement.")
  }

  num.open.parens <- cumsum(the.chars == symbol.open.paren) - cumsum(the.chars == symbol.close.paren)
  open.1 <- which(num.open.parens == 1)
  open.2 <- which(num.open.parens == 2)
  
  begin.x <- min(open.2)
  end.x <- min(open.1[open.1 > begin.x])
  
  x <- eval(parse(text = sprintf("c%s", substring(text = the.formula, first = begin.x, last = end.x))))
  
  begin.prob <- min(open.2[open.2 > end.x])
  end.prob <- min(open.1[open.1 > begin.prob])
  prob <- eval(parse(text = sprintf("c%s", substring(text = the.formula, first = begin.prob, last = end.prob))))
  
  the.values <- sample(x = x, size = n * num.experiments, replace = T, prob = prob)
  
  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)
  
  return(res)
}
