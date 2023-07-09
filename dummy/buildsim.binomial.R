# Internal Function:  Do not export

buildsim.binomial <- function(the.formula, the.variable, n, num.experiments = 1){
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]
  
  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. Bin(10, 0.5).")
  }
  
  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. Bin(10, 0.5).")
  }
  
  closing.parens <- which(the.chars == ")")
  last.closing.paren <- closing.parens[length(closing.parens)]
  
  if(last.closing.paren < first.open.paren){
    stop("Error:  Distribution must end with a closing ), e.g. Bin(10, 0.5)")
  }
  
  intermediate.text <- paste(the.chars[(first.open.paren + 1):(last.closing.paren-1)], collapse = "")
  
  the.pieces <- trimws(strsplit(x = intermediate.text, split = ",")[[1]])
  
  num.trials <- floor(as.numeric(the.pieces[1]))
  success.prob <- as.numeric(the.pieces[2])
  
  if(success.prob > 1 | success.prob < 0){
    stop("The value inside the parentheses must be a probability between 0 and 1, e.g. binary(0.5) for a 0.5 chance of success.")
  }
  
  if(num.trials < 0){
    num.trials <- 1
  }
  
  the.values <- rbinom(n = n * num.experiments, size = num.trials, prob = success.prob)
  
  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)
  
  return(res)
}
