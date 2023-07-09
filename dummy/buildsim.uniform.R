# Internal Function:  Do not export

buildsim.uniform <- function(the.formula, the.variable, n, num.experiments = 1){
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]
  
  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. U(0,1).")
  }
  
  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. U(0,1).")
  }
  
  open.parens <- which(the.chars == ")")
  last.open.paren <- open.parens[length(open.parens)]
  
  first.comma <- first.open.paren + which(x = the.chars[(first.open.paren + 1):(last.open.paren - 1)] == ",")[1]
  
  if(first.comma == first.open.paren + 1){
    stop("The Uniform function must be specified like U(0, 1), filling in numbers for the min and max.")
  }
  
  the.min <- as.numeric(trimws(paste(the.chars[(first.open.paren + 1):(first.comma -1)], collapse = "")))
  the.max <- as.numeric(trimws(paste(the.chars[(first.comma + 1):(last.open.paren -1)], collapse = "")))
  
  
  the.values <- runif(n = n * num.experiments, min = the.min, max = the.max)
  
  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)
  
  return(res)
}
