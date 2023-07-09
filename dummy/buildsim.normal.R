# Internal Function:  Do not export

buildsim.normal <- function(the.formula, the.variable, n, num.experiments){
  
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]
  
  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. N(0,1).")
  }
  
  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. N(0,1).")
  }
  
  open.parens <- which(the.chars == ")")
  last.open.paren <- open.parens[length(open.parens)]
  
  first.comma <- first.open.paren + which(x = the.chars[(first.open.paren + 1):(last.open.paren - 1)] == ",")[1]
  
  if(first.comma == first.open.paren + 1){
    stop("The Normal function must be specified like N(0, 1), filling in numbers for the mean and SD.")
  }
  
  the.mean <- as.numeric(trimws(paste(the.chars[(first.open.paren + 1):(first.comma -1)], collapse = "")))
  the.sd <- as.numeric(trimws(paste(the.chars[(first.comma + 1):(last.open.paren -1)], collapse = "")))
  
  the.values <- rnorm(n = n * num.experiments, mean = the.mean, sd = the.sd)
  
  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)
  
  return(res)
}
