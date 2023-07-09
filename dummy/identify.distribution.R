# Internal Function:  Do not export

identify.distribution <- function(dat = NULL, the.step, n, num.experiments, step.split = "~", value.split = ","){
  pieces.step <- trimws(strsplit(x = the.step, split = step.split)[[1]])
  
  require(data.table)
  
  the.variable <- pieces.step[1]
  the.formula <- pieces.step[2]
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  
  first.open.paren <- which(the.chars == "(")[1]
  
  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. N(0,1).")
  }
  
  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. N(0,1).")
  }
  
  the.function <- tolower(paste(the.chars[1:(first.open.paren - 1)], collapse = ""))
  
  if(the.function %in% c("n", "norm", "normal")){
    sim.variable <- buildsim.normal(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("u", "unif", "uniform")){
    sim.variable <- buildsim.uniform(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("pois", "poisson")){
    sim.variable <- buildsim.poisson(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("binary")){
    sim.variable <- buildsim.binary(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("bin", "binomial")){
    sim.variable <- buildsim.binomial(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("sample", "categorical")){
    sim.variable <- buildsim.sample(the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments, value.split = value.split)
  }
  if(the.function %in% c("logistic")){
    sim.variable <- buildsim.logistic(dat = dat, the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  if(the.function %in% c("lm", "linear.regression")){
    sim.variable <- buildsim.lm(dat = dat, the.formula = the.formula, the.variable = the.variable, n = n, num.experiments = num.experiments)
  }
  
  if(!is.null(dat)){
    res <- data.table(dat, sim.variable)
  }
  if(is.null(dat)){
    res <- sim.variable
  }
  
  return(res)
}
