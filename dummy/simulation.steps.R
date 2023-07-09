# param:  the.steps:  A character vector of variables to simulate.  The variables are simulated in the order specified.  Later variables can be generated to depend on earlier variables.  The possible specifications include:

## Normal:  "X ~ N(100, 5)" with the mean and SD.

## Uniform:  "X ~ U(0, 100)" with the minimum and maximum.

## Poisson:  "X ~ Poisson(3)" with the mean.

## Binary:  "X ~ Binary(0.5)" with the probability of success.

## Binomial:  "X ~ Bin(10, 0.2)" with the number of trials and probability of success.

## Categorical:  "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))" with the values in the first set of parentheses and their respective probabilities in the second.

## Logistic Regression:  "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

## Linear Regression:  "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))".  Note that the error term may be specified symbolically with any of the above distributions.

# param:  n:  A numeric value for the number of observations in each experiment.

# param:  num.experiments:  A numeric value representing the number of simulated experiments.

# param:  experiment.name:  A character value providing the name for the column identifying the experiment.

# param:  step.split:  A character value that separates the name of the variable to be simulated (left side) from its distribution (right side).  Using the.steps = "X ~ N(0,1)" with step.split = "~" will generate a variable named X from a standard Normal distribution.

# param:  seed:  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

# param:  vstr:  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).

simulation.steps <- function(the.steps, n, num.experiments = 1, experiment.name = "experiment", step.split = "~", seed = 62, vstr = 3.6){
  
  n <- pmax(1, floor(n[1]))
  
  num.experiments <- pmax(1, floor(num.experiments))
  
  num.steps <- length(the.steps)
  
  if(num.steps < 1){
    stop("Error:  the.steps hould specify a character vector with the sequence of steps to build the simulation.")
  }
  RNGversion(vstr = vstr)
  set.seed(seed = seed)
  
  dat <- NULL
  for(i in 1:num.steps){
    dat <- identify.distribution(dat = dat, the.step = the.steps[i], n = n, num.experiments = num.experiments)
  }

  setDT(dat)
  
  dat[, eval(experiment.name) := rep.int(x = 1:num.experiments, times = n)]
  
  setorderv(x = dat, cols = experiment.name, order = 1L)
  
  setcolorder(x = dat, neworder = experiment.name)
  
  return(dat[])
}
