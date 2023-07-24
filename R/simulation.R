#' @title analyze.simstudy.lm

#' @param the.coefs A data.frame or data.table object of the summary table of estimated coefficients for repeated linear regression models.  Structure is in the form returned by the function simulation::sim.statistics.lm$the.coefs().

#' @param summary.stats A data.frame or data.table object of the summary statistics of repeated linear regression models.  Structure is in the form returned by the function simitation::sim.statistics.lm$summary.stats().

#' @param conf.level A numeric value between 0 and 1 representing the confidence level (1 - significance level).

#' @param the.quantiles A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param coef.name A character value specifying the column of the.coefs that contains the names of the input variables of the linear regression model.

#' @param estimate.name A character value specifying the column of the.coefs that contains the estimated coefficients of the linear regression model.

#' @param lm.p.name A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the linear regression model.

#' @param f.p.name A character value specifying the column of summary.stats that contains the p-value for the linear regression model's F test.
#' @import data.table

#' @return Returns a list with four elements:
#' 1. 'lm.estimate.summary': A data.table that provides a summary of the estimates of the coefficients for each variable in the linear model. The summary includes several descriptive statistics such as the mean, standard error, and various quantiles as specified by the 'the.quantiles' argument.
#' 2. 'lm.p.summary': A data.table that shows the proportion of times the null hypothesis was rejected (i.e., p-value is less than 1 - 'conf.level') and not rejected (i.e., p-value is greater than or equal to 1 - 'conf.level') for each variable in the model.
#' 3. 'lm.stats.summary': A data.table that provides a summary of several overall statistics of the linear model, including sigma (standard error of the residuals), rse (residual standard error), r.squared, adjusted r.squared, and F statistic. The summary includes the mean, standard error, and the specified quantiles for each of these statistics.
#' 4. 'fstatistic.p.summary': A data.table that provides the proportion of times the null hypothesis was rejected (i.e., p-value is less than 1 - 'conf.level') and not rejected (i.e., p-value is greater than or equal to 1 - 'conf.level') for the overall F statistic of the model.

analyze.simstudy.lm <- function(the.coefs, summary.stats, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), coef.name = "Coefficient", estimate.name = "Estimate", lm.p.name = "Pr(>|t|)", f.p.name = "f.pvalue"){


  is.dt.the.coefs <- is.data.table(the.coefs)
  setDT(the.coefs)

  is.dt.summary.stats <- is.data.table(summary.stats)
  setDT(summary.stats)

  lm.estimate.summary <- the.coefs[, internal.quantiles.mean.sd(x = get(estimate.name), the.quantiles = the.quantiles), by = coef.name]

  lm.p.summary <- the.coefs[, .(reject.proportion = mean(get(lm.p.name) < 1 - conf.level), non.reject.proportion = mean(get(lm.p.name) >= 1 - conf.level)), by = coef.name]

  sigma.summary <- data.table(stat = "sigma", internal.quantiles.mean.sd(x = summary.stats$sigma, the.quantiles = the.quantiles, na.rm = T))

  rse.summary <- data.table(stat = "rse", internal.quantiles.mean.sd(x = summary.stats$rse, the.quantiles = the.quantiles, na.rm = T))

  r.squared.summary <- data.table(stat = "r.squared", internal.quantiles.mean.sd(x = summary.stats$r.squared, the.quantiles = the.quantiles, na.rm = T))

  adj.r.squared.summary <- data.table(stat = "adj.r.squared", internal.quantiles.mean.sd(x = summary.stats$adj.r.squared, the.quantiles = the.quantiles, na.rm = T))

  fstatistic.summary <- data.table(stat = "fstatistic", internal.quantiles.mean.sd(x = summary.stats$fstatistic, the.quantiles = the.quantiles, na.rm = T))

  lm.stats.summary <- rbindlist(l = list(sigma.summary, rse.summary, r.squared.summary, adj.r.squared.summary, fstatistic.summary), fill = T)

  fstatistic.p.summary <- summary.stats[, .(reject.proportion = mean(get(f.p.name) < 1 - conf.level), non.reject.proportion = mean(get(f.p.name) >= 1 - conf.level))]


  res <- list(lm.estimate.summary = lm.estimate.summary, lm.p.summary = lm.p.summary, lm.stats.summary = lm.stats.summary, fstatistic.p.summary = fstatistic.p.summary)

  if(is.dt.the.coefs == F){
    setDF(the.coefs)
  }
  if(is.dt.summary.stats == F){
    setDF(summary.stats)
  }

  return(res)
}

#' @title analyze.simstudy.logistic
#' @param the.coefs  A data.frame or data.table object of the summary table of estimated coefficients for repeated logistic regression models.  Structure is in the form returned by the function simitation::sim.statistics.logistic$the.coefs().

#' @param summary.stats A data.frame or data.table object of the summary statistics of repeated logistic regression models.  Structure is in the form returned by the function simitation::sim.statistics.logistic$summary.stats().

#' @param conf.level A numeric value between 0 and 1 representing the confidence level (1 - significance level).

#' @param the.quantiles A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param coef.name A character value specifying the column of the.coefs that contains the names of the input variables of the logistic regression model.

#' @param estimate.name A character value specifying the column of the.coefs that contains the estimated coefficients of the logistic regression model.

#' @param logistic.p.name A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the logistic regression model.
#' @import data.table
#' @return Returns a list with three elements:
#' 1. 'logistic.estimate.summary': A data.table that provides a summary of the estimate of coefficients for each variable in the logistic regression model. The summary includes several descriptive statistics such as the mean, standard error, and various quantiles as specified by the 'the.quantiles' argument.
#' 2. 'logistic.p.summary': A data.table that shows the proportion of times the null hypothesis was rejected (i.e., p-value is less than 1 - 'conf.level') and not rejected (i.e., p-value is greater than or equal to 1 - 'conf.level') for each variable in the model.
#' 3. 'logistic.stats.summary': A data.table that provides a summary of several overall statistics of the logistic regression model such as deviance, Akaike Information Criterion (AIC), degrees of freedom for the residuals, null deviance, degrees of freedom for the null model, number of iterations, and dispersion. The summary includes the mean, standard error, and the specified quantiles for each of these statistics.


analyze.simstudy.logistic <- function(the.coefs, summary.stats, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), coef.name = "Coefficient", estimate.name = "Estimate", logistic.p.name = "Pr(>|z|)"){

  coef.copy <- copy(the.coefs)
  summary.stats.copy <- copy(summary.stats)

  # require(data.table)
  setDT(coef.copy)
  setDT(summary.stats.copy)

  logistic.estimate.summary <- coef.copy[, internal.quantiles.mean.sd(x = get(estimate.name), the.quantiles = the.quantiles, na.rm = T), by = coef.name]

  logistic.p.summary <- coef.copy[, .(reject.proportion = mean(get(logistic.p.name) < 1 - conf.level), non.reject.proportion = mean(get(logistic.p.name) >= 1 - conf.level)), by = coef.name]


  deviance.summary <- data.table(stat = "deviance", internal.quantiles.mean.sd(x = summary.stats.copy$deviance, the.quantiles = the.quantiles, na.rm = T))

  aic.summary <- data.table(stat = "aic", internal.quantiles.mean.sd(x = summary.stats.copy$aic, the.quantiles = the.quantiles, na.rm = T))

  df.residual.summary <- data.table(stat = "df.residual", internal.quantiles.mean.sd(x = summary.stats.copy$df.residual, the.quantiles = the.quantiles, na.rm = T))

  null.deviance.summary <- data.table(stat = "null.deviance", internal.quantiles.mean.sd(x = summary.stats.copy$null.deviance, the.quantiles = the.quantiles, na.rm = T))

  df.null.summary <- data.table(stat = "df.null", internal.quantiles.mean.sd(x = summary.stats.copy$df.null, the.quantiles = the.quantiles, na.rm = T))

  iter.summary <- data.table(stat = "iter", internal.quantiles.mean.sd(x = summary.stats.copy$iter, the.quantiles = the.quantiles, na.rm = T))

  dispersion.summary <- data.table(stat = "dispersion", internal.quantiles.mean.sd(x = summary.stats.copy$dispersion, the.quantiles = the.quantiles, na.rm = T))

  logistic.stats.summary <- rbindlist(l = list(deviance.summary, aic.summary, df.residual.summary, null.deviance.summary, df.null.summary, iter.summary, dispersion.summary), fill = T)

  res <- list(logistic.estimate.summary = logistic.estimate.summary, logistic.p.summary = logistic.p.summary, logistic.stats.summary = logistic.stats.summary)

  return(res)
}



#' @title simulation.steps
#' @param the.steps  A character vector of variables to simulate.  The variables are simulated in the order specified.  Later variables can be generated to depend on earlier variables.  The possible specifications include:
#'  Normal:  "X ~ N(100, 5)" with the mean and SD.

#' Uniform:  "X ~ U(0, 100)" with the minimum and maximum.

#' Poisson:  "X ~ Poisson(3)" with the mean.

#' Binary:  "X ~ Binary(0.5)" with the probability of success.

#' Binomial:  "X ~ Bin(10, 0.2)" with the number of trials and probability of success.

#' Categorical:  "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))" with the values in the first set of parentheses and their respective probabilities in the second.

#' Logistic Regression:  "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

#' Linear Regression:  "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))".  Note that the error term may be specified symbolically with any of the above distributions.

#' @param n A numeric value for the number of observations in each experiment.

#' @param num.experiments A numeric value representing the number of simulated experiments.

#' @param experiment.name A character value providing the name for the column identifying the experiment.

#' @param step.split A character value that separates the name of the variable to be simulated (left side) from its distribution (right side).  Using the.steps = "X ~ N(0,1)" with step.split = "~" will generate a variable named X from a standard Normal distribution.

#' @param seed A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).
#' @return The simulation.steps function returns a data.table with the simulated data for each experiment.

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


#' @title buildsim.binary
#' @description This internal function generates binary data based on a specified probability of success. The probability of success is extracted from the formula provided as a character string (e.g., binary(0.5) indicates a 0.5 probability of success). The function then returns a data frame containing binary values with the specified number of rows (n) and repetitions (num.experiments).
#' @param the.formula A character string specifying the probability of success in the form of a function (e.g., "binary(0.5)" for a 0.5 probability of success).
#' @param the.variable  A character string specifying the name of the variable to be used in the output data frame.
#' @param n The number of rows (samples) to be generated in the output data frame.
#' @param num.experiments The number of experiments to be conducted, resulting in multiple sets of binary data. The default is 1, indicating a single set of binary data.
#' @return The function returns a data frame containing binary values, where each row represents a binary outcome based on the specified probability of success.

buildsim.binary <- function(the.formula, the.variable, n, num.experiments = 1){
  the.chars <- strsplit(x = the.formula, split = "")[[1]]
  first.open.paren <- which(the.chars == "(")[1]

  if(is.na(first.open.paren) == TRUE){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. binary(0.5).")
  }

  if(first.open.paren == 1){
    stop("Error:  Distribution must be specified with a character value indicating a function, e.g. binary(0.5).")
  }

  closing.parens <- which(the.chars == ")")
  last.closing.paren <- closing.parens[length(closing.parens)]

  if(last.closing.paren < first.open.paren){
    stop("Error:  Distribution must end with a closing ), e.g. binary(0.5)")
  }

  intermediate.text <- paste(the.chars[(first.open.paren + 1):(last.closing.paren-1)], collapse = "")


  success.prob <- eval(parse(text = intermediate.text))

  if(success.prob > 1 | success.prob < 0){
    stop("The value inside the parentheses must be a probability between 0 and 1, e.g. binary(0.5) for a 0.5 chance of success.")
  }

  the.values <- sample(x = c(TRUE, FALSE), size = n * num.experiments, replace = T, prob = c(success.prob, 1 - success.prob))

  res <- data.frame(V1 = the.values)
  names(res) <- c(the.variable)

  return(res)
}

#' @title buildsim.binomial
#' @description  This internal function generates data based on a binomial distribution. It simulates a specified number of binomial trials (success/failure experiments) with a given probability of success for each trial. The function returns a data frame containing the generated binomial data.
#' @param the.formula A character string specifying the binomial distribution in the form "Bin(num.trials, success.prob)", where num.trials is the number of trials for each observation, and success.prob is the probability of success in each trial.
#' @param the.variable A character string specifying the name of the variable to be used in the output data frame.
#' @param n The number of observations to be generated in the output data frame. Each observation consists of num.experiments sets of binomial trials.
#' @param num.experiments The number of repetitions or experiments to be conducted, resulting in multiple sets of binomial data. The default is 1, indicating a single set of binomial data.
#' @return The function returns a data frame containing the generated binomial data, where each row represents an observation with the specified number of successes out of num.trials trials.


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

#' @title buildsim.lm
#' @description This internal function generates simulated data for linear regression models. It uses a linear regression formula with fixed terms and a specification of the residual error. The function then returns a data frame containing the generated data based on the specified formula.
#' @param dat The input data for the simulation, which can be either a data.table or data.frame.
#' @param the.formula A character string specifying the linear regression formula in the form of "lm(fixed.terms + residual.error)", where fixed.terms represents the fixed components of the formula, and residual.error specifies the residual error term.
#' @param the.variable A character string specifying the name of the variable to be used in the output data frame.
#' @param n The number of observations to be generated in the output data frame. Each observation consists of num.experiments sets of simulated data.
#' @param num.experiments The number of repetitions or experiments to be conducted, resulting in multiple sets of simulated data. The default is 1, indicating a single set of simulated data.
#' @import data.table
#' @return The function returns a data frame containing the generated data based on the specified linear regression formula. Each row in the data frame represents an observation with the simulated values based on the formula. The column name of the generated data is specified by the the.variable parameter.


buildsim.lm <- function(dat, the.formula, the.variable, n, num.experiments = 1){

  is.dt <- is.data.table(dat)

  if(is.dt == FALSE){
    # require(data.table)
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

#' @title buildsim.logistic
#' @description This internal function generates simulated data for logistic regression models. It uses a logistic regression formula with fixed terms to compute the log-odds, and then applies the logistic function to obtain the probabilities of success (binary outcomes). The function returns a data frame containing the generated binary outcomes based on the specified logistic regression formula.
#' @param dat The input data for the simulation, which can be either a data.table or data.frame.
#' @param the.formula A character string specifying the logistic regression formula in the form of "logistic(fixed.terms)", where fixed.terms represents the fixed components of the formula.
#' @param the.variable A character string specifying the name of the variable to be used in the output data frame.
#' @param n The number of observations to be generated in the output data frame. Each observation consists of num.experiments sets of simulated data.
#' @param num.experiments The number of repetitions or experiments to be conducted, resulting in multiple sets of simulated data. The default is 1, indicating a single set of simulated data.
#' @import data.table
#' @return The function returns a data frame containing the generated binary outcomes (success or failure) based on the specified logistic regression formula. Each row in the data frame represents an observation with the simulated binary outcome. The column name of the generated data is specified by the the.variable parameter.

buildsim.logistic <- function(dat, the.formula, the.variable, n, num.experiments = 1){

  is.dt <- is.data.table(dat)

  if(is.dt == FALSE){
    # require(data.table)
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

#' @title buildsim.normal
#' @description This internal function generates simulated data from a normal distribution. It takes a normal distribution formula in the form of "N(mean, sd)" and returns a data frame containing the simulated data based on the specified mean and standard deviation.
#' @param the.formula A character string specifying the normal distribution formula in the form of "N(mean, sd)", where "mean" represents the mean of the normal distribution, and "sd" represents the standard deviation.
#' @param the.variable A character string specifying the name of the variable to be used in the output data frame.
#' @param n The number of observations to be generated in the output data frame. Each observation consists of num.experiments sets of simulated data.
#' @param num.experiments The number of repetitions or experiments to be conducted, resulting in multiple sets of simulated data.
#' @return The function returns a data frame containing the generated data based on the specified normal distribution formula. Each row in the data frame represents an observation with the simulated values from the normal distribution. The column name of the generated data is specified by the the.variable parameter.

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

#' @title buildsim.poisson
#' @description This internal function generates simulated data from a Poisson distribution. It expects a formula specification in the form of "poisson(lambda)" and returns a data frame containing simulated data based on the specified mean parameter lambda.
#' @param the.formula A character string specifying the Poisson distribution formula in the form of "poisson(lambda)", where "lambda" represents the mean parameter of the Poisson distribution. The function is sensitive to the specific format and will raise errors if it is not complied with.
#' @param the.variable A character string specifying the name of the column to be used in the output data frame.
#' @param n The number of samples to be generated for each experiment.
#' @param num.experiments The number of independent sets of 'n' samples to be generated, effectively multiplying the total number of observations generated by 'n'.
#'
#' @return The function returns a data frame containing the generated Poisson-distributed data as specified by the input parameters. The data frame has 'n * num.experiments' rows and a single column named as per the.variable parameter, each row representing a single observation.

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

#' @title buildsim.sample
#' @description This internal function generates simulated data from a specified sample distribution. It expects a formula specification in the form "sample(('Red', 'Green', 'Blue'), (0.5, 0.3, 0.2))" where the first parentheses set corresponds to possible values, and the second corresponds to their respective probabilities. The function returns a data frame containing simulated data.
#' @param the.formula A character string specifying the sample distribution formula, which contains the possible values in the first set of parentheses and their respective probabilities in the second set. The function will raise an error if the formula is not correctly formatted.
#' @param the.variable A character string specifying the name of the column to be used in the output data frame.
#' @param n The number of samples to be generated for each experiment.
#' @param num.experiments The number of independent sets of 'n' samples to be generated, effectively multiplying the total number of observations generated by 'n'.
#' @param value.split A character string specifying the delimiter used to separate the values in the formula. The default is a comma.
#' @param symbol.open.paren A character string specifying the character used to open the parentheses in the formula. The default is an open parenthesis.
#' @param symbol.close.paren A character string specifying the character used to close the parentheses in the formula. The default is a close parenthesis.
#'
#' @return The function returns a data frame containing the generated sample-distributed data as specified by the input parameters. The data frame has 'n * num.experiments' rows and a single column named as per the.variable parameter, each row representing a single observation.

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

#' @title buildsim.uniform
#' @description This internal function generates simulated data from a uniform distribution. It expects a formula specification in the form of "U(min,max)" and returns a data frame containing simulated data.
#' @param the.formula A character string specifying the uniform distribution formula in the form of "U(min,max)", where "min" and "max" represent the minimum and maximum parameters of the uniform distribution. The function will raise an error if the formula is not correctly formatted.
#' @param the.variable A character string specifying the name of the column to be used in the output data frame.
#' @param n The number of samples to be generated for each experiment.
#' @param num.experiments The number of independent sets of 'n' samples to be generated, effectively multiplying the total number of observations generated by 'n'. Default is 1.
#'
#' @return The function returns a data frame containing the generated uniformly-distributed data as specified by the input parameters. The data frame has 'n * num.experiments' rows and a single column named as per the.variable parameter, each row representing a single observation.

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

#' @title identify.distribution
#' @description This internal function identifies the type of distribution specified in the provided formula, and then calls the appropriate function to generate simulated data based on this distribution. The function supports normal, uniform, poisson, binary, binomial, sample (categorical), logistic, and linear regression (lm) distributions. It expects a formula specification in the form of "dist(param1, param2)".
#' @param dat A data frame or data.table object. Default is NULL. When provided, it is combined with the output data frame generated by the simulation.
#' @param the.step A character string specifying the formula and the variable in the format "variable ~ distribution".
#' @param n The number of samples to be generated for each experiment.
#' @param num.experiments The number of independent sets of 'n' samples to be generated, effectively multiplying the total number of observations generated by 'n'.
#' @param step.split A character string specifying the delimiter used to separate the variable and the distribution in the.step. Default is "~".
#' @param value.split A character string specifying the delimiter used to separate the values in the formula. The default is a comma.
#' @import data.table
#'
#' @return The function returns a data frame or data.table (if dat was provided) containing the simulated data as specified by the input parameters. The column name of the simulated data is the variable provided in the.step.

identify.distribution <- function(dat = NULL, the.step, n, num.experiments, step.split = "~", value.split = ","){
  pieces.step <- trimws(strsplit(x = the.step, split = step.split)[[1]])

  # require(data.table)

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

#' @title internal.quantiles.mean.sd
#' @description This internal function computes quantiles, the mean, and the standard deviation of a given numeric vector, x. If no specific quantiles are specified, it computes the 2.5th, 10th, 25th, 50th (median), 75th, 90th, and 97.5th percentiles by default.
#' @param x A numeric vector for which quantiles, the mean, and standard deviation will be computed.
#' @param the.quantiles A numeric vector of probabilities with values in [0,1] representing the quantiles to be computed. If not provided or empty, the default values are c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975).
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds. Default is TRUE.
#'
#' @return The function returns a data.table containing the computed quantiles (named as 'q.[quantile]'), the mean ('mean'), and the standard deviation ('st.error') of the numeric vector x.

internal.quantiles.mean.sd <- function(x, the.quantiles, na.rm = T){
  if(is.null(the.quantiles) | length(the.quantiles) == 0){
    the.quantiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  }

  # Creating data.table using data.table::data.table()
  stat.summary <- data.table::data.table(x = t(quantile(x = x, probs = the.quantiles, na.rm = na.rm)))

  names(stat.summary) <- gsub(pattern = "%", replacement = "", x = sprintf("q.%s", names(stat.summary)), fixed = T)

  # Adding new columns using the := operator inside data.table::data.table()
  stat.summary[, "mean" := mean(x, na.rm = na.rm)]
  stat.summary[, "st.error" := sd(x, na.rm = na.rm)]

  return(stat.summary)
}

#' @title internal.statistics.one.lm
#' @description This internal utility function performs a linear regression analysis on the input data using the provided formula (the.formula). It generates a summary of the model fit, which includes coefficient estimates, sigma, degrees of freedom, residual standard error (RSE), R-squared, adjusted R-squared, F-statistic and its associated degrees of freedom and p-value. These statistics are organized into two tables and returned as a list.
#' @param the.data A data.table or data.frame containing the data on which the linear regression is performed.
#' @param the.formula A character string specifying the formula of the linear regression model. This formula will be passed to stats::lm() for the regression analysis.
#' @import data.table
#' @return A list containing two data.tables: coef.table containing the coefficients of the model and summary.stats containing various fit statistics of the model.


internal.statistics.one.lm <- function(the.data, the.formula){
  # require(data.table)
  mod <- lm(formula = the.formula, data = the.data)

  summary.mod <- summary(mod)
  coef.table <- as.data.table(x = summary.mod$coefficients, keep.rownames = T)
  names(coef.table)[names(coef.table) == "rn"] <- "Coefficient"

  fstats <- as.data.table(t(summary.mod$fstatistic))
  fstats$p = pf(q = fstats$value, df1 = fstats$numdf, df2 = fstats$dendf, lower.tail = F)

  rse <- sqrt(sum(summary.mod$residuals^2) / summary.mod$df[2])

  summary.stats <- data.table(sigma = summary.mod$sigma, df = summary.mod$df[2], rse = rse, r.squared = summary.mod$r.squared, adj.r.squared = summary.mod$adj.r.squared, fstatistic = fstats$value, f.numdf = fstats$numdf, f.dendf = fstats$dendf, f.pvalue = fstats$p)

  res <- list(coef.table = coef.table, summary.stats = summary.stats)
}

#' @title internal.statistics.one.logistic
#' @description This internal utility function performs a logistic regression analysis on the input data using the provided formula (the.formula) and returns various statistics related to the regression model.
#' @param the.data A data.table or data.frame containing the data on which the logistic regression is performed.
#' @param the.formula The formula representing the logistic regression model.
#' @import data.table
#' @return A list containing two data.tables: coef.table containing the coefficients of the model and summary.stats containing various fit statistics of the model.

internal.statistics.one.logistic <- function(the.data, the.formula){
  # require(data.table)
  mod <- glm(formula = the.formula, family = "binomial", data = the.data)

  summary.mod <- summary(mod)
  coef.table <- as.data.table(x = summary.mod$coefficients, keep.rownames = T)
  names(coef.table)[names(coef.table) == "rn"] <- "Coefficient"

  summary.stats <- data.table(deviance = summary.mod$deviance, aic = summary.mod$aic, df.residual = summary.mod$df.residual, null.deviance = summary.mod$null.deviance, df.null = summary.mod$df.null, iter = summary.mod$iter, dispersion = summary.mod$dispersion)

  res <- list(coef.table = coef.table, summary.stats = summary.stats)
  return(res)
}

#' @title quantile.dt
#' @description This function calculates the quantiles of a numeric vector at the specified probabilities and returns the results as a data.table.
#' @param x A numeric vector whose quantiles are to be calculated.
#' @param probs A numeric vector of probabilities at which the quantiles will be calculated.
#' @param na.rm A logical value indicating whether NA values should be removed before the calculation.
#' @import data.table
#' @return A data.table where the column names are the specified probabilities (prefixed with 'q.') and the values are the corresponding quantiles of x.

quantile.dt <- function(x, probs, na.rm = T){
  # require(data.table)

  y <- quantile(x = x, probs = probs, na.rm = na.rm)

  res <- as.data.table(t(as.matrix(y)))
  names(res) <- gsub(pattern = "%", replacement = "", x = sprintf("q.%s", names(res)), fixed = T)

  return(res)
}


#' @title sim.statistics.lm
#'
#' @description This function fits a linear regression model to data grouped by given variables and
#' returns the regression coefficients and summary statistics for each group.
#'
#' @param simdat A data.table or data.frame that holds the data to be used in multivariable
#' regression models across one or more experiments. The structure is as returned by the function
#' simitation::simulation.steps().
#'
#' @param the.formula A formula object or character string specifying the formula for the
#' regression model. The formula represents the linear regression model to be fitted to the data.
#'
#' @param grouping.variables A character vector of column names from 'simdat' on which to
#' group the data. The linear regression model will be fit separately for each group defined by
#' these variables.
#' @import data.table
#'
#' @return A list containing the regression coefficients (the.coefs) and summary statistics
#' (summary.stats) for each group of data.

sim.statistics.lm <- function(simdat, the.formula, grouping.variables){
  # require(data.table)

  is.dt <- is.data.table(simdat)
  if(is.dt == F){
    setDT(simdat)
  }

  the.coefs <- simdat[, internal.statistics.one.lm(the.data = .SD, the.formula = the.formula)$coef.table, by = grouping.variables]

  summary.stats <- simdat[, internal.statistics.one.lm(the.data = .SD, the.formula = the.formula)$summary.stats, by = grouping.variables]

  res <- list(the.coefs = the.coefs, summary.stats = summary.stats)

  if(is.dt == F){
    setDF(simdat)
  }

  return(res)
}


#' @title sim.statistics.logistic
#' @description This function fits a logistic regression model to data grouped by given variables and
#' returns the regression coefficients and summary statistics for each group.
#'
#' @param simdat A data.table or data.frame that holds the data to be used in multivariable
#' logistic regression models across one or more experiments. The structure is as returned by the
#' function simitation::simulation.steps().
#'
#' @param the.formula A formula object or character string specifying the formula for the
#' logistic regression model. The formula represents the logistic regression model to be fitted to the data.
#'
#' @param grouping.variables A character vector of column names from 'simdat' on which to
#' group the data. The logistic regression model will be fit separately for each group defined by
#' these variables.
#' @import data.table
#'
#' @return A list containing the regression coefficients (the.coefs) and summary statistics
#' (summary.stats) for each group of data.
#'

sim.statistics.logistic <- function(simdat, the.formula, grouping.variables){
  # require(data.table)

  is.dt <- is.data.table(simdat)
  if(is.dt == F){
    setDT(simdat)
  }

  the.coefs <- simdat[, internal.statistics.one.logistic(the.data = .SD, the.formula = the.formula)$coef.table, by = grouping.variables]

  summary.stats <- simdat[, internal.statistics.one.logistic(the.data = .SD, the.formula = the.formula)$summary.stats, by = grouping.variables]

  res <- list(the.coefs = the.coefs, summary.stats = summary.stats)

  if(is.dt == F){
    setDF(simdat)
  }

  return(res)
}

#' @title simstudy.lm
#' @description This function is designed to simulate experiments based on a given specification and perform a linear regression analysis on the data. It returns the generated data, statistics, and analysis.

#' @param the.steps  A character vector of variables to simulate.  The variables are simulated in the order specified.  Later variables can be generated to depend on earlier variables.  The possible specifications include:
#' Normal:  "X ~ N(100, 5)" with the mean and SD.

#' Uniform:  "X ~ U(0, 100)" with the minimum and maximum.

#' Poisson:  "X ~ Poisson(3)" with the mean.

#' Binary:  "X ~ Binary(0.5)" with the probability of success.

#' Binomial:  "X ~ Bin(10, 0.2)" with the number of trials and probability of success.

#' Categorical:  "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))" with the values in the first set of parentheses and their respective probabilities in the second.

#' Logistic Regression:  "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

#' Linear Regression:  "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))".  Note that the error term may be specified symbolically with any of the above distributions.

#' @param n  A numeric value for the number of observations in each experiment.

#' @param num.experiments  A numeric value representing the number of simulated experiments.

#' @param the.formula  A formula object or character value specifying the formula for the regression model.

#' @param conf.level  A numeric value between 0 and 1 representing the confidence level (1 - significance level).

#' @param the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param experiment.name  A character value providing the name for the column identifying the experiment.

#' @param step.split  A character value that separates the name of the variable to be simulated (left side) from its distribution (right side).  Using the.steps = "X ~ N(0,1)" with step.split = "~" will generate a variable named X from a standard Normal distribution.

#' @param coef.name  A character value specifying the column of the.coefs that contains the names of the input variables of the linear regression model.

#' @param estimate.name  A character value specifying the column of the.coefs that contains the estimated coefficients of the linear regression model.

#' @param lm.p.name  A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the linear regression model.

#' @param f.p.name  A character value specifying the column of summary.stats that contains the p-value for the linear regression model's F test.

#' @param seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param vstr  A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).

#' @return A list containing the generated data (simdat), the statistics (statistics), the analysis (sim.analysis) and the steps (the.steps).

simstudy.lm <- function(the.steps, n, num.experiments, the.formula, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", step.split = "~", coef.name = "Coefficient", estimate.name = "Estimate", lm.p.name = "Pr(>|t|)", f.p.name = "f.pvalue", seed = 41, vstr = 3.6){

  simdat.lm <- simulation.steps(the.steps = the.steps, n = n, num.experiments = num.experiments, experiment.name = experiment.name, step.split = step.split, seed = seed, vstr = vstr)

  statistics.lm <- sim.statistics.lm(simdat = simdat.lm, the.formula = the.formula, grouping.variables = experiment.name)

  sim.analysis <- analyze.simstudy.lm(the.coefs = statistics.lm$the.coefs, summary.stats = statistics.lm$summary.stats, conf.level = conf.level, the.quantiles = the.quantiles, coef.name = coef.name, estimate.name = estimate.name, lm.p.name = lm.p.name, f.p.name = f.p.name)

  return(list(the.steps = the.steps, simdat = simdat.lm, statistics = statistics.lm, sim.analysis = sim.analysis))
}


#' @title simstudy.logistic
#' @description This function is designed to simulate experiments based on a given specification and perform a logistic regression analysis on the data. It returns the generated data, statistics, and analysis.
#'
#' @param the.steps  A character vector of variables to simulate.  The variables are simulated in the order specified.  Later variables can be generated to depend on earlier variables.  The possible specifications include:
#'  Normal:  "X ~ N(100, 5)" with the mean and SD.

#' Uniform:  "X ~ U(0, 100)" with the minimum and maximum.

#' Poisson:  "X ~ Poisson(3)" with the mean.

#' Binary:  "X ~ Binary(0.5)" with the probability of success.

#' Binomial:  "X ~ Bin(10, 0.2)" with the number of trials and probability of success.

#' Categorical:  "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))" with the values in the first set of parentheses and their respective probabilities in the second.

#' Logistic Regression:  "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

#' Linear Regression:  "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))".  Note that the error term may be specified symbolically with any of the above distributions.

#' @param n  A numeric value for the number of observations in each experiment.

#' @param num.experiments A numeric value representing the number of simulated experiments.

#' @param the.formula  A formula object or character value specifying the formula for the regression model.

#' @param conf.level  A numeric value between 0 and 1 representing the confidence level (1 - significance level).

#' @param the.quantiles  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

#' @param experiment.name  A character value providing the name for the column identifying the experiment.

#' @param step.split  A character value that separates the name of the variable to be simulated (left side) from its distribution (right side).  Using the.steps = "X ~ N(0,1)" with step.split = "~" will generate a variable named X from a standard Normal distribution.

#' @param coef.name  A character value specifying the column of the.coefs that contains the names of the input variables of the linear regression model.

#' @param estimate.name  A character value specifying the column of the.coefs that contains the estimated coefficients of the linear regression model.

#' @param logistic.p.name  A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the logistic regression model.

#' @param seed  A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).

#' @param vstr A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).

#' @return A list containing the generated data (simdat), the statistics (statistics), the analysis (sim.analysis) and the steps (the.steps).
#'
simstudy.logistic <- function(the.steps, n, num.experiments, the.formula, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), experiment.name = "experiment", step.split = "~", coef.name = "Coefficient", estimate.name = "Estimate", logistic.p.name = "Pr(>|z|)", seed = 39, vstr = 3.6){

  simdat <- simulation.steps(the.steps = the.steps, n = n, num.experiments = num.experiments, experiment.name = experiment.name, step.split = step.split, seed = seed, vstr = vstr)

  statistics <- sim.statistics.logistic(simdat = simdat, the.formula = the.formula, grouping.variables = experiment.name)

  sim.analysis <- analyze.simstudy.logistic(the.coefs = statistics$the.coefs, summary.stats = statistics$summary.stats, conf.level = conf.level, the.quantiles = the.quantiles, coef.name = coef.name, estimate.name = estimate.name, logistic.p.name = logistic.p.name)

  return(list(the.steps = the.steps, simdat = simdat, statistics = statistics, sim.analysis = sim.analysis))
}
