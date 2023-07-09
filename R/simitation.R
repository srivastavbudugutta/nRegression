# param: the.coefs:  A data.frame or data.table object of the summary table of estimated coefficients for repeated linear regression models.  Structure is in the form returned by the function simitation::sim.statistics.lm$the.coefs().

# param:  summary.stats:  A data.frame or data.table object of the summary statistics of repeated linear regression models.  Structure is in the form returned by the function simitation::sim.statistics.lm$summary.stats().

# param:  conf.level:  A numeric value between 0 and 1 representing the confidence level (1 - significance level).

# param:  the.quantiles:  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

# param:  coef.name:  A character value specifying the column of the.coefs that contains the names of the input variables of the linear regression model.

# param:  estimate.name:  A character value specifying the column of the.coefs that contains the estimated coefficients of the linear regression model.

# param:  lm.p.name:  A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the linear regression model.

# param:  f.p.name:  A character value specifying the column of summary.stats that contains the p-value for the linear regression model's F test.

analyze.simstudy.lm <- function(the.coefs, summary.stats, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), coef.name = "Coefficient", estimate.name = "Estimate", lm.p.name = "Pr(>|t|)", f.p.name = "f.pvalue"){

  require(data.table)
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


# param: the.coefs:  A data.frame or data.table object of the summary table of estimated coefficients for repeated logistic regression models.  Structure is in the form returned by the function simitation::sim.statistics.logistic$the.coefs().

# param:  summary.stats:  A data.frame or data.table object of the summary statistics of repeated logistic regression models.  Structure is in the form returned by the function simitation::sim.statistics.logistic$summary.stats().

# param:  conf.level:  A numeric value between 0 and 1 representing the confidence level (1 - significance level).

# param:  the.quantiles:  A numeric vector of values between 0 and 1.  Summary statistics to analyze the tests will return the specified quantiles.

# param:  coef.name:  A character value specifying the column of the.coefs that contains the names of the input variables of the logistic regression model.

# param:  estimate.name:  A character value specifying the column of the.coefs that contains the estimated coefficients of the logistic regression model.

# param:  logistic.p.name:  A character value specifying the column of the.coefs that contains the p-values for the tests of the estimated coefficients of the logistic regression model.

analyze.simstudy.logistic <- function(the.coefs, summary.stats, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), coef.name = "Coefficient", estimate.name = "Estimate", logistic.p.name = "Pr(>|z|)"){

  coef.copy <- copy(the.coefs)
  summary.stats.copy <- copy(summary.stats)

  require(data.table)
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

# Internal Function:  Do not export

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


# Internal function, do not export.

internal.quantiles.mean.sd <- function(x, the.quantiles, na.rm = T){

  require(data.table)

  if(is.null(the.quantiles) | length(the.quantiles) == 0){
    the.quantiles <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  }
  stat.summary <- as.data.table(x = t(quantile(x = x, probs = the.quantiles, na.rm = na.rm)))

  names(stat.summary) <- gsub(pattern = "%", replacement = "", x = sprintf("q.%s", names(stat.summary)), fixed = T)

  stat.summary[, eval("mean") := mean(x, na.rm = na.rm)]
  stat.summary[, eval("st.error") := sd(x, na.rm = na.rm)]

  return(stat.summary[])
}


# internal function; do not export.
internal.statistics.one.lm <- function(the.data, the.formula){
  require(data.table)
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


internal.statistics.one.logistic <- function(the.data, the.formula){
  require(data.table)
  mod <- glm(formula = the.formula, family = "binomial", data = the.data)

  summary.mod <- summary(mod)
  coef.table <- as.data.table(x = summary.mod$coefficients, keep.rownames = T)
  names(coef.table)[names(coef.table) == "rn"] <- "Coefficient"

  summary.stats <- data.table(deviance = summary.mod$deviance, aic = summary.mod$aic, df.residual = summary.mod$df.residual, null.deviance = summary.mod$null.deviance, df.null = summary.mod$df.null, iter = summary.mod$iter, dispersion = summary.mod$dispersion)

  res <- list(coef.table = coef.table, summary.stats = summary.stats)
}


# Internal Function, do not export

quantile.dt <- function(x, probs, na.rm = T){
  require(data.table)

  y <- quantile(x = x, probs = probs, na.rm = na.rm)

  res <- as.data.table(t(as.matrix(y)))
  names(res) <- gsub(pattern = "%", replacement = "", x = sprintf("q.%s", names(res)), fixed = T)

  return(res)
}


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



