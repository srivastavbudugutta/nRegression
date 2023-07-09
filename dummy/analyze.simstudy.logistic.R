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

