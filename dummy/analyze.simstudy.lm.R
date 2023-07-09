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

