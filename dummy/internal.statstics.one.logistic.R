internal.statistics.one.logistic <- function(the.data, the.formula){
  require(data.table)
  mod <- glm(formula = the.formula, family = "binomial", data = the.data)
  
  summary.mod <- summary(mod)
  coef.table <- as.data.table(x = summary.mod$coefficients, keep.rownames = T)
  names(coef.table)[names(coef.table) == "rn"] <- "Coefficient"
  
  summary.stats <- data.table(deviance = summary.mod$deviance, aic = summary.mod$aic, df.residual = summary.mod$df.residual, null.deviance = summary.mod$null.deviance, df.null = summary.mod$df.null, iter = summary.mod$iter, dispersion = summary.mod$dispersion)
  
  res <- list(coef.table = coef.table, summary.stats = summary.stats)
}
