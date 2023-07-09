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