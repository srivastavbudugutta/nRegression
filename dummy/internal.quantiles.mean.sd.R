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
