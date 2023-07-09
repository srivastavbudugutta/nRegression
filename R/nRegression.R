#' @Title nRegression
#'
#' @param the.steps
#' @param num.experiments
#' @param model.type
#' @param the.formula
#' @param the.variable
#' @param seed
#' @param n.start
#' @param n.min
#' @param n.max
#' @param increment
#' @param stop.threshold
#' @param power
#' @param conf.level
#' @param verbose
#' @param vstr
#'
#' @return
#' @import data.table
#' @export
#'
#' @examples
nRegression <- function(the.steps, num.experiments, model.type = "lm", the.formula, the.variable, seed, n.start = 50, n.min = 1, n.max = 10000, increment = 50, stop.threshold = 5, power = 0.8, conf.level = 0.95, verbose = TRUE, vstr = 3.6){
  #require(simitation)  --> Insert this once the package is published.
  # require(data.table)
  requireNamespace("simitation")
  requireNamespace("data.table")

  value.lm <- "lm"

  if(!(model.type %in% c("logistic", "glm"))){
    model.type <- value.lm
  }

  lower <- n.min
  upper <- n.max
  n <- n.start

  i = 1

  iterations <- list()
  the.studies <- list()


  while(lower < upper - stop.threshold){

    if(model.type == value.lm){
      the.studies[[i]] <- simstudy.lm(the.steps = the.steps, n = n, num.experiments = num.experiments, the.formula = the.formula, conf.level = conf.level, seed = seed, vstr = vstr)

      calculated.power <- the.studies[[i]]$sim.analysis$lm.p.summary[Coefficient == the.variable, reject.proportion]
    }
    if(model.type != value.lm){
      the.studies[[i]] <- simstudy.logistic(the.steps = the.steps, n = n, num.experiments = num.experiments, the.formula = the.formula, conf.level = conf.level, seed = seed, vstr = vstr)

      calculated.power <- the.studies[[i]]$sim.analysis$logistic.p.summary[Coefficient == the.variable, reject.proportion]
    }

    iterations[[i]] <- data.table(i = i, lower = lower, n = n, upper = upper, power = calculated.power)

    if(verbose == TRUE){
      message(sprintf("i = %d:  lower = %d, n = %d, upper = %d, estimated statistical power = %.4f", i, lower, n, upper, calculated.power))
    }

    if(calculated.power < power){
      lower <- n
    }
    if(calculated.power >= power){
      upper <- n
    }
    n <- min(n + increment, ceiling(mean(c(upper, lower))))
    i <- i + 1
  }

  n.considered <- unlist(lapply(X = iterations, FUN = function(x){return(x$n)}))

  if(!(n %in% n.considered)){
    if(model.type == value.lm){
      the.studies[[i]] <- simstudy.lm(the.steps = the.steps, n = n, num.experiments = num.experiments, the.formula = the.formula, conf.level = conf.level, seed = seed, vstr = vstr)

      calculated.power <- the.studies[[i]]$sim.analysis$lm.p.summary[Coefficient == the.variable, reject.proportion]
    }
    if(model.type != value.lm){
      the.studies[[i]] <- simstudy.logistic(the.steps = the.steps, n = n, num.experiments = num.experiments, the.formula = the.formula, conf.level = conf.level, seed = seed, vstr = vstr)

      calculated.power <- the.studies[[i]]$sim.analysis$logistic.p.summary[Coefficient == the.variable, reject.proportion]
    }
    iterations[[i]] <- data.table(i = i, lower = lower, n = n, upper = upper, power = calculated.power)
  }

  if(verbose == TRUE & !(n %in% n.considered)){
    message(sprintf("i = %d:  lower = %d, n = %d, upper = %d, estimated statistical power = %.4f", i, lower, n, upper, calculated.power))
  }


  specified.power <- power
  iterations <- rbindlist(l = iterations)
  number.exceeding <- iterations[get("power") >= specified.power, .N]

  if(number.exceeding > 0){
    qualifying.iterations <- iterations[get("power") >= specified.power,]
    v <- qualifying.iterations[, which(n == min(n))][1]
    the.sample.size <- qualifying.iterations[v, n]
    the.power <- qualifying.iterations[v, power]

  }
  if(number.exceeding == 0){
    v <- iterations[, which(power == max(power))]
    the.sample.size <- iterations[v, n]
    the.power <- iterations[v, power]
  }

  u <- iterations[, which(n == the.sample.size)][1]
  simstudy <- the.studies[[u]]

  return(list(n = the.sample.size, power = the.power, iterations = iterations, simstudy = simstudy))
}
