## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup_1------------------------------------------------------------------
library(nRegression)

## -----------------------------------------------------------------------------
power = 0.9

## -----------------------------------------------------------------------------
step.age <- "Age ~ N(45, 10)"
step.female <- "Female ~ binary(0.53)"
step.health.percentile <- "Health.Percentile ~ U(0,100)"
step.exercise.sessions <- "Exercise.Sessions ~ Poisson(2)"
step.diet <- "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))"

## -----------------------------------------------------------------------------
step.healthy.lifestyle <- "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"


## -----------------------------------------------------------------------------
step.weight <- "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))"

## -----------------------------------------------------------------------------
the.steps <- c(step.age, step.female, step.health.percentile, step.exercise.sessions, step.diet, step.healthy.lifestyle, step.weight)


## -----------------------------------------------------------------------------
the.formula.logistic <- Healthy.Lifestyle ~ Age + Female + Health.Percentile + Exercise.Sessions + Weight

## -----------------------------------------------------------------------------
the.variable = "Exercise.Sessions"

## -----------------------------------------------------------------------------
conf.level = 0.95
model.type = "logistic"
seed = 41
vstr = 3.6
num.experiments = 10

n.start = 200
n.min = 1
n.max = 300
increment = 100
stop.threshold = 1

## -----------------------------------------------------------------------------
n.logistic = nRegression(the.steps = the.steps, num.experiments = num.experiments, the.formula = the.formula.logistic, the.variable = the.variable, seed = seed, n.start = n.start, n.min = n.min, n.max = n.max, increment = increment, stop.threshold = stop.threshold, power = power, model.type = model.type, verbose = T)

## -----------------------------------------------------------------------------
names(n.logistic)

## -----------------------------------------------------------------------------
n.logistic$n

## -----------------------------------------------------------------------------
n.logistic$power

## -----------------------------------------------------------------------------
n.logistic$iterations

## -----------------------------------------------------------------------------
names(n.logistic$simstudy)

## -----------------------------------------------------------------------------
the.formula.lm <- Weight ~ Age + Female + Health.Percentile + Exercise.Sessions + Healthy.Lifestyle
model.type = "lm"
num.experiments <- 10

n.start = 500
n.max <- 800
increment = 100
stop.threshold = 10

the.variable = "Healthy.LifestyleTRUE"

## -----------------------------------------------------------------------------
n.lm = nRegression(the.steps = the.steps, num.experiments = num.experiments, the.formula = the.formula.lm, the.variable = the.variable, seed = seed, n.start = n.start, n.max = n.max, increment = increment, stop.threshold = stop.threshold, power = power, model.type = model.type, verbose = T)

n.lm$n

## -----------------------------------------------------------------------------
n.lm$iterations

## -----------------------------------------------------------------------------
n.lm = nRegression(the.steps = the.steps, num.experiments = 10, the.formula = the.formula.lm, the.variable = the.variable, seed = seed, n.start = n.start, n.max = n.max, increment = increment, stop.threshold = 50, power = power, model.type = model.type, verbose = F)

n.lm$iterations

## -----------------------------------------------------------------------------
n.lm = nRegression(the.steps = the.steps, num.experiments = 10, the.formula = the.formula.lm, the.variable = the.variable, seed = seed, n.start = 650, n.min = 600, n.max = 700, increment = increment, stop.threshold = 1, power = power, model.type = model.type, verbose = F)

n.lm$n
n.lm$iterations

## -----------------------------------------------------------------------------
n.lm = nRegression(the.steps = the.steps, num.experiments = num.experiments, the.formula = the.formula.lm, the.variable = the.variable, seed = seed, n.start = n.start, n.max = 700, increment = increment, stop.threshold = stop.threshold, power = power, model.type = model.type, verbose = T)

n.lm$n
n.lm$power
n.lm$iterations

