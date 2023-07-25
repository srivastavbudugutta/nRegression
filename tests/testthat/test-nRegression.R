describe("nRegression function tests",{

library(nRegression)

## variables
power = 0.9
step.age <- "Age ~ N(45, 10)"
step.female <- "Female ~ binary(0.53)"
step.health.percentile <- "Health.Percentile ~ U(0,100)"
step.exercise.sessions <- "Exercise.Sessions ~ Poisson(2)"
step.diet <- "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))"
step.healthy.lifestyle <- "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"
step.weight <- "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))"
the.steps <- c(step.age, step.female, step.health.percentile, step.exercise.sessions, step.diet, step.healthy.lifestyle, step.weight)
the.variable = "Exercise.Sessions"
conf.level = 0.95

seed = 41
vstr = 3.6
num.experiments = 200
n.start = 200
n.min = 1
n.max = 3000
increment = 100
stop.threshold = 1

#1 Simulations for Logistic Regression

the.formula.logistic <- Healthy.Lifestyle ~ Age + Female + Health.Percentile + Exercise.Sessions + Weight
model.type = "logistic"


n.logistic = nRegression(the.steps = the.steps, num.experiments = num.experiments, the.formula = the.formula.logistic, the.variable = the.variable, seed = seed, n.start = n.start, n.min = n.min, n.max = n.max, increment = increment, stop.threshold = stop.threshold, power = power, model.type = model.type, verbose = T)

test_that("nRegression (Logistic Regression) returns a list of length 4", {
  expect_length(n.logistic, 4)
})

test_that("nRegression (Logistic Regression) returns expected names", {
  expect_named(n.logistic, c("n", "power", "iterations", "simstudy"))
})

test_that("nRegression (Logistic Regression) returns expected n value", {
  expect_equal(n.logistic$n, 141)
})

test_that("nRegression (Logistic Regression) returns expected power value", {
  expect_equal(n.logistic$power, 0.93)
})


test_that("nRegression (Logistic Regression) returns expected simstudy names", {
  expect_named(n.logistic$simstudy, c("the.steps", "simdat", "statistics", "sim.analysis"))
})

test_that("nRegression (Logistic Regression) returns expected iterations", {
  expected_iterations <- data.frame(
    i = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    lower = c(1, 1, 101, 101, 126, 139, 139, 139, 139),
    n = c(200, 101, 151, 126, 139, 145, 142, 141, 140),
    upper = c(3000, 200, 200, 151, 151, 151, 145, 142, 141),
    power = c(0.965, 0.840, 0.900, 0.895, 0.880, 0.900, 0.930, 0.930, 0.885)
  )

  expect_equal(as.data.frame(n.logistic$iterations), expected_iterations)
})

#2 Simulations for Linear Regression

## Variables

the.formula.lm <- Weight ~ Age + Female + Health.Percentile + Exercise.Sessions + Healthy.Lifestyle
model.type = "lm"
num.experiments <- 500

n.start = 500
n.max <- 10000
increment = 500
stop.threshold = 10

the.variable = "Healthy.LifestyleTRUE"

n.lm = nRegression(the.steps = the.steps, num.experiments = num.experiments, the.formula = the.formula.lm, the.variable = the.variable, seed = seed, n.start = n.start, n.max = n.max, increment = increment, stop.threshold = stop.threshold, power = power, model.type = model.type, verbose = T)


test_that("nRegression (Linear Regression) returns a list of length 4", {
  expect_length(n.lm, 4)
})

test_that("nRegression (Linear Regression) returns expected names", {
  expect_named(n.lm, c("n", "power", "iterations", "simstudy"))
})

test_that("nRegression (Linear Regression) returns expected n value", {
  expect_equal(n.lm$n, 860)
})

test_that("nRegression (Linear Regression) returns expected power value", {
  expect_equal(n.lm$power, 0.904)
})


test_that("nRegression (Linear Regression) returns expected simstudy names", {
  expect_named(n.lm$simstudy, c("the.steps", "simdat", "statistics", "sim.analysis"))
})

test_that("nRegression (Linear Regression) returns expected iterations", {
  expected_iterations <- data.frame(
    i = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    lower = c(1, 500, 500, 750, 750, 813, 844, 844, 852),
    n = c(500, 1000, 750, 875, 813, 844, 860, 852, 856),
    upper = c(10000 , 10000 , 1000, 1000, 875, 875, 875, 860, 860),
    power = c(0.634, 0.940, 0.856, 0.912, 0.858, 0.896, 0.904, 0.890, 0.884)
  )

  expect_equal(as.data.frame(n.lm$iterations), expected_iterations)
})


})
