#library(amss)
library("readxl")
require("data.table")

setwd("~/GitHub/MMM/Coffee Example")

price <- read_excel("Coffee_price.xlsx", col_types="numeric")

# specify the number of time intervals in the simulation
# 4 years of weekly data
n.years <- 5
time.n <- n.years * 52

# natural behavior of the consumer population in the absence of marketing interventions
# individuals from all activity states have a 60%/30%/10% chance of entering
# inactive/exploratory/purchase states at the beginning of each time interval
activity.transition <- matrix(
  c(0.60, 0.30, 0.10, # migration originating from inactive state
    0.60, 0.30, 0.10, # exploratory state
    0.60, 0.30, 0.10), # purchase state
  nrow = length(kActivityStates), byrow = TRUE)

# individuals have a 3%/7%/65%/20%/5% chance of transitioning to 
# unaware, negative, neutral, somewhat favorable, favorable brand favorability states
favorability.transition <- matrix(
  c(0.03, 0.07, 0.65, 0.20, 0.05, # migration from the unaware state
    0.03, 0.07, 0.65, 0.20, 0.05, # negative state
    0.03, 0.07, 0.65, 0.20, 0.05, # neutral state
    0.03, 0.07, 0.65, 0.20, 0.05, # somewhat favorable state
    0.03, 0.07, 0.65, 0.20, 0.05), # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)

# a sinusoidal pattern
market.rate.nonoise <-
  SimulateSinusoidal(n.years * 52, 52,
                     vert.trans = 0.6, amplitude = 0.25)
# with some added noise
market.rate.seas <- pmax(
  0, pmin(1,
          market.rate.nonoise *
            SimulateAR1(length(market.rate.nonoise), 1, 0.1, 0.3)))

# specification of the complete set of parameters
# controlling consumer mindset in the absence of marketing interventions
nat.mig.params <- list(
  population = 2.4e8,
#  population = 3.7e7,
  market.rate.trend = 0.68,
  market.rate.seas = market.rate.seas,
  # activity states for newly responsive (in-market & un-satiated)
  prop.activity = c(0.375, 0.425, 0.2),
  # brand favorability, initial proportions.
  prop.favorability = c(0.03, 0.07, 0.65, 0.20, 0.05),
  # everyone is a switcher
  prop.loyalty = c(1, 0, 0),
  transition.matrices = list(
    activity = activity.transition,
    favorability = favorability.transition))

# a common budget cycle is set for all media channels
# budgets are determined on a yearly basis, so each week within a budget
# period of 52 consecutive weeks is assigned the same identifier
budget.index <- rep(1:n.years, each = 52)

# a weekly flighting pattern is specified for TV
tv.flighting <-
  pmax(0,
       market.rate.seas +
         SimulateAR1(length(market.rate.seas), -0.7, 0.7, -0.7))
tv.flighting <- tv.flighting[c(6:length(tv.flighting), 1:5)]

# TV causes changes in consumer mindset by increasing brand awareness
# and brand favorability
tv.activity.trans.mat <- matrix(
  c(1.00, 0.00, 0.00, # migration originating from the inactive state
    0.00, 1.00, 0.00, # exploratory state
    0.00, 0.00, 1.00), # purchase state
  nrow = length(kActivityStates), byrow = TRUE)
tv.favorability.trans.mat <- matrix(
  c(0.4, 0.0, 0.4, 0.2, 0.0, # migration from the unaware state
    0.0, 0.9, 0.1, 0.0, 0.0, # negative state
    0.0, 0.0, 0.6, 0.4, 0.0, # neutral state
    0.0, 0.0, 0.0, 0.8, 0.2, # somewhat favorable state
    0.0, 0.0, 0.0, 0.0, 1.0), # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)

# Complete list of arguments for the traditional media module includes
# arguments controlling audience membership (i.e., tv viewership), the budget,
# the cost per exposure, and the relationship between ad frequency and efficacy
params.tv <- list(
  audience.membership = list(activity = rep(0.4, 3)),
  budget = rep(c(545e5, 475e5, 420e5, 455e5), length = n.years),
#  budget = rep(c(0, 0, 0, 0), length = n.years),
  budget.index = budget.index,
  flighting = tv.flighting,
  unit.cost = 0.005,
  hill.ec = 1.56,
  hill.slope = 1,
  transition.matrices = list(
    activity = tv.activity.trans.mat,
    favorability = tv.favorability.trans.mat))

# the minimum and maximum cost per click for the advertiser is set
cpc.min <- 0.8
cpc.max <- 1.1

# uncapped spend, shut off the first 2 of every 13 weeks
spend.cap.fn <- function(time.index, budget, budget.index) {
  if ((time.index %% 13) > 1) {
    return(Inf)
  } else {
    return(0)
  }
}

# the bid also does not depend on the budget and is fixed at 1.1 (maximum CPC)
bid.fn <- function(time.index, per.capita.budget, budget.index) {
  return(1.1)
}

# the relationship between budget and spend is driven by the following function
# It is written as a function of the per capita budget
# i.e., the budget divided by the size of the population
kwl.fn <- function(time.index, per.capita.budget, budget.index) {
  return(4.5 * per.capita.budget)
}

search.activity.trans.mat <- matrix(
  c(0.05, 0.95, 0.00, # starting state: inactive
    0.00, 0.85, 0.15, # starting state: exploratory
    0.00, 0.00, 1.00), # starting: purchase
  nrow = length(kActivityStates), byrow = TRUE)
search.favorability.trans.mat <- matrix(
  c(1.0, 0.0, 0.0, 0.0, 0.0, # unaware
    0.0, 1.0, 0.0, 0.0, 0.0, # negative
    0.0, 0.0, 1.0, 0.0, 0.0, # neutral
    0.0, 0.0, 0.0, 1.0, 0.0, # favorable
    0.0, 0.0, 0.0, 0.0, 1.0), # loyal
  nrow = length(kFavorabilityStates), byrow = TRUE)

# effect of paid search is limited to changes in activity state
params.search <- list(
  audience.membership = list(activity = c(0.01, 0.3, 0.4)),
  budget = (2.4e7 / n.years) * (1:n.years),
#  budget = (0 / n.years) * (1:n.years),
  budget.index = budget.index,
  spend.cap.fn = spend.cap.fn,
  bid.fn = bid.fn,
  kwl.fn = kwl.fn,
  query.rate = 1,
  cpc.min = cpc.min,
  cpc.max = cpc.max,
  ctr = list(activity = c(0.005, 0.08, 0.10)),
  relative.effectiveness = c(0, 0.1, 1),
  transition.matrices = list(
    activity = search.activity.trans.mat,
    favorability = search.favorability.trans.mat))

sales.params <- list(
  competitor.demand.max = list(loyalty = c(0.8, 0, 0.8)),
  advertiser.demand.slope = list(favorability = rep(0, 5)),
  advertiser.demand.intercept = list(
    favorability = c(0.014, 0, 0.2, 0.3, 0.9)),
  #price = 80)
  price = as.numeric(price[[1]]))

# simulate data

sim.data <- SimulateAMSS(
  time.n = time.n,
  nat.mig.params = nat.mig.params,
  media.names = c("tv", "search"),
  media.modules = c(
    `DefaultTraditionalMediaModule`,
    `DefaultSearchMediaModule`),
  media.params = list(params.tv, params.search),
  sales.params = sales.params)

# Observed Data
burn.in.length <- 52
final.year.end <- n.years * 52
final.year.start <- final.year.end - 51
observed.data <- sim.data$data[(burn.in.length + 1):final.year.end, ]

# full data
observed.data <- sim.data$data[1:final.year.end, ]
observed.data[,
              market.rate :=
                market.rate.seas[1:final.year.end]]

dirname(rstudioapi::getSourceEditorContext()$path)
fName = paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/MMM_data_amss_base_sales_full.csv')

write.csv(observed.data, file = fName)

# after burn-in
#observed.data[,
#              price :=
#                price$price[(burn.in.length + 1):final.year.end]]
observed.data[,
              market.rate :=
                market.rate.seas[(burn.in.length + 1):final.year.end]]

dirname(rstudioapi::getSourceEditorContext()$path)
fName = paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/MMM_data_amss.csv')

write.csv(observed.data, file = fName)

observed.data[,
              market.rate :=
                market.rate.seas[1:final.year.end]]

names(observed.data)

dirname(rstudioapi::getSourceEditorContext()$path)
fName = paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/MMM_data_amss_base_sales.csv')

write.csv(observed.data, file = fName)

# a scenario family with varying levels of lag
GetLagArgs <- function (
  time.n, nat.mig.params, params.tv, params.search, sales.params,
  lag.alpha = 0) {
  # add lag by mixing no-lag transition matrix w/ identity matrix
  nat.mig.params$transition.matrices$favorability <-
    (1 - lag.alpha) * nat.mig.params$transition.matrices$favorability +
    lag.alpha * diag(nrow(nat.mig.params$transition.matrices$favorability))
  # adjust initial media impact to keep total impact constant
  params.tv$transition.matrices$favorability <-
    (1 - lag.alpha) * params.tv$transition.matrices$favorability +
    lag.alpha * diag(nrow(params.tv$transition.matrices$favorability))
  return(list(
    time.n = time.n,
    nat.mig.params = nat.mig.params,
    media.names = c("tv", "search"),
    media.modules = c(
      `DefaultTraditionalMediaModule`,
      `DefaultSearchMediaModule`),
    media.params = list(params.tv, params.search),
    sales.params = sales.params))
}

args <- GetLagArgs(
  time.n = time.n,
  nat.mig.params = nat.mig.params,
  params.tv = params.tv,
  params.search = params.search,
  sales.params = sales.params,
  lag.alpha = 0.5)

# calculating ground truth - calculate ROAS
tv.roas <- CalculateROAS(
  sim.data,
  media.names = "tv",
  t.start = final.year.start,
  t.end = final.year.end,
  min.reps = 10,
  max.time = 30,
  verbose = TRUE)

tv.mroas <- CalculateROAS(
  sim.data,
  media.names = "tv",
  budget.proportion = 0.95,
  t.start = final.year.start,
  t.end = final.year.end,
  min.reps = 10,
  max.time = 30,
  verbose = TRUE)