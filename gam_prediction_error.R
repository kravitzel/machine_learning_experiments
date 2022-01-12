# Libraries -------------------
library(tidymodels)
library(tidyverse)
library(mgcv)

# Function to simulated data ---------------------------------------------------
set.seed(20)

# Nonlinear function of x
m_of_x <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10

curve(m_of_x(x), 0 , 1)

# Simulates y
ysim <- function(n, scale = 2) {
  x <- runif(n)
  e <- rnorm(n, 0, scale)
  f <- m_of_x(x)
  y <- f + e
  tibble(y = y, x = x, f = f)
}

# Simulate one dataset ----------------------------------------------------------
n_sim = 1e3
n_samples = 200
sim_dfs = map(1:n_sim, ~ysim(n = n_samples))


