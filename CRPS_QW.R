# Continuous Ranked Probability Score (CRPS) for simulated forecast distribuition 
crps <- function(y, sim_dens){
  J <- length(sim_dens)
  alpha <-  seq.int(0.5 * 1/J, 1 - 0.5 * 1/J, length.out = length(sim_dens))
  x <- sort(sim_dens)
  2 * mean(((y <  x) - alpha) * (x -  y))
}

# Quantile weighted CRPS (T. Gneiting and R. Ranjan (2011):
#                         Comparing Density Forecasts Using Threshold-and
#                         Quantile-Weighted Scoring Rules, JBES)
crps_qw <- function(y, sim_dens, w){
  J <- length(sim_dens)
  alpha <-  seq.int(0.5 * 1/J, 1 - 0.5 * 1/J, length.out = length(sim_dens))
  x <- sort(sim_dens)
   2 * mean(w(alpha) * ((y <  x) - alpha) * (x -  y))
}

# Weighting functions proposed by Gneiting and Ranjan 2011
## To emphasize the center
w_c <- function(alpha) alpha * (1-alpha)
alpha <- seq(0,1, 0.01)
plot(w_c(alpha), type = "l")

## To emphasize the tails
w_t <- function(alpha) (2 * alpha - 1)^2
plot(w_t(alpha), type = "l")

## To emphasize the right tail 
w_r <- function(alpha) alpha^2 
plot(w_r(alpha), type = "l")

## To emphasize the left tail
w_l <- function(alpha) (1-alpha)^2
plot(w_l(alpha), type = "l")

# Directly source script
#library(RCurl)
#script <- getURL("https://raw.githubusercontent.com/alexandergerber/CRPS_QW/master/CRPS_QW.R", ssl.verifypeer = FALSE)
#eval(parse(text = script))
