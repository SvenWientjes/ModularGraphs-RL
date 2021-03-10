############################################################################################################################
######################################### Script for testing splines in Stan ###############################################
############################################################################################################################
library(rstan)
library(splines)
library(ggplot2)
library(data.table)
library(bridgesampling)

num_knots <- 4
spline_degree <- 3
num_basis <- num_knots + spline_degree - 1
X <- sample(seq(from=1, to=15, by=1), replace=T, size=1000)
num_data <- length(X)
knots <- unname(quantile(X,probs=seq(from=0, to=1, length.out = num_knots)))
a0 <- 0.2
a <- rnorm(num_basis, 0, 1)
B_true <- t(bs(X, df=num_basis, degree=spline_degree, intercept = TRUE))
#Y_true <- as.vector(a0*X + a%*%B_true)
Y_true <- rep(-2,num_data); Y_true[X>1] <- 2
Y <- Y_true + rnorm(length(X), 0, 0.2)

splinedat <- list(
  num_data = num_data,
  num_knots = num_knots,
  knots = knots,
  spline_degree = spline_degree,
  Y = Y,
  X = X
)

splinefit <- stan(
  file = "src/Stan/splinetest.stan",
  data = splinedat,
  chains = 4,
  warmup = 500,
  iter = 1000,
  cores = 2,
  verbose = T
)

spsum <- as.data.table(summary(splinefit)$summary, keep.rownames = T)
plot(X, spsum[grepl('Y_hat',rn, fixed=T),mean])
plot(X, Y_true)

bridge_sampler(splinefit)



