#' ## Example P.11
#' ### Normal Distribution Probability Calculation

rm(list=ls())

library(mosaic)

#' Base
curve(dnorm(x), -3,3)

#' Mosaic
plotDist("norm") # mean=0, sd=1
plotDist("norm", mean=2, sd=1)
plotDist("norm", mean=0, sd=4)

#' Use ggplot
ggplot(data = data.frame(x = c(-4, 6)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), colour = "red") + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 2), colour = "blue", lty=4) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 2, sd = 1), colour = "black", lty=2) +
  ylab("f(x)") +
  ggtitle(expression(paste("Figure P.5 Normal probability density functions ", N(mu,sigma^{2})))) +
  geom_text(x = -1.2, y=0.3, label="N(0,1)", color="red") +
  geom_text(x = -3, y = 0.1, label="N(0,4)", color="blue") +
  geom_text(x = 3.2, y = 0.3, label="N(2,1)", color="black") +
  theme_bw()

#' If X~N(3,9), then P(4 <= X <= 6)
xpnorm(c(4,6), mean = 3, sd = sqrt(9))

#' Base
pnorm(6, mean = 3, sd = sqrt(9))  
pnorm(4, mean = 3, sd = sqrt(9))

pnorm(6, mean = 3, sd = sqrt(9)) - pnorm(4, mean = 3, sd = sqrt(9))

#' Standard normal percentiles, Table P.7
qnorm(0.975)
qnorm(0.025)

xpnorm(qnorm(0.975), mean = 0, sd = 1)
xpnorm(qnorm(0.025), mean = 0, sd = 1)

#' ...or
xqnorm(c(0.025, 0.975), mean = 0, sd = 1)

#' The Bivariate Normal Distribution
library(MASS)

#' Set parameters
#' Let; $\mu_x=\mu_y=5,\; \sigma_x=\sigma_y=3,\;\rho=0.7$.
mu_x <- 5
mu_y <- 5
sigma_x <- 3
sigma_y <- 3
rho <- 0.7

#' Generate bivariate normal data
set.seed(123)  # for reproducibility
n <- 10000
samples <- mvrnorm(n, mu = c(mu_x, mu_y), 
                   Sigma = matrix(c(sigma_x^2, rho*sigma_x*sigma_y, 
                                    rho*sigma_x*sigma_y, sigma_y^2), 2))
data <- as.data.frame(samples)
colnames(data) <- c("X", "Y")

#' 2D density plot
ggplot(data, aes(X, Y)) + 
  geom_point(aes(X, Y), alpha = 0.2, size = 0.5) + 
  stat_density_2d(aes(X, Y, fill = after_stat(level)), geom = "polygon") +
  scale_fill_viridis_c() + 
  theme_minimal() + 
  labs(title = "Bivariate Normal Distribution")


#' The regression function, $E(Y|X)=\alpha+\beta X$
slope <- 0.7*3*3/3^2
intercept <- 5-0.7*5

#' A plot of the bivariate normal distribution with the regression line.
ggplot(data, aes(X, Y)) + 
  geom_point(aes(X, Y), alpha = 0.2, size = 0.5) + 
  stat_density_2d(aes(X, Y, fill = after_stat(level)), geom = "polygon") +
  geom_abline(intercept = intercept, slope = slope, color = "red", size = 1.2) +  # Adding the regression line
  scale_fill_viridis_c() + 
  theme_minimal() + 
  labs(title = "Bivariate Normal Distribution with Regression Line")


