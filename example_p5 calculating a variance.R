#' ## Example P.5
#' ### Calculating a Variance
 
rm(list=ls())

library(tidyverse)

#' Package ‘prob’ is removed from the CRAN repository.
#browseURL("https://cran.r-project.org/web/packages/prob/index.html")

#' You can install it from the archive. However, if you run the following code:
source("https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/remnants_prob.R")
#' This sources the functions we need in this section.

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- probspace(cbind(X,Y))
pop

#' Find the pdf of X
probX <- marginal(pop, vars = "X")
probX

#' The expected value, or mean of X, E(X) is:
EX <- probX %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))
EX

#' E(X^2)
EX2 <- probX %>% mutate(prod2=X^2*probs) %>% summarise(sum=sum(prod2))
EX2

#' The variance of X
EX2$sum-EX$mean^2

#' ... or
probX %>% mutate(prod=X*probs) %>% mutate(mean=sum(prod),
                                          dev2=(X-mean)^2,
                                          var=dev2*probs) %>% summarise(VarX=sum(var))
