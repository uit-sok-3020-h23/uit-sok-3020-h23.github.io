#' ## Example P.6
#' ### Calculating a Correlation

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

#' Finding E(XY)
pop %>% mutate(prod=X*Y*probs) %>% summarise(sum=sum(prod)) -> EXY
EXY

#' Find the pdf of X
probX <- marginal(pop, vars = "X")
probX

#' The expected value, or mean of X, E(X) is:
probX %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod)) -> EX
EX

#' Find the pdf of Y
probY <- marginal(pop, vars = "Y")
probY

#' The expected value, or mean of Y, E(Y) is:
probY %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod)) -> EY
EY

#' E(X^2)
probX %>% mutate(prod2=X^2*probs) %>% summarise(sum=sum(prod2)) -> EX2

#' The variance of X
VarX <- EX2$sum-EX$mean^2
VarX

#' E(Y^2)
probY %>% mutate(prod2=Y^2*probs) %>% summarise(sum=sum(prod2)) -> EY2
EY2

#' The variance of Y
VarY <- EY2$sum-EY$mean^2
VarY

#' The correlation
(EXY$sum-EX$mean*EY$mean)/(sqrt(VarX)*sqrt(VarY))

#' ... or
pop %>% mutate(devx=(X-EX$mean)/sqrt(VarX),
               devy=(Y-EY$mean)/sqrt(VarY),
               prod=devx*devy*probs) %>% summarise(corr=sum(prod))
