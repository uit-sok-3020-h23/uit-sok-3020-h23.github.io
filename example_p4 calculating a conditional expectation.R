#' ## Example P.4
#' ### Calculating a Conditional Expectation

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

#' Find the conditional pdf of X when Y=1 (grey)
probX <- pop %>% filter(Y==1) %>% probspace()
probX 

#' Find the expected value of X (value), given that Y=1 (grey)?
#' E(X|Y=1)
probX %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

