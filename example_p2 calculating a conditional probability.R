#' ## Example P.2
#' ### Calculating a conditional probability

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

#' Joint and Marginal probabilities, Table P.4
marginal(pop, vars = "X")
marginal(pop, vars = "Y")
marginal(pop, vars = c("X", "Y"))

#' What is the probability of drawing a random slip with the value X=2, given that it is grey, Y=1?
#' P(X=2|Y=1)
A <- subset(pop, X == 2)
B <- subset(pop, Y == 1)

Prob(A)
Prob(B)

#' P(A and B)
intersect(A,B)
Prob(intersect(A,B))

#' P(A|B)=Prob(A and B)/ P(B)
Prob(A, given = B)

#' ... or long way
Prob(intersect(A,B))/Prob(B)

