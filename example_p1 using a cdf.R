#' ## Example P.1
#' ### Using a cdf

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

#' The parts of the pdf (probability density function)
Prob(pop, X==1)
Prob(pop, X==2)
Prob(pop, X==3)
Prob(pop, X==4)

#' The sum og all the probabilities has to be 1
Prob(pop)

pop %>% ggplot(aes(x=X, y=probs)) + geom_bar(stat = "identity") + 
  ggtitle("Figure P.1 Probability density function for X (value on tie)") +
  xlab("X value") + ylab("Probability")

#' F(2) = P(X <= 2)
Prob(pop, X==1) + Prob(pop, X==2)

#' P(X > 2)
1 - (Prob(pop, X==1) + Prob(pop, X==2))

#' This is tedious if we have many values.
#' We find the marginal pdf of X
marginal(pop, vars = "X")

#' Could also use 
pop %>% group_by(X) %>% summarise(probs=sum(probs))
pdfX <- pop %>% group_by(X) %>% summarise(probs=sum(probs))
pdfX

#' F(2) = P(X <= 2)
pdfX %>% filter(X <= 2) %>% summarise(prob=sum(probs))

#' P(X > 2)
pdfX %>% filter(X > 2) %>% summarise(prob=sum(probs))

#' Cumulative probability of X
pdfX %>% mutate(cumprob=cumsum(probs))

#' P(X <= x)
pdfX %>% mutate(cumprob=cumsum(probs)) %>% ggplot(aes(x=X, y=cumprob)) + geom_bar(stat = "identity") + 
  ggtitle("Figure P.1a Cumulative probability density function for X (value on tie)") +
  xlab("X value") + ylab("Probability")

