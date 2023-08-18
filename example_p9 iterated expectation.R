#' ## Example P.9
#' ### Iterated Expectation

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

pop <- as_tibble(cbind(X,Y))
pop

#' Previously
#' E(X|Y=0)
pop %>% filter(Y==0) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

#' E(X|Y=1)
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

#' Also
pop %>% filter(X==1) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==2) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==3) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==4) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))

#' Find the marginal pdf of X
probX <- pop %>% probspace() %>% marginal(vars = "X")
probX

#' Iterated Expectation
EYx1 <- pop %>% filter(X==1) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx2 <- pop %>% filter(X==2) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx3 <- pop %>% filter(X==3) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx4 <- pop %>% filter(X==4) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))

fx1 <- probX$probs[1]
fx2 <- probX$probs[2]
fx3 <- probX$probs[3]
fx4 <- probX$probs[4]

EYx1$mean*fx1+EYx2$mean*fx2+EYx3$mean*fx3+EYx4$mean*fx4

