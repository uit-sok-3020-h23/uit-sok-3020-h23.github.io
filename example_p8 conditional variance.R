#' ## Example P.8
#' ### Conditional Variance

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

#' The expected value, or mean of X, when Y=1 (grey), E(X|Y=1) is:
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod)) -> EXy1
EXy1$mean

#' E(X^2)
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod2=X^2*probs) %>% summarise(sum=sum(prod2)) -> EX2y1

#' The variance of X, when Y=1 (grey), Var(X|Y=1) is:
VarXy1 <- EX2y1$sum-EXy1$mean^2
VarXy1

#' The expected value, or mean of X, when Y=0 (white), E(X|Y=0) is:
pop %>% filter(Y==0) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod)) -> EXy0
EXy0$mean

#' E(X^2)
pop %>% filter(Y==0) %>% probspace() %>% mutate(prod2=X^2*probs) %>% summarise(sum=sum(prod2)) -> EX2y0

#' The variance of X, when Y=0 (grey), Var(X|Y=0) is:
VarXy0 <- EX2y0$sum-EXy0$mean^2
VarXy0

#' ... or
#' Find the conditional variance of X when Y=1
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod=X*probs,
                                                mean=sum(prod),
                                                dev2=(X-mean)^2,
                                                var=dev2*probs) %>% summarise(VarXy1=sum(var))

pop %>% filter(Y==0) %>% probspace() %>% mutate(prod=X*probs,
                                                mean=sum(prod),
                                                dev2=(X-mean)^2,
                                                var=dev2*probs) %>% summarise(VarXy0=sum(var))

pop %>% filter(X==1) %>% probspace() %>% mutate(prod=Y*probs,
                                                mean=sum(prod),
                                                dev2=(Y-mean)^2,
                                                var=dev2*probs) %>% summarise(VarYx1=sum(var))

pop %>% filter(X==2) %>% probspace() %>% mutate(prod=Y*probs,
                                                mean=sum(prod),
                                                dev2=(Y-mean)^2,
                                                var=dev2*probs) %>% summarise(VarYx2=sum(var))

pop %>% filter(X==3) %>% probspace() %>% mutate(prod=Y*probs,
                                                mean=sum(prod),
                                                dev2=(Y-mean)^2,
                                                var=dev2*probs) %>% summarise(VarYx3=sum(var))

pop %>% filter(X==4) %>% probspace() %>% mutate(prod=Y*probs,
                                                mean=sum(prod),
                                                dev2=(Y-mean)^2,
                                                var=dev2*probs) %>% summarise(VarYx4=sum(var))

