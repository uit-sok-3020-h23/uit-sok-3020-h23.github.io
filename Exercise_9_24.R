

# How to generate lags of a variable in R (i.e., x, x_t-1 is first lag of x)

#################################################
# In `base R`, lag(x,-1) - is the lag operator, 
#              lag(x)   - is a lead (t+1) operator.
########################################
#' Lead and Lag is used to shift one variable ahead or back in time 
#' so that the movements of two variables are more closely aligned 
#' if there is a time lag between a change in one variable and its 
#' impact on another.


#' Example 

x=ts(1:5)  # x is ts object 

# First lag of x, denoted x_1
x_1 <- stats::lag(x,-1)

# Second lag of x, denoted x_2
x_2 <- stats::lag(x,-2)

cbind(x,x_1,x_2)


###################
# Using tidyverse package, specifically using the dplyr
###################
dplyr::lag(x) # the variable must not be a time series object 

x <- 1:5

# First lag of x, denoted x_1
x_1 <- dplyr::lag(x,1)

# Second lag of x, denoted x_2
x_2 <- dplyr::lag(x,2)

data <- cbind(x,x_1,x_2)
data
# remove the first two rows 
data[-1:-2,]


###########################
# Exercise 9.24
#############################
rm(list = ls())
library(tidyverse)
library(dynlm)

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cons_inc.rdata"))
head(cons_inc)
tail(cons_inc)

# If you want to create new time series variables, 
# and want to use the mutate fun from tidyverse. 

mydata<- cons_inc %>% 
  mutate(dy = y-dplyr::lag(y),
         dc= cons-dplyr::lag(cons),
         
         dy1 = dplyr::lag(dy,1), # first lag of dy 
         dy2 = dplyr::lag(dy,2), # second lag of dy 
         dy3 = dplyr::lag(dy,3), #  third lag of dy 
         dy4 = dplyr::lag(dy,4)) # fourth lag of dy 

head(mydata)

# remove the first 4 observations 

mydata <- mydata[-1:-5,]


fit_1 <- lm(dc ~ dy +dy1 +dy2 +dy3 +dy4, data = mydata)
summary(fit_1)


#####################################
# Directly without generating the lag variables 
########################################

head(cons_inc)

cons <- ts(cons_inc$cons, start = c(1959,3), frequency = 4)
y <- ts(cons_inc$y, start = c(1959,3), frequency = 4)

fit_2 <- dynlm(d(cons) ~ L(d(y),0:4))

summary(fit_2)
summary(fit_1)



# check autocorrelation in the estimated model

require(lmtest)
#LM test
# H0: No autocorrelation
bgtest(fit_1,1)
bgtest(fit_1, order=2)
bgtest(fit_1, order=3)

# Alternatively, the Durbin-Watson test
dwtest(fit_1)

# conclusion: there is a significant autocorrelation/serial correlation in the estimated model 


##########################
# Solution to correct autocorrelation: USE HAC
###########################
require(sandwich)
# OLS estimates and White HCE standard errors
coeftest(fit_1, vcov=vcovHAC, type = c("HAC") )

# OLS estimates and standard errors
coeftest(fit_1)



###########################
# Exercise 9.25
#############################
# Estimate each model and check if there is 
# autocorrelation problem. 

model_1 <- dynlm(d(cons) ~ L(d(cons),1) +L(d(y)))
model_2 <- dynlm(d(cons) ~ d(y) +L(d(y),3))

summary(model_1)
summary(model_2)


