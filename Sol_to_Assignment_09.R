

###############################
###################
# Solution to Assignment 09
####################
##########################


# Forecasting using ARDL model using dLagM pckage, ARDL(2,1) model 

# 9.16

rm(list=ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

#install.packages("dLagM")
library(dLagM)   

?ardlDlm

rem.p = c(0)
rem.q = c(0)

model.ardl = ardlDlm(x = usmacro[,"g"],
                     y = usmacro[,"u"], p = 1 , q = 2, 
                     remove = list(p = rem.p , q = rem.q))  

#'The argument remove=list() is used to specify which lags of 
#' the series will be removed from the full model,
#' see help("ardlDlm") for more details.
#' Note that the role of p and q are changed in this package 

summary(model.ardl)

# Table 9.4, page 435 
fc <- dLagM::forecast(model = model.ardl, x = c(usmacro[,"g"][273],0.869,1.069), h = 3, interval = TRUE)
fc
# see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7034805/


# 9.20
rm(list = ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/okun5_aus.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/okun5_aus.rdata"))

# rename data
okun <- okun5_aus
names(okun)
head(okun)

U <- ts(okun$u, start=c(1978,2), freq=4)
G <- ts(okun$g, start=c(1978,2), freq=4)

model <- dynlm(d(U)~ G + L(G,1)+ L(G,2)+ L(G,3)+ L(G,4))
summary(model)

#Alternatively , and compactly 
model <- dynlm(d(U)~L(G,0:4))
summary(model)

# correlogram of the residuals from the model 
resd <- resid(model)
acf(resd)

library(forecast)
ggAcf(resd) +
  labs(title = "Correlogram for residuals for the model above")

# The correlogram shows that there is no significant 
#autocorrelation in the residuals from the model. 

# c)

library(lmtest)
library(sandwich)
# OLS estimates and White HCE standard errors
model.HAC <- coeftest(model, vcov=vcovHAC, type = c("HAC") )

confint(model)  ## Using OLS estimates and standard errors
confint(model.HAC) ## Using OLS estimates and HAC standard errors



# 12.3

rm(list = ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/unit.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/unit.rdata"))


# w x y z
# 
# Obs: 199  
# 
# w : AR process
# 
# x : AR process with trend
# 
# y : Random walk
# 
# z : Random walk with drift
# 
# Data Source:  generated series 
# 
# Variable |       Obs        Mean    Std. Dev.       Min        Max
# -------------+--------------------------------------------------------
#   w |       199    8.131558    2.601894   1.301199   14.48945
# x |       199    17.27226    6.647608   1.357299   28.48739
# y |       199    .7488905    3.401586  -7.431993   8.252406
# z |       199    18.55878    9.265142   .9120676   37.53984


w.ts <- ts(unit$w)
x.ts <- ts(unit$x)
y.ts <- ts(unit$y)
z.ts <- ts(unit$z)

# Quick look at of the time series plots
plot(w.ts)
plot(x.ts)
plot(y.ts)
plot(z.ts)

# a).

# Unit root test using the "urca" package 
library(urca)
#' helpful link: https://stats.stackexchange.com/questions/24072/interpreting-rs-ur-df-dickey-fuller-unit-root-test-results
??ur.df

summary(ur.df(w.ts, type = "drift", lags = 0,selectlags = c("AIC"))) 
summary(ur.df(y.ts, type = "drift", lags =0,selectlags = c("AIC"))) 

summary(ur.df(x.ts, type = "trend", lags = 0,selectlags = c("AIC"))) 
summary(ur.df(z.ts, type = "trend", lags = 0,selectlags = c("AIC"))) 

#' Notice here no augmentation terms have been included in all variables,
#' as the model is free of serial correlation. The maximum lag in each case is 
#' determined based on AIC criteria.


# b) Reject Ho if tau(t-value) <= t_Cv, 
# The t_CV =-2.86 at 5% level, for the variables w.ts and y.ts,whereas,  
# it is t_CV = -3.43 at 5% level for the variables x.ts and z.ts. Why?

# conclusion: w.ts is stationary, y.ts is non-stationary, x.ts is non-stationary
# and z.ts is non-stationary

# c) 
unit$t <- 1:dim(unit)[1]
head(unit)
fit <- lm(x ~ t + z, data = unit)
summary(fit)

# check whether the residuals from the model is stationary/non-stationary?
# If stationary, the variables x and z are cointegrated otherwise, they are not.
resd <- resid(fit)
plot(ts(resd))

summary(ur.df(resd, type = "none", lags = 0,selectlags = c("AIC"))) 
# conclusion: Reject Ho: unit-root/non-station ---> the variables are cointegrated.


# d) 
summary(ur.df(diff(z.ts), type = "drift", lags = 1,selectlags = c("AIC"))) 
# conclusion: while d(z.ts) is stationary. 
#Hence, the variable z or z.ts is stationary at first difference, so the order of integration is one.


# 12.14

rm(list = ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/gdp5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/gdp5.rdata"))

head(gdp5)
gdp <- ts(gdp5$gdp)
plot(gdp)

# Since gdp has a trend, we need to include a trend when testing unit root test.
summary(ur.df(gdp, type = "trend", lags = 1,selectlags = c("AIC"))) 

# Unit root at first difference 
summary(ur.df(diff(gdp), type = "none", lags = 0,selectlags = c("AIC")))
# gdp ~I(1)

head(gdp5)
tail(gdp5)

# ARIMA forecasts
gdp5 %>% select(gdp) %>% Arima(., order=c(2,0,0)) %>%  forecast(h=1)
# Plot the forecast value 
gdp5 %>% select(gdp) %>% Arima(., order=c(2,0,0)) %>%  forecast(h=20) %>% autoplot()

