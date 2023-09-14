rm(list=ls())

library(mosaic)

#' Data set on food expenditure and weekly income from a random sample of 40 households.
#' Data definition file: <http://www.principlesofeconometrics.com/poe5/data/def/food.def>
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

head(food)
str(food)

#' Estimating the parameters of the simple linear regression model
fit <- lm(food_exp~income, data=food)
summary(fit)

# Variance covariance matrix
vcov(fit)

# standard errors
sqrt(diag(vcov(fit)))


# -----------------------------------------------------
# A digression, but repeat formulas so far

x <- food$income
y <- food$food_exp

b2 <- cov(x,y)/var(x)
b1 <- mean(y)-b2*mean(x)

print(c(b1,b2))
coef(fit)

y_hat1 <- b1+b2*x
y_hat2 <- predict(fit)

all.equal(y_hat1, y_hat2, tolerance = 0.0000001, check.names = FALSE)

e1 <- y-y_hat1
e2 <- resid(fit)

all.equal(e1, e2, tolerance = 0.0000001, check.names = FALSE)

sum(e1^2) # SSE
deviance(fit) # SSE

N <- length(y)

var_fit <- sum(e1^2)/(N-2)
var_fit

se_fit <- sqrt(var_fit)
se_fit

summary(fit)

# Finding the variance covariance matrix
# Using matrix algebra instead of the formulas 2.14, 2.15 and 2.16, p. 70
X <- model.matrix(fit) # The X matrix, ie, explanatory data matrix, including "(Intercept)"
solve(t(X) %*% X) %*% t(X) %*% y  # X'X^-1 X'y

var_fit * solve(t(X) %*% X) # variance * X'X^-1
vcov(fit)

SST <- sum((y-mean(y))^2)
SSR <- sum((y_hat1-mean(y))^2)
SSE <- sum((y-y_hat1)^2) 

all.equal(SST, SSR+SSE, tolerance = 0.0000001, check.names = FALSE)
anova(fit)

# End of digression
# -----------------------------------------------------

# Example 3.1
confint(fit)
confint(fit, level = 0.9) # change to a 90% CI
qt(0.975, fit$df.residual) # critical t, 1-0.05/2, N-2 df

# manual 95% CI
coef(fit)[2]+qt(0.975,38)*sqrt(diag(vcov(fit)))[2]
coef(fit)[2]-qt(0.975,38)*sqrt(diag(vcov(fit)))[2]

#' Repeated sampling (mosaic::do) or bootstrapping
# https://cran.r-project.org/web/packages/mosaic/vignettes/Resampling.pdf
# Bootstrapping involves sampling with replacement from a population,
# repeatedly calculating a sample statistic of
# interest to empirically construct the sampling distribution.
head(resample(food))
head(do(2)*resample(food))
tail(do(2)*resample(food))

resample(food)
do(10)*lm(food_exp~income, data=resample(food))
do(10)*confint(lm(food_exp~income, data=resample(food)))

# Table 3.1
test <- do(10)*confint(lm(food_exp~income, data=resample(food)))
test %>% filter(.row==1) # intercept
test %>% filter(.row==2) # slope

m1.bootstrap <- do(10) * lm(food_exp ~ income, data=resample(food))
confint(m1.bootstrap)

#' Standard normal pdf
xpnorm(c(-1.96,1.96), mean=0, sd=1)

# dnorm() gives the "height" of the pdf
dnorm(-1.96)
# a plot of the standard normal distribution
curve(dnorm(x),-6,6, main=(expression(Z%~%N(0,1))))
abline(v=-1.96, col="red")
abline(h=dnorm(-1.96), col="blue")

#pnorm gives the probability and is the cumulative of the pdf, also called the cdf
curve(pnorm(x), -3,3)
pnorm(1.96)
abline(v=1.96, col="red")
abline(h=pnorm(1.96), col="blue")

#area to the right
1-pnorm(1.96)
#area to the left
pnorm(-1.96)

#adding them up
1-pnorm(1.96)+pnorm(-1.96)

#' Comparing the normal and t-distribution with 3 df
x  <- seq(-3, 3, 0.05)
y1 <- dnorm(x)
y2 <- dt(x,df=3)
df <- data.frame(x,y1,y2)

ggplot(df, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y1), colour="red") +  # first layer Normal distr
  geom_line(aes(y=y2), colour="blue")  # second layer t distr

#' Critical t-values, p. 116
qt(c(0.025, 0.975), df=38) 
qdist("t", c(.025, .975) , df=38) # plot

### Continue
#A 95% confidence interval is calculated ad follows:
confint(fit, level = 0.95)

# Various plots of the parameters and their CI
library(pacman)
p_load(coefplot)
coefplot(fit)
coefplot(fit, predictors="income") # remove Intercept
mplot(fit, which=7, rows=-1, xlim=c(-1,15)) 

#' Repeated Sample - A Computer Simulation 
p_load(TeachingDemos)
set.seed(219)
ci.examp(mean.sim=10.21, sd=2.09, n=38, reps=100, conf.level = 0.95, method = "t")

p_load(car, multcomp)  # install car and multcomp packages 

# install the package "FSAmisc" 
# To download "FSAmisc" see the link:  https://rdrr.io/github/droglenc/FSAmisc/ 
#install.packages("remotes")
#library(remotes)
#remotes::install_github("droglenc/FSAmisc")
library(FSAmisc)

# Example 3.2, p.123, here we specify a one-tail right-test, H0: b2=0, H1: b2>0
# t-critical (tc) alpha=0.05 
qt(0.95, 38)
hoCoef(fit, term = 2, bo = 0, alt = c("greater")) # FSA::hoCoef, one sided test, specify the H1 using alt=
summary(glht(fit, linfct = c("income <= 0"))) # multcomp::glht, one sided test,  specify the H0 using linfct=
# calculated t > tc, reject H0
summary(glht(fit, linfct = c("income <= 0")))$test$tstat > qt(0.95, 38)

# Example 3.3, p. 124, here we specify a one-tail test, H0: b2<=5.5, H1: b2>5.5
# t-critical (tc) alpha=0.01 
qt(0.99, 38)
hoCoef(fit, term = 2, bo = 5.5, alt = c("greater")) # FSA::hoCoef, one sided test
summary(glht(fit, linfct = c("income <= 5.5"))) # multcomp::glht, one sided test
# calculated t not larger than critical, keep H0
summary(glht(fit, linfct = c("income <= 5.5")))$test$tstat > qt(0.99, 38)

# Example 3.4, p.125, here we specify a one-tail test, H0: b2>=15, H1: b2<15
# t-critical (tc) alpha=0.05 
qt(0.05, 38) # note that since this is a left tail test, how we change the quantile to the left of the distribution
hoCoef(fit, term = 2, bo = 15, alt = c("less")) # FSA::hoCoef, one sided test
summary(glht(fit, linfct = c("income >= 15"))) # multcomp::glht, one sided test, H0
# we reject H0
summary(glht(fit, linfct = c("income >= 15")))$test$tstat < qt(0.05, 38)

# Example 3.5, p.125, two-tail test, here we specify a two-tail test, H0: b2=7.5, H1: b2!=7.5
# t-critical (tc) alpha/2=0.05/2
qt(c(0.025,0.975), 38)
linearHypothesis(fit, "income = 7.5") # gives an F-value
hoCoef(fit, term = 2, bo = 7.5, alt = c("two.sided"))
summary(glht(fit, linfct = c("income = 7.5")))
# we do not reject H0
summary(glht(fit, linfct = c("income = 7.5")))$test$tstat > abs(qt(c(0.025,0.975), 38))

# Example 3.6
summary(fit)

# The 100(1-alpha)% confidence interval, any value of H0: b2=c inside this interval, we keep H0.
confint(fit, level = 0.95)

# Example 3.2, p. 123, one-tail test revisited, by looking at the p-value for a two tail test.
summary(glht(fit, linfct = c("income <= 0")))
linearHypothesis(fit, "income = 0") # two-tail test
qt(0.95, 38) # critical t-valule
#To get the one-tail p-value, we take the two-tail p-value and divide it with 2.
h <- linearHypothesis(fit, "income = 0")
str(h)
# t-value, with one df, the t^2 = F
sqrt(h$F)
# one-tail p-value
h$Pr/2
# or
1-pt(sqrt(h$F), df=h$Res.Df[2])

# Example 3.6, default two-tail test of significance
# t-critical (tc) alpha/2=0.05/2
qt(c(0.025,0.975), 38)
summary(fit)

##############################################
# The p-value rule:
# if p-value <= alpha (level of significance) reject H0
# often used, more often misused
##############################################

# Predict Expected Values from the regression model
#' Prediction at income=20
# Base R
predict.lm(fit,newdata = data.frame(income=20))

f <- makeFun(fit)
f(income=20)
f(20)
# Example 3.8, p. 131
f(20, interval="confidence")
f(20, interval="confidence", level=0.9)

p_load(rockchalk)

predictOMatic(fit)
predictOMatic(fit, predVals = list(income=20))
predictOMatic(fit, predVals = list(income=20), interval="confidence")
predictOMatic(fit, predVals = list(income=20), interval="prediction")

plotSlopes(fit, plotx = "income", interval = "conf")
plotSlopes(fit, plotx = "income", interval = "conf", opacity = 80, col="red")

# The delta method "car" package
# Example 3.9, Linear combination of two parameters, p. 132
# E(food_exp | income=20) = b1+b2*20 
# H0: b1+b2*20 <= 250, H1: b1+b2*20 > 250 
# Rewrite: H0: b1+b2*20 -250 <= 0, H1: b1+b2*20 -250 > 0 

# Use deltaMethod in car package
deltaMethod(fit, "Intercept+20*income-250") 

# more convenient than names, use parameter names
deltaMethod(fit, "b1+20*b2-250", parameterNames= paste("b", 1:2, sep="")) 

dmt <- deltaMethod(fit, "b1+20*b2-250", parameterNames= paste("b", 1:2, sep="")) 
# t-value
dmt$Estimate/dmt$SE
# critical t, alphe=0.05
qt(0.95, 38)

# reject H0
dmt$Estimate/dmt$SE > qt(0.95, 38)

# Trick to use glht, with hypothesis involving (Intercept), create own intercept, call it "one"
head(model.matrix(fit))
food <- food %>% mutate(one=1)
head(food)

# remove default (Intercept) with 0, replace it with one
fit2 <- lm(food_exp~0+one+income, data=food)
summary(fit) # original model
summary(fit2) # identical, but (Intercept) replaced with variable the variable we called one

head(model.matrix(fit2))

# now we can use glht, one sided test, specify H0
summary(glht(fit2, linfct = c("one+20*income <= 250")))
summary(glht(fit, linfct = c("(Intercept)+20*income <= 250"))) # Alternatively write out (Intercept)

# is by default a two-tail test, divide p-value/2 to get 1-tail p-value
linearHypothesis(fit, "(Intercept)+20*income -250 = 0")

lht <- linearHypothesis(fit, "(Intercept)+20*income -250 = 0")
lht$`Pr(>F)`[2]/2
