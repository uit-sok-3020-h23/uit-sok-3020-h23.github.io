#' Appendix C
#' Remember to restart your R session

rm(list=ls())

#' Note that the Hip data (y) is now in a data frame, p. 814
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/hip.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/hip.rdata"))

head(hip)
str(hip)

library(mosaic)

#' Example C.1
histogram(~y, data=hip) # p. 814
gf_histogram(~y, data = hip)
hip %>% ggplot(aes(y)) + geom_histogram(bins = 10, alpha=0.7)

#' Example C. 2
#' Some descriptive statistics
#' Various ways of calculating the mean
mean(~y, data=hip)
mean(hip[,1]) # column
mean(hip[,"y"]) # column name
mean(hip$y) # variable/vector
favstats(~y, data=hip)

#' Example C.3 Sampling variation
#' draw 10 obs from hip, with replacement
set.seed(1234)
sample(hip, size = 10)
resample(hip, size = 10) # same functionality

#' one sample, n=10, then calculate mean of it
sample(hip, size = 10) %>% summarise(mean=mean(y))

#' two samples, n=10, note .index indicates sample
do(2)*sample(hip, size = 10, replace=TRUE)

#' ten samples, n=10, store in objects, then calculate mean of it
datasample <- do(10)*sample(hip, size = 10, replace=TRUE)
mean(~y | .index, data=datasample)
#' or
datasample %>% group_by(.index) %>% summarise(mean=mean(y))
#' or 
do(10)*mean(~y, data=resample(hip, size=10))

#' The sampling distribution of the mean
#' Replicate Figure C.2, with 3 sample sizes, n=10,20 and 50
sample.10 <- do(3000)*mean(~y, data=resample(hip, size=10))
sample.20 <- do(3000)*mean(~y, data=resample(hip, size=20))
sample.50 <- do(3000)*mean(~y, data=resample(hip)) # n=50, use all

#' population mean
mean(~y, data=hip)
mean(~mean, data=sample.10)
mean(~mean, data=sample.20)
mean(~mean, data=sample.50)

#' add sample size
sample.10$size <- 10
sample.20$size <- 20
sample.50$size <- 50

dframe <- bind_rows(sample.10, sample.20, sample.50)

#' Figure C.2, bootstrapped, empirical distributionn
#' Larger sample size decrease the sampling distribution of the mean
gf_dens(~mean, color=~as.factor(size), lwd=2, data=dframe)


#' Example C.4
#' The effect of sample size on sample mean precision

#' calculations, variance=10, N=40
1/sqrt(0.25)
2*pnorm(2)-1 # probability

#' Let's make a plot of it
N <- 1:80
dta <- as.data.frame(cbind(N, 1/sqrt(10/N), 2*pnorm(1/sqrt(10/N))-1))
names(dta)[2:3] <- c("Z", "Prob")
head(dta)

xyplot(Prob~N, data = dta, main="The probability that the true mean is within 1 sd as a function of N")


#' Example C.5
#' The Central Limit Theorem
require(TeachingDemos) || {install.packages("TeachingDemos"); require(TeachingDemos)}
#' We look at 4 different distributions, similar to figure C. 3, p. 819 
#' we vary, n, using: 1, 2, 6 and 30 as sample size.
clt.examp(n = 1, reps = 10000, nclass =16)
clt.examp(n = 2, reps = 10000, nclass =16)
clt.examp(n = 6, reps = 10000, nclass =16)
clt.examp(n = 30, reps = 10000, nclass =16)


#' Do it manually, for a uniform distribution.
#' n=1, runif() draws values from the uniform distribution
xbar <- numeric(10000)
for (i in 1:10000){x=runif(1);xbar[i]=mean(x)}
hist(xbar,col="orange",main="n=1")
gf_dens(~xbar)
mean(xbar)
sd(xbar)
#' n=3
xbar2=numeric(10000)
for (i in 1:10000){x=runif(3);xbar2[i]=mean(x)}
hist(xbar2,col="red",main="n=3")
gf_dens(~xbar2)
mean(xbar2)
sd(xbar2)
#' n=30
xbar3=numeric(10000)
for (i in 1:10000){x=runif(30);xbar3[i]=mean(x)}
hist(xbar3,col="green",main="n=30")
gf_dens(~xbar3)
mean(xbar3)
sd(xbar3)

#' Example C.6

#' sample variance
var(hip$y)

#' estimated variance of the sample mean
var(hip$y)/length(hip$y)

#' sample st.dev
sd(hip$y)

#' estimated st.error of the sample mean
sd(hip$y)/sqrt(length(hip$y))

library(fBasics)

#' Skewness is a measure of symmetry, or more precisely, the lack of symmetry
skewness(hip)

#' A symmetrical distribution has a skewness of zero.
#' An asymmetrical distribution with a long tail to the right (higher values) has a positive skew.
#' An asymmetrical distribution with a long tail to the left (lower values) has a negative skew.
#' The skewness is unitless.

#' Kurtosis is a measure of whether the data are peaked or flat relative to a normal distribution
kurtosis(hip)

#' Kurtosis quantifies whether the shape of the data distribution matches the Gaussian distribution.
#' A Gaussian distribution has a kurtosis of 0.
#' A flatter distribution has a negative kurtosis.
#' A distribution more peaked than a Gaussian distribution has a positive kurtosis.
#' Kurtosis has no units.


#' The normal distribution
curve(dnorm(x),-3,3, main="The Standard Normal Distribution") 
curve(pnorm(x),-3,3, main="The Cumulative Standard Normal Distribution") 

#' Example C.7
#' The probability that a person have hips wider than 18 inches, P(Y>18)
#' Using the normal distribution
1-pnorm(18, mean=mean(hip$y), sd=sd(hip$y))
#' Using the standard normal 
1-pnorm((18-mean(hip$y))/sd(hip$y))
#' or
1-pnorm((18-mean(hip$y))/sd(hip$y), mean=0, sd=1)
#' *100 % of the population would not be able to fit into an 18 inch wide seat

#' even nicer
xpnorm(18, mean=mean(hip$y),sd=sd(hip$y))

#' At what hip width would 95% of the population fit?
qnorm(0.95, mean=mean(hip$y),sd=sd(hip$y))
#' check the probability
xpnorm(qnorm(0.95, mean=mean(hip$y),sd=sd(hip$y)), mean=mean(hip$y),sd=sd(hip$y))


#' Example C.8 
#' Simulating intervals
#' Y ~ N(10, 10)
rnorm(30, mean = 10, sd = sqrt(10)) # 30 values
mean(rnorm(30, mean = 10, sd = sqrt(10)))
confint(rnorm(30, mean = 10, sd = sqrt(10)))

#' 10 means of 30 values
do(10)*mean(rnorm(30, mean = 10, sd = sqrt(10)))

#' 10 confidence intervals
rmeans <- do(10)*mean(rnorm(30, mean = 10, sd = sqrt(10)))

#' Use formula C.13 to calculate the 95% interval, p. 823
 
#' The critical z value for calculating a 95% interval
qnorm(0.975)
qnorm(0.025)

value = 10
rmeans %>% 
  mutate(LowerBound=mean-1.96*sqrt(10/30),
         UpperBound=mean+1.96*sqrt(10/30)) %>%
  mutate(ok = value >= LowerBound & value <= UpperBound)

#' do 10000 simulations
rmeans10000 <- do(10000)*mean(rnorm(30, mean = 10, sd = sqrt(10)))

value = 10
rmeans10000 <- rmeans10000 %>%   
  mutate(LowerBound=mean-1.96*sqrt(10/30),
         UpperBound=mean+1.96*sqrt(10/30)) %>%
  mutate(ok = value >= LowerBound & value <= UpperBound)

prop(~ok, data = rmeans10000)
1-prop(~ok, data = rmeans10000)

#' Example C.10 Simulating the hip data
#' Confidence interval in linear regression with only intercept, is mean of y, using all observations
mean(~y, data=hip)
m1 <- lm(y~1 , data=hip)
summary(m1)
confint(m1)

#' Confidence interval using Monte Carlo simulations, or bootstrapping
#browseURL("https://pirates.fandom.com/wiki/William_Turner_Sr.")
resample(hip)

trials <- do(10000)*mean(~y, data = resample(hip))
gf_histogram(~mean, data = trials, xlab = "Mean Hip Size")
confint(trials, level = 0.95, method = "quantile")


library(HH)
#' Example C.11 & Example C.13
#' One tail t-test, H0: mu=16.5, H1: mu > 16.5
t.test(~y, data=hip, alternative='greater', mu=16.5, conf.level=.95)
NTplot(t.test(~y, data=hip, alternative='greater', mu=16.5, conf.level=.95))
qt(0.975,49)
#' so calc t > crit t -> reject H0

#' Example C.12 & Example C.14
#' Two tail t-test, H0: mu=17, H1: mu != 17
t.test(~y, data=hip, alternative='two.sided', mu=17, conf.level=.95)
NTplot(t.test(~y, data=hip, alternative='two.sided', mu=17, conf.level=.95))
#' If we are using p-values, we reject the H0 when p < alpha (level of significance)
#' If we calculate a 1-alpha confidence interval, and the value under H0 is inside this
#' interval, we keep the H0, if outside, we reject H0.

#' Bootstrapping the CI from C.12, N=10000
head(trials)
qdata(~mean, c(.0255, .975), data = trials) 

#' Some useful tests
x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)

#' Do x and y have the same variance?
var.test(x, y)                  
var.test(lm(x ~ 1), lm(y ~ 1))  # The same.

#' Do x and y have the same mean?
t.test(x,y)
t.test(y,x, var.equal = FALSE)
t.test(y,x, var.equal = TRUE) # wrong based on F-test above

#' Example C. 15
#' Testing the normality of the hip data
require(moments)
#' H0: Normality
jarque.test(hip$y)

#' C.8 Introduction to Maximum Likelihood Estimation
#' The wheel of fortune, spin 3 times: WIN, WIN, LOSS
#' You do not know which wheel was chosen, and you must pick which wheel was spun
#' Which would you select?

#' Wheel A 25% shaded, prob of winning
#' The probability, or likelihood, of obserwing the sequence, WIN, WIN, LOSS,
#' when p(winning)=1/4 is:
A <- c(1/4,1/4,3/4)
prod(A)

#' Wheel B 75% shaded, prob of winning
B <- c(3/4,3/4,1/4)
#' The probability, or likelihood, of obserwing the sequence, WIN, WIN, LOSS,
#' when p(winning)=3/4 is:
prod(B)

#' Based on the available data;
prod(B)/prod(A) # it is 3 times more likely that wheel B was spun

#' The maximum likelihood principle, seeks to maximize the probability, or likelihood
#' of observing the outcomes actually obtained

#' Wheel of fortune game, 0 < p < 1, any value of p between 0 and 1
#' Likelihood function L(p) for WIN, WIN, LOSS:
#' L(p)=p x p x (1-p)
L <- function(p) {p^2-p^3}
curve(L(x),0,1, main="Figure C.11 - A Likelihood function")
L(0.5) # Value of the function at p=.5
#' Where is its maximum

#' Find the derivative of the likelihood function
D(expression(p^2-p^3),"p")
#' The derivative of the likelihood function as an R function
dL <- function (p) {2*p-3*p^2}
curve(dL(x), 0,1, main="Derivative of the Likelihood function")
#' Where is the derivative equal to zero
abline(h=0,v=2/3, col="red")

#install.packages("rootSolve")
#http://cran.r-project.org/web/packages/rootSolve/vignettes/rootSolve.pdf 
# require(rootSolve)
# # Find the roots where dL is equal to zero
# uniroot.all(dL, c(0,1)) 
# soln <- uniroot.all(dL, c(0,1))
# points(soln, y = rep(0, length(soln)), pch = 16, cex = 2, col="blue")

findZeros(dL(p)~p)

#' Log-Likelihood function
logL <- function (p) {2*log(p)+log(1-p)}
curve(logL(x),0,1, main="Log-Likelihood function")

#' Find the maximum of both functions 
optimize(L, interval=c(0,1), maximum=TRUE)
optimize(logL, interval=c(0,1), maximum=TRUE)

#' Example C.20 Testing a Population Proportion, chi-square value in example C.21
prop.test(x=75, n=200, p=0.4, alternative ="two.sided", conf.level = 0.95, correct = FALSE)

#' Example C.24 Minimizing the sum of squares, hip data
a0 <- sum(hip$y^2)
a1 <- sum(hip$y)
a2 <- length(hip$y)

S <- function(mu) {a0 - 2*a1*mu + a2*mu^2}
plotFun(S(mu)~mu, xlim = c(14,20), main="Figure C.18 - The sum of squares parabola for the hip data")

#' Kernel density estimators
density(hip$y)
plot(density(hip$y))
densityplot(~y, data=hip)
gf_density(~y, data=hip)

