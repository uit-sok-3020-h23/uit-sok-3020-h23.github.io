rm(list=ls())

1:9
mean(1:9)

10:16
mean(10:16)

# differences in mean, small - large
mean(1:9) - mean(10:16)
# differences in mean, large - small
mean(10:16) - mean(1:9)

# make data
group1 <- data.frame(y=1:9, g="group_1", d=0, t=FALSE)
group1
str(group1)

group2 <- data.frame(y=10:16, g="group_2", d=1, t=TRUE)
group2
str(group2)

# stack data, bind rows
dframe <- rbind(group1,group2)
dframe

library(mosaic)
# the grand mean, independent of groups
mean(~y, data = dframe)

# the group means
mean(y~g, data = dframe)
mean(y~d, data = dframe)
mean(y~t, data = dframe)

# the group means as a linear model with only intercept
lm(y~1, data = filter(dframe, g=="group_1")) 
lm(y~1, data = filter(dframe, g=="group_2")) 

# the differences in group means, dependent on reference category
diffmean(y~g, data = dframe)
diffmean(y~d, data = dframe)
diffmean(y~t, data = dframe)

# the differences in group means, as a linear model with a dummy variable
lm(y~g, data = dframe) 
lm(y~d, data = dframe) 
lm(y~t, data = dframe) 

# group 1 is the base, i.e., the intercept, the parameter on ggroup_2 is the difference in mean between groups
coef(lm(y~g, data = dframe))
sum(coef(lm(y~g, data = dframe))) # group 2 mean

# group 1 is the base, i.e., the intercept, the parameter on d is the difference in mean between groups
coef(lm(y~d, data = dframe))
sum(coef(lm(y~d, data = dframe))) # group 2 mean

# group 1 is the base, i.e., the intercept, the parameter on tTRUE is the difference in mean between groups
coef(lm(y~t, data = dframe))
sum(coef(lm(y~t, data = dframe))) # group 2 mean

# switch coding (inverse)
dframe <- dframe %>% mutate(gI=ifelse(g=="group_1","group_2","group_1"),
                  dI=ifelse(d==1,0,1),
                  tI=ifelse(t==TRUE,FALSE,TRUE)) %>% select(y,g,gI,d,dI,t,tI)
dframe

# notice group 2 is now the base, i.e. the intercept is its mean, the difference in mean is the same,
# but with oposite sign!
lm(y~g, data = dframe) 
lm(y~gI, data = dframe) 

lm(y~d, data = dframe) 
lm(y~dI, data = dframe) 

lm(y~t, data = dframe) 
lm(y~tI, data = dframe) 

# t-test on the difference between means
summary(lm(y~g, data = dframe))
t.test(y~gI, data = dframe, var.equal=TRUE)
