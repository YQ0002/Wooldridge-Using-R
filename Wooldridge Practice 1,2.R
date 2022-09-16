# Introductory Econometrics A Modern Approach

# install.packages('wooldridge')
library(wooldridge)

# Chapter 1 
# Computer Exercises
# C1
head(wage1,3)
help(wage1)

# 1) education level
summary(wage1$educ)
c(mean(wage1$educ),max(wage1$educ),min(wage1$educ))
# 2) average hourly wage
mean(wage1$wage)
# 3) female
table(wage1$female)

# C2
head(bwght,3)
help(bwght)

# 1) smocking during pregnancy
length(bwght$cigs)
length(bwght$cigs[bwght$cigs>0])
# 2) average number of cigarettes smoked per day
mean(bwght$cigs)
# 3) smoker's average
mean(bwght$cigs[bwght$cigs>0])
# 4) average fatheduc
length(bwght$fatheduc)
table(is.na(bwght$fatheduc))
mean(bwght$fatheduc, na.rm = T)
# 5) average and sd of family income 
mean(bwght$faminc)
sd(bwght$faminc)

# C3
head(meap01,3)
help(meap01)

# 1) largest and smallest values of math4
c(max(meap01$math4), min(meap01$math4))
summary(meap01$math4)
# 2) schools with perfect pass rate
length(meap01$math4[meap01$math4==100])
length(meap01$math4[meap01$math4==100])/length(meap01$math4)
# 3) schools with exactly 50% pass rate
length(meap01$math4[meap01$math4==50])
# 4) compare average pass rates of math and reading
mean(meap01$math4)
mean(meap01$read4)
with(meap01,mean(math4)>mean(read4))
help(with)
# 5) correlation between math4 and read4
cor(meap01$math4,meap01$read4)
with(meap01, cor(math4,read4))
# 6) average and sd of exppp
mean(meap01$exppp)
sd(meap01$exppp)
with(meap01, c(mean(exppp),sd(exppp)))
plot(meap01$lexpend)

# C4
head(jtrain2,3)
str(jtrain2)
help(jtrain2)

# 1) fraction of men who received job training
table(jtrain2$train)
with(jtrain2,length(train[train==1])/length(train))
# 2) average of re78 for trained and untrained groups
length(jtrain2$re78)
aggregate(jtrain2$re78,by=jtrain2[c('train')],FUN=mean)
help(aggregate)
# 3) fraction of unemployed men who received training
with(jtrain2,table(unem78,train))
prop.table(with(jtrain2,table(unem78,train)),2)
help(prop.table)

# C5
head(fertil2,3)
help(fertil2)

# 1) average, smallest and largest values of children
summary(fertil2$children)
# 2) percentage of women have electricity
table(fertil2$electric)
with(fertil2,length(electric[electric==1])/length(electric))
# 3) average of children with and without electricity
with(fertil2,table(electric,children))
mean(fertil2$children)
aggregate(fertil2$children,by=fertil2[c('electric')],FUN=mean)

# C6
head(countymurders,3)
help(countymurders)

# 1)  0 murders and 0 executions countries in 1996 
data <- subset(countymurders,year==1996)
with(data,length(countyid))
with(data,length(countyid[murders==0]))
with(data,length(countyid[execs==0]))
# 2) largest number of murders and executions in 1996
max(data$murders)
max(data$execs)
mean(data$execs)
# 3) correlation coefficient between murders and execs
cor(data$murders,data$execs)

# C7
head(alcohol,3)
??alconol
str(wooldridge::alcohol)	

# 1) percentage of alcohol abusers
table(alcohol$abuse)
with(alcohol,length(abuse[abuse==1])/length(abuse))
# 2) employment rate of alcohol abusers
with(alcohol,table(abuse,status))
abusers <- subset(alcohol,abuse==1)
with(abusers,length(abuse[status==3])/length(abuse))
# 3) employment rate of sobers
sobers <- subset(alcohol,abuse==0)
with(sobers,length(abuse[status==3])/length(abuse))

# C8
head(econmath,3)
help(econmath)

# 1) students taking an economics course
table(econmath$econhs)
with(econmath,length(econhs[econhs==1]))
# 2) average score of students who did and did not take economics
with(econmath,mean(score[econhs==1]))
with(econmath,mean(score[econhs==0]))

# Chapter 2

head(ceosal1,3)
str(ceosal1)
sal <- lm(salary~roe,data = ceosal1)
plot(sal)

head(wage1,3)
mean(wage1$wage)

y <- lm(wage1$wage~wage1$educ)
summary(y)
summary(y)["r.squared"]

z <- lm(log(wage1$wage)~wage1$educ)
summary(z)
summary(z)[c("r.squared", "adj.r.squared")]

# Problems
# 3
Students <- c(1:8)
GPA <- c(2.8,3.4,3.0,3.5,3.6,3.0,2.7,3.7)
ACT <- c(21,24,26,27,29,25,25,30)
data <- data.frame(Students,GPA,ACT)

# 1) Estimate the relationship between GPA and ACT using OLS
# method a)
beta_1 <- cov(ACT,GPA)/var(ACT)
beta_0 <- mean(GPA)-beta_1*mean(ACT)
c(beta_0,beta_1)
# method b)
ols_1 <- lm(GPA~ACT,data = data)
summary(ols_1)
summary(ols_1)$coefficients
# method c)
lsfit(ACT,GPA)$coefficients
help(lsfit)
# 2) Compute the fitted values and residuals
# method a)
fitted_value <- beta_0 + ACT*beta_1
residuals <- GPA - fitted_value
# method b)
fitted(ols_1)
residuals(ols_1)
# 3) When ACT = 20
beta_0 + 20*beta_1
# 4) How much does ACT explains the GPA (R-suqared value)
# method a) R^2=1-SSR/SST
R2 <- 1-(sum(residuals^2)/sum((GPA-mean(GPA))^2))
R2
# method b)
summary(ols_1)$r.squared

# 4
head(bwght,3)
help(bwght)
length(bwght$parity)
bwght <- lm(bwght~cigs,data = bwght)
summary(bwght)$coefficients

# 1) The predicted birth weight when cigs = 0 and 70
a <- data.frame(cigs = c(0,70))
result <-  predict(bwght,a)
predict(bwght,a)
help(predict)
# 3) Predict cigs when birth weight = 125
coef(bwght)
b1 <- bwght$coefficients['cigs']
b0 <- bwght$coefficients['(Intercept)']
(125 - b0)/b1

# 5
# cons = -124.84 + 0.853 * inc
# n = 100, R^2 = 0.692
# MPC(slop): marginal propensity to consume
# APC(cons/inc): average propensity to consume

# 2) The predicted consumption when income is $30,000
-124.84 + 0.853 * 30000
# 3) Draw estimated MPC and APC
inc <- c(1:100)
cons = -124.84 + 0.853 * inc
MPC <- 0.853
APC <- cons/inc
plot(inc,APC,type = 'l')
abline(h=MPC)

# Computer Exercises
# C1
View(k401k)
head(k401k,3)
help(k401k)

# 1) The average participation rate and the average match rate
mean(k401k$prate)
mean(k401k$mrate)
# 2) Estimate the SLR
prate <- lm(prate~mrate,data = k401k)
summary(prate)$coefficients
summary(prate)$r.squared
# 4) Predict prate when mrate = 3.5
predict(prate,data.frame(mrate=3.5))

# C2
head(ceosal2,3)
help(ceosal2)

# 1) The average salary and the average tenure
c(mean(ceosal2$salary),mean(ceosal2$ceoten))
# 2) The numbers of CEOs in their first year; the longest tenure as CEO
with(ceosal2,length(ceoten[ceoten==0]))
max(ceosal2$ceoten)
# 3) log(salary) = beta_0 + beta_1 * ceoten + u
log_salary <- lm(lsalary~ceoten,data = ceosal2)
coef(log_salary)

# C3
head(sleep75,3)
help(sleep75)
sleep <- lm(sleep~totwrk,data = sleep75)

# 1) intercept and R^2
summary(sleep)[c('coefficients','r.squared')]
# 2) If totwrk increases by 2 hours, how much is sleep estimated to fall
coef(sleep)
sleep$coefficients['totwrk']*2

# C4
head(wage2,3)
help(wage2)

# 1) The average salary and average IQ
with(wage2,c(mean(wage),mean(IQ)))
# 2) SLR of IQ and wage; predict increase of wage when IQ raises 15; R^2
wage <- lm(wage~IQ,data = wage2)
summary(IQ_wage)$r.squared
coef(wage)
p <- 15 * wage$coefficients['IQ']
p
# 3.1) Log regression between wage and IQ 
lwage <- lm(lwage~IQ, wage2)
coef(lwage)
summary(lwage)$r.squared
# 3.2) percentage increase of wage when IQ increased by 15
q <- 15 * lwage$coefficients['IQ']
q

# C5
head(rdchem,3)
help(rdchem)
