# Chapter 9 
rm(list=ls())
library(wooldridge)

# 9-1 Functional Form Misspecification
# Example 9.1
help(crime1)
avgsensq <- crime1$avgsen^2
narr86 <- lm(narr86~pcnv+avgsen+avgsensq+ptime86+qemp86+inc86+black+hispan,crime1)
summary(narr86)
narr86_2 <- lm(narr86~pcnv+pcnvsq+avgsen+avgsensq+ptime86+pt86sq+qemp86+inc86+inc86sq+black+hispan,crime1)
summary(narr86_2)

library(stargazer)
stargazer(narr86, narr86_2, type = "text")
help(stargazer)

# H0: beta_2 = beta_6 = beta_9 = 0
SSR_r <- sum(residuals(narr86)^2)
SSR_ur <- sum(residuals(narr86_2)^2)
q <- 3
df1 <- summary(narr86)$fstatistic[3]
df2 <- summary(narr86_2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected
F

# 9-1a regression specification error test (RESET)
# If y = beta_0 + beta_1 * x_1 + ... + beta_k * x_k + u satisfies MLR.4
# MLR.4: E(u|x_1, ... , x_k) = 0
# then no nonlinear functions of the independent variables should be significant when added to the equation
# nonlinear functions: y^2 and y^3

# Example 9.2
help(hprice1)
price <- lm(price~lotsize+sqrft+bdrms, hprice1)
summary(price)
library(lmtest)
resettest(price, power=2:3, type="fitted")
help(resettest)
# RESET = 9.6038, p-value = 1.703e-05
lprice <- lm(lprice~llotsize+lsqrft+bdrms, hprice1)
summary(lprice)
resettest(lprice, power=2:3, type="fitted")
# RESET = 2.565, p-value = 0.08308
# lprice fits better.
# RESET is a functional form test, and nothing more.

# 9-2 Using Proxy Variables for Unobserved Explanatory Variables
# Example 9.3
help(wage2)
lwage_1 <- lm(lwage~educ+exper+tenure+married+south+urban+black,wage2)
lwage_2 <- lm(lwage~educ+exper+tenure+married+south+urban+black+IQ,wage2)
lwage_3 <- lm(lwage~educ+exper+tenure+married+south+urban+black+IQ+educ*IQ,wage2)
library(stargazer)
stargazer(lwage_1, lwage_2, lwage_3, type = "text")

# 9-2a Using Lagged Dependent Variables as Proxy Variables
# Example 9.4
help(crime2) 
crime2_87 <- subset(crime2, year==87) 
lcrmrte1 <- lm(lcrmrte~unem+llawexpc, crime2_87)
summary(lcrmrte1)
crime2_82 <- subset(crime2, year==82) 
lcrmrte_82 <- crime2_82$lcrmrte
lcrmrte2 <- lm(lcrmrte~unem+llawexpc+lcrmrte_82, crime2_87)
summary(lcrmrte2)
library(stargazer)
stargazer(lcrmrte1, lcrmrte2, type = "text")

# 9-3 Models with Random Slopes
# y_i = a_i + b_i * x_i
# Define α = E(a_i) and β = E(b_i)
# Call β the Average Partial Effect(APE), or the Average Marginal Effect(AME)
# Say a_i = α + c_i and b_i = β + d_i
# Then d_i is the individual-specific deviation from the APE
# By contraction, E(c_i) = 0 and E(d_i) = 0
# y_i = α + β * x_i + c_i + d_i * x_i = α + β * x_i + u_i
# Where u_i = c_i + d_i * x_i
# MLR.4: E(u_i|x_i) = 0
# Then E(c_i|x_i) = E(c_i) = 0,  E(d_i|x_i) = E(d_i) = 0
# E(a_i|x_i) = E(a_i), E(b_i|x_i) = E(b_i)

# About Heteroskedasticity
# If Var(c_i|x_i) = (σ_c)^2, Var(d_i|x_i) = (σ_d)^2, and Cov(c_i,d_i|x_i) = 0,
# Then Var(u_i|x_i) = (σ_c)^2 + (σ_d)^2 * (x_i)^2

# 9-4 Properties of OLS under Measurement Error
# 9-4a Measurement Error in the Dependent Variable
# y* = beta_0 + beta_1 * x_1 + ... + beta_k * x_k + u
# Assuming it fits MLR.1~4
# e_0 = y - y*
# y* = y - e_0 
# y = beta_0 + beta_1 * x_1 + ... + beta_k * x_k + u + e_0
# If e_0 is independent of the explanatory variables, the OLS is still good. 
# Var(u+e_0) = (σ_u)^2 + (σ_e_0)^2 > (σ_u)^2
# Otherwise it can cause biases in OLS.

# 9-4b Measurement Error in an Explanatory Variable
# y = beta_0 + beta_1 * x*_1 + u
# Assuming it fits MLR.1~4
# e_1 = x - x*
# E(e_1) = 0, E(u_i|x*_1)=0, E(u_i|x_1)=0
# E(y|x*_1,x_1) = E(y|x*_1)
# Replacing x*_1 by x_1 in function y
# Assuming Cov(x_1, e_1) = 0
# x* = x - e_1
# y = beta_0 + beta_1 * x_1 + (u - beta_1 * e_1)
# Var(u-beta_1*e_1) = (σ_u)^2 + (beta_1)^2*(σ_e_1)^2 > (σ_u)^2, beta_1 ≠ 0

# 9-5 Missing Data, Nonrandom Samples, and Outlying Observations

# 9-5c Outlying Observations
# Example 9.8
help(rdchem)
rdintens <- lm(rdintens~sales+profmarg, rdchem)
summary(rdintens)
plot(rdchem$sales, rdchem$rdintens)
rdchem_2 <- subset(rdchem, sales!=max(sales))
dim(rdchem)
dim(rdchem_2)
rdintens2 <- lm(rdintens~sales+profmarg, rdchem_2)
summary(rdintens2)
library(stargazer)
stargazer(rdintens, rdintens2, type = "text")

matrix(c(rdchem$sales,residuals(rdintens)),32)
plot(rdchem$sales,residuals(rdintens))

dummy1 <- rdchem$sales == max(rdchem$sales)
rdintens_d1 <- lm(rdintens~sales+profmarg+dummy1, rdchem)
summary(rdintens_d1)
dummy2 <- residuals(rdintens) == max(residuals(rdintens))
rdintens_d2 <- lm(rdintens~sales+profmarg+dummy2, rdchem)
summary(rdintens_d2)
stargazer(rdintens_d1, rdintens_d2, type = "text")
rdintens_d3 <- lm(rdintens~sales+profmarg+dummy1+dummy2, rdchem)
summary(rdintens_d3)

# Example 9.9
# rd = sales^(beta_1)*exp(beta_0 + beta_2*profmarg + u)
# log(rd) = beta_0 + beta_1 * log(sales) + beta_2 * profmarg + u
lrd1 <- lm(lrd~lsales+profmarg, rdchem)
summary(lrd1)
lrd2 <- lm(lrd~lsales+profmarg, rdchem_2)
summary(lrd2)
library(stargazer)
stargazer(lrd1, lrd2, type = "text")

# Example 9.10
help(infmrt)
infmrt_1 <- subset(infmrt, year==1990)
infmort1 <- lm(infmort~lpcinc+lphysic+lpopul, infmrt_1)
summary(infmort1)
matrix(c(infmrt_1$DC, infmrt_1$infmort),51)
infmrt_2 <- subset(infmrt_1, DC==0)
infmort2 <- lm(infmort~lpcinc+lphysic+lpopul, infmrt_2)
summary(infmort2)
library(stargazer)
stargazer(infmort1, infmort2, type = "text")


# Problems
# 1
help(ceosal2)
lsalary1 <- lm(lsalary~lsales+lmktval+profmarg+ceoten+comten,ceosal2)
summary(lsalary1)
lsalary2 <- lm(lsalary~lsales+lmktval+profmarg+ceoten+comten+ceotensq+comtensq,ceosal2)
summary(lsalary2)
SSR_r <- sum(residuals(lsalary1)^2)
SSR_ur <- sum(residuals(lsalary2)^2)
q <- 2
df1 <- summary(lsalary1)$fstatistic[3]
df2 <- summary(lsalary2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # True, H0 rejected
F

# 2
help(vote1)
head(vote1,5)

help(vote2)
head(vote2,5)


vote90 <- lm(vote90~prtystr+democ, vote2)
summary(voteA90)

