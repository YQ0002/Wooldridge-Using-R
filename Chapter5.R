# Chapter 5 
library(wooldridge)

# Figure 5.2
hist(k401k$prate,freq=FALSE)

# Example 5.2
help(bwght)
lbwght <- lm(lbwght~cigs+lfaminc,bwght)
summary(lbwght)
SE_cigs <- summary(lbwght)$coef[2,2]
length(bwght$faminc)/2
data <- bwght[0:694,]
n1 <- length(bwght$lbwght)
n2 <- length(data$lbwght)
lbwght2 <- lm(lbwght~cigs+lfaminc,data)
summary(lbwght2)
SE_cigs2 <- summary(lbwght2)$coef[2,2]
SE_cigs/SE_cigs2
sqrt(n2/n1)

# Example 5.3
help(crime1)
# narr86_1 = beta_0 + beta_1 * pcnv + beta_2 * avgsen + beta_3 * tottime + beta_4 * ptime86 + beta_5 * qemp86 + u
narr86_1 <- lm(narr86~pcnv+avgsen+tottime+ptime86+qemp86,crime1)
summary(narr86_1)
# H0: beta_2 = 0 and beta_3 = 0
narr86_2 <- lm(narr86~pcnv+ptime86+qemp86,crime1)
summary(narr86_2)
u_2 <- residuals(narr86_2)
n <- length(u_2)
LM_u <- lm(u_2~pcnv+ptime86+qemp86+avgsen+tottime,crime1)
Ru_2 <- summary(LM_u)$r.squared
LM <- n * Ru_2
df <- (3-1)*(2-1)
LM > qchisq(0.9,df) # FALSE, fail to reject H0
LM > qchisq(0.85,df) # TRUE, H0 rejected

# Problems
# 1
# y = beta_0 + beta_1 * x + u, plim(beta_1_hat) = beta_1
# proof: plim(beta_0_hat) = beta_0
# y_mean = beta_0_hat + beta_1_hat * x_mean
# beta_0_hat = y_mean - beta_1_hat * x_mean
# beta_0_hat = beta_0 + beta_1 * x_mean + u_mean - beta_1_hat * x_mean
# beta_0_hat = beta_0 + (beta_1 - beta_1_hat) * x_mean
# plim(beta_1_hat) - beta_1 = 0, E(u_mean) = 0, 
# plim(beta_0_hat) = beta_0

# 3
help(smoke)
hist(smoke$cigs,breaks=20,freq=FALSE)
x <- seq(1,80,length.out=80)
y <- dnorm(x,mean(smoke$cigs),sd(smoke$cigs))
lines(x,y)

# 4
# y = beta_0 + beta_1 * x + u
# E(y) = beta_0 + beta_1 * E(x) + E(u)
# E(u) = 0, E(y) = μ_y, E(x) = μ_x
# beta_0 = μ_y - beta_1 * μ_x
# beta_0_hat = mean(y) - beta_1_hat * mean(x)
# beta_0_est = mean(y) - beta_1_est * mean(x)
# plim(beta_0_hat) = plim(mean(y) - beta_1_hat * mean(x))
# plim(beta_0_hat) = plim(mean(y)) - plim(beta_1_hat) * plim(mean(x))
# plim(mean(y)) = μ_y, plim(mean(x)) = μ_x 
# plim(beta_0_est) = beta_0

# 5
help(econmath)
hist(econmath$score,breaks=30,freq=FALSE)
x <- seq(20,100,length.out=100)
y <- dnorm(x,mean(econmath$score),sd(econmath$score))
lines(x,y)
# 1) Estimate the probability that score exceeds 100 by normal distribution
1-pnorm(100,mean(econmath$score),sd(econmath$score))


# Computer Exercises
library(wooldridge)

# C1
help(wage1)
wage <- lm(wage~educ+exper+tenure,wage1)
summary(wage)
# 1) plot histogram for wage
hist(residuals(wage),breaks=20,freq=FALSE)
summary(residuals(wage))
x <- seq(min(residuals(wage)),max(residuals(wage)),0.1)
y <- dnorm(x,mean(residuals(wage)),sd(residuals(wage)))
lines(x,y)
# 2) plot histogram for lwage
lwage <- lm(lwage~educ+exper+tenure,wage1)
summary(lwage)
hist(residuals(lwage),breaks=20,freq=FALSE)
summary(residuals(lwage))
x <- seq(min(residuals(lwage)),max(residuals(lwage)),0.05)
y <- dnorm(x,mean(residuals(lwage)),sd(residuals(lwage)))
lines(x,y)
# 3) which model fits MLR.6 better
SSR_wage <- sum(residuals(wage)^2)
SSR_lwage <- sum(residuals(lwage)^2)
SSR_wage > SSR_lwage # lwage fits better

# C2
help(gpa2)
dim(gpa2)
# 1) colgpa = beta_0 + beta_1 * hsperc + beta_2 * sat + u
colgpa <- lm(colgpa~hsperc+sat,gpa2)
summary(colgpa)
# 2) using the first 2,070 data
data <- gpa2[0:2070,]
dim(data)
colgpa2 <- lm(colgpa~hsperc+sat,data)
summary(colgpa2)
# 3)the ratio of the standard errors on hsperc
se_hsperc_1 <- summary(colgpa)$coef[2,2]
se_hsperc_2 <- summary(colgpa2)$coef[2,2]
se_hsperc_2/se_hsperc_1
n_1 <- length(gpa2$sat)
n_2 <- length(data$sat)
E_se <- sqrt(n_1/n_2)
E_se  
# se(beta_j_hat) ≈ c_j / sqrt(n)
# c_j= δ / (δ_j * sqrt(1-p_j_square))
δ <- sd(residuals(colgpa))
colgpa_1 <- lm(colgpa~sat,gpa2)
δ_j <- sd(residuals(colgpa_1))
p_j_square <- summary(colgpa_1)$r.squared
c_j <- δ / (δ_j * sqrt(1-p_j_square))
se_beta_j_hat <- c_j / sqrt(n_1)
se_beta_j_hat     

# C3
# b1 = beta_0 + beta_1 * cigs + beta_2 * parity + beta_3 * faminc + beta_4 * motheduc + beta_5 * fatheduc + u
help(bwght)
b1 <- lm(bwght~cigs+parity+faminc+motheduc+fatheduc,bwght)
summary(b1)
# b2 = beta_0 + beta_1 * cigs + beta_2 * parity + beta_3 * faminc + u
b2 <- lm(bwght~cigs+parity+faminc,data=bwght)
summary(b2)
# LM H0: beta_5 = 0 and beta_6 = 0
re_2 <- residuals(b2)
n <- length(b2)
LM_re <- lm(re_2~cigs+parity+faminc+motheduc+fatheduc,bwght)
R_2 <- summary(LM_re)$r.squared
LM <- n * R_2
df <- (3-1)*(2-1)
LM > qchisq(0.95,df) # FALSE, fail to reject H0

# C4
# skewness = n^-1 * ((x - mean(x))/sd(x))^3
# 1) skewness of inc and log(inc) when fsize = 1 under k401ksubs
help(k401ksubs)
data <- subset(k401ksubs,fsize==1)
summary(data)
sum(((data$inc-mean(data$inc))/sd(data$inc))^3)/length(data$inc)
sum(((log(data$inc)-mean(log(data$inc)))/sd(log(data$inc)))^3)/length(data$inc)
# install.packages('moments')
library(moments)
skewness(data$inc)
skewness(log(data$inc))
# 2) skewness of bwght and log(bwght) under bwght2
help(bwght2)
skewness(bwght2$bwght)
skewness(log(bwght2$lbwght))

# C5
help(htv)
# educ = beta_0 + beta_1 * motheduc + beta_2 * fatheduc + beta_3 * abil + beta_4 * abil^2 + u
abil_2 <- htv$abil^2
data <- cbind(htv,abil_2)
educ <- lm(educ~motheduc+fatheduc+abil+abil_2,data)
summary(educ)
# 1) 
length(htv$educ)
library(moments)
skewness(htv$educ)
# 2) 
hist(htv$educ,freq=FALSE)
summary(htv$educ)
x <- seq(min(htv$educ),max(htv$educ),0.1)
y <- dnorm(x,mean(htv$educ),sd(htv$educ))
lines(x,y)

# C6
help(econmath)
# 1) 
length(econmath$score)
summary(econmath$score)
# 2) score = beta_0 + beta_1 * colgpa + beta_2 * actmth + beta_3 * acteng + u
# t test on H0: beta_3 = 0
score <- lm(score~colgpa+actmth+acteng,econmath) 
hist(residuals(score),freq=FALSE)
summary(residuals(score))
x <- seq(min(residuals(score)),max(residuals(score)),0.1)
y <- dnorm(x,mean(residuals(score)),sd(residuals(score)))
lines(x,y)
t_acteng <- summary(score)$coef[4,3]
t_acteng > qnorm(0.95) # FALSE, fail to reject H0
