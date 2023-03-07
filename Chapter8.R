# Chapter 8 Heteroskedasticity
rm(list=ls())
library(wooldridge)

# y_i = beta_0 + beta_1 * x_i + u_i
# var(u_i|x_i) = σ_i ^2

# beta_1_hat = beta_1 + (sum(x_i - x_mean)*u_i) / (sum(x_i - x_mean)^2)
# var(beta_1_hat) = (sum(x_i - x_mean)^2)*σ^2 / SST_x ^2
# SST_x = sum(x_i - x_mean)^2

# Heteroskedasticity-robust standard error
# var(beta_1_hat) = (sum(r_hat_ij ^2 * u_i_hat ^2) / SSR_j ^2

# 8-2 Heteroskedasticity-Robust Inference after OLS Estimation

# Example 8.1 
help(wage1)
marrmale <- (wage1$female==0 & wage1$married==1) + 0
marrfem <- (wage1$female==1 & wage1$married==1) + 0
singfem <- (wage1$female==1 & wage1$married==0) + 0
lwage1 <- lm(lwage~marrmale+marrfem+singfem+educ+exper+expersq+tenure+tenursq,wage1)
summary(lwage1)  

# Heteroskedasticity-robust t statistic
# install.packages("lmtest")
# install.packages("zoo")

library(lmtest)
library(car) 
coeftest(lwage1)
coeftest(lwage1, vcov = hccm(lwage1, type = "hc0"))
help(coeftest)
help(hccm)

# Example 8.2
help(gpa3)
data <- subset(gpa3, spring==1)
cumgpa <- lm(cumgpa~sat+hsperc+tothrs+female+black+white, data)
summary(cumgpa)

library(lmtest)
library(car)
coeftest(cumgpa)
coeftest(cumgpa, vcov = hccm(cumgpa, type = "hc0"))

# H0: beta_5 = 0, beta_6 = 0
cumgpa2 <- lm(cumgpa~sat+hsperc+tothrs+female, data)
summary(cumgpa2)
# F = ((R2_ur-R2_r)/q)/((1-R2_ur)/df_ur)
R2_r <- summary(cumgpa2)$r.squared
R2_ur <- summary(cumgpa)$r.squared
q <- 2 # Variables ignored in restricted model
df_r <- summary(cumgpa2)$fstatistic[3]
df_ur <- summary(cumgpa)$fstatistic[3]
F <- ((R2_ur-R2_r)/q)/((1-R2_ur)/df_ur)
F > qf(0.95,q,df_ur) # FALSE, fail to reject H0
F

# Heteroskedasticity-robust F statistic 
library(lmtest)
library(car)
waldtest(cumgpa, cumgpa2, vcov = vcovHC(cumgpa, type = "HC0")) # FALSE, fail to reject H0
help(waldtest)
help(vcovHC)

# Example 8.3
help(crime1)
avgsensq <- crime1$avgsen^2
narr86 <- lm(narr86~pcnv+avgsen+avgsensq+ptime86+qemp86+inc86+black+hispan,crime1)
summary(narr86)

library(lmtest)
library(car)
# coeftest(narr86)
# coeftest(narr86, vcov = hccm(narr86, type = "hc0"))
coeftest(narr86)[4,3]
coeftest(narr86, vcov = hccm(narr86, type = "hc0"))[4,3]

# Δnarr86/Δavgsen = beta_2 + 2 * beta_3
beta_2 <- coef(narr86)[3]
beta_3 <- coef(narr86)[4]
-beta_2 / (2 * beta_3)

# Heteroskedasticity-Robust LM Tests
# H0: beta_2 = 0, beta_3 = 0
narr86_2 <- lm(narr86~pcnv+ptime86+qemp86+inc86+black+hispan,crime1)
summary(narr86_2)
u_2 <- residuals(narr86_2)
n <- length(u_2)
LM_u <- lm(u_2~pcnv+avgsen+avgsensq+ptime86+qemp86+inc86+black+hispan,crime1)
summary(LM_u)
R2_u_2 <- summary(LM_u)$r.squared
LM <- n * R2_u_2
LM
df <- (6-1)*(5-1)
LM > qchisq(0.95,df) # FALSE, fail to reject H0

# 8-3 Testing for Heteroskedasticity

# Example 8.4
help(hprice1)
price <- lm(price~lotsize+sqrft+bdrms, hprice1)
# summary(price)
summary(price)$r.squared
u2_price <- residuals(price)^2
u2_price <- lm(u2_price~lotsize+sqrft+bdrms, hprice1)
summary(u2_price)
R2_u2 <- summary(u2_price)$r.squared
n <- length(hprice1$price)
k <- 3
df <- n-k-1
F <- (R2_u2/k)/((1-R2_u2)/df)
F > qf(0.95,k,df) # TRUE, H0 rejected
F
LM <- n * R2_u2
LM > qchisq(0.95,k) # TRUE, H0 rejected
LM

lprice <- lm(lprice~llotsize+lsqrft+bdrms, hprice1)
summary(lprice)
u2_lprice <- residuals(lprice)^2
u2_lprice <- lm(u2_lprice~llotsize+lsqrft+bdrms, hprice1)
summary(u2_lprice)
R2_u2 <- summary(u2_lprice)$r.squared
n <- length(hprice1$price)
k <- 3
df <- n-k-1
F <- (R2_u2/k)/((1-R2_u2)/df)
F > qf(0.95,k,df) # FALSE, fail to reject H0
F
LM <- n * R2_u2
LM > qchisq(0.95,k) # FALSE, fail to reject H0
LM

# BP test
# H0: homoscedasticity
library(lmtest)
library(car)
bptest(price)
# p-value = 0.002782, H0 rejected, heteroscedasticity exists
bptest(lprice)
# p-value = 0.2383, fail to reject H0
help(bptest)

# Example 8.5
help(hprice1)
lprice <- lm(lprice~llotsize+lsqrft+bdrms, hprice1)
lpirce_hat <- fitted(lprice)
lprice_hatsq <- lpirce_hat^2
u2_lprice <- residuals(lprice)^2
u2_LM <- lm(u2_lprice~lpirce_hat+lprice_hatsq)
summary(u2_LM)
R2 <- summary(u2_LM)$r.squared
n <- length(hprice1$price)
k <- 2
LM <- n * R2
LM > qchisq(0.95,k) # FALSE, fail to reject H0
LM
1-pchisq(LM,k)

# White test
library(lmtest)
library(car)
bptest(lprice, ~ fitted(lprice) + I(fitted(lprice)^2))


# 8-4 Weighted Least Squares (WLS) Estimation
# 8-4a The Heteroskedasticity Is Known up to a Multiplicative Constant

# Var(u|x) = σ^2 * h(x)
# (σ_i)^2 = Var(u_i|x_i) = σ^2 * h(x_i) = σ^2 * h_i
# E(u_i|sqrt(h_i)^2) = E(u_i^2)/h_i = (σ^2 * h_i)/h_i = σ^2

# Example 8.6
help(k401ksubs)
data <- subset(k401ksubs,fsize==1)
nettfa <- lm(nettfa~inc+age+agesq,data)
summary(nettfa)
# Δnettfa/Δage = beta_2 + 2 * beta_3
beta_2 <- coef(nettfa)['age']
beta_3 <- coef(nettfa)['agesq']
-beta_2 / (2 * beta_3)

nettfa_OLS1 <- lm(nettfa~inc, data)
summary(nettfa_OLS1)
nettfa_WLS1 <- lm(nettfa~inc, weight = 1/inc, data)
summary(nettfa_WLS1)
coeftest(nettfa_OLS1, vcov = hccm(nettfa_OLS1, type = "hc0"))

agesq0 <- (data$age-25)^2
nettfa_OLS2 <- lm(nettfa~inc+agesq0+male+e401k,data)
summary(nettfa_OLS2)
nettfa_WLS2 <- lm(nettfa~inc+agesq0+male+e401k, weight = 1/inc, data)
summary(nettfa_WLS2)
coeftest(nettfa_OLS2, vcov = hccm(nettfa_OLS2, type = "hc0"))


# 8-4b The Heteroskedasticity Function Must Be Estimated 
# Feasible GLS (FLGS) estimator: Heteroscedasticity is unknown

# Example 8.7
help(smoke)
cigs_OLS <- lm(cigs~lincome+lcigpric+educ+age+agesq+restaurn, smoke)
summary(cigs_OLS)

u2 <- residuals(cigs_OLS)^2
u2 <- lm(u2~lincome+lcigpric+educ+age+agesq+restaurn, smoke)
summary(u2)$r.squared

library(lmtest)
bptest(cigs_OLS)
# p-value = 1.456e-05, H0 rejected, heteroscedasticity exists

# Feasible GLS (FLGS)
u2 <- residuals(cigs_OLS)^2
log_u2 <- log(u2)
g_hat <- lm(log_u2~lincome+lcigpric+educ+age+agesq+restaurn, smoke)
h_hat <- exp(fitted(g_hat))
cigs_WLS <- lm(cigs~lincome+lcigpric+educ+age+agesq+restaurn, weight = 1/h_hat, smoke)
summary(cigs_WLS)

library(stargazer)
stargazer(cigs_OLS, cigs_WLS,
          column.labels = c("OLS", "FGLS"),
          type = "text",
          keep.stat = c("n","rsq"))
help(stargazer)

# 8-5 The Linear Probability Model Revisited

# Example 8.8 
help(mroz)
inlf <- lm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,mroz)
summary(inlf)

library(lmtest)
library(car)
coeftest(inlf)
coeftest(inlf, vcov = hccm(inlf, type = "hc0"))

# Example 8.9
help(gpa1)
parcoll <- (gpa1$fathcoll==1 | gpa1$mothcoll==1) + 0
PC_OLS <- lm(PC~hsGPA+ACT+parcoll, gpa1)
summary(PC_OLS)

library(lmtest)
library(car)
coeftest(PC_OLS)
coeftest(PC_OLS, vcov = hccm(PC_OLS, type = "hc0"))

bptest(PC_OLS)
# p-value = 0.04194, H0 rejected, heteroscedasticity exists

y_hat <- fitted(PC_OLS)
summary(y_hat)
h_hat <- y_hat * (1-y_hat)
PC_WLS <- lm(PC~hsGPA+ACT+parcoll, weight = 1/h_hat, gpa1)
summary(PC_WLS)

library(stargazer)
stargazer(PC_OLS, PC_WLS,
          column.labels = c("OLS", "WLS"),
          type = "text",
          keep.stat = c("n","rsq"))

# Problems
# 2
# y_beer = beta_0 + beta_1*inc + beta_2*price + beta_3*educ + beta_4*female + u
# E(u|inc, price, educ, female)  = 0
# Var(u|inc, price, educ, female) = (σ^2)*(inc^2)

# h(x) = h(inc, price, educ, female)
# Var(u|h(x)) = (σ^2)*(inc^2)
# SE(u|h(x)) = σ * inc
# E((u/inc)^2) = E(u^2)*(inc^2) = ((σ^2)*(inc^2))/(inc^2) = σ^2
# y_beer/inc = beta_0/inc + beta_1 + beta_2/inc*price + beta_3/inc*educ + beta_4/inc*female + u

# 4 
help(gpa3)
data <- subset(gpa3, c(spring==0 & frstsem==0))
trmgpa <- lm(trmgpa~crsgpa+cumgpa+tothrs+sat+hsperc+female+season,data)
summary(trmgpa)
library(lmtest)
library(car)
coeftest(trmgpa)
coeftest(trmgpa, vcov = hccm(trmgpa, type = "hc0"))
# 1)
t <- abs(summary(trmgpa)$coef[,3])
pnorm(t)
# 2)
# H0: beta_1 = 0
trmgpa_r <- lm(trmgpa~cumgpa+tothrs+sat+hsperc+female+season,data)
summary(trmgpa_r)
R2_r <- summary(trmgpa_r)$r.squared
R2_ur <- summary(trmgpa)$r.squared
q <- 1
df_r <- summary(trmgpa_r)$fstatistic[3]
df_ur <- summary(trmgpa)$fstatistic[3]
F <- ((R2_ur-R2_r)/q)/((1-R2_ur)/df_ur)
F > qf(0.9,q,df_ur) # TRUE, H0 rejected
F
# 3)
u2_r <- residuals(trmgpa_r)^2
log_u2_r <- log(u2_r)
g_hat <- lm(log_u2_r~cumgpa+tothrs+sat+hsperc+female+season,data)
h_hat <- exp(fitted(g_hat))
trmgpa_r_WLS <- lm(trmgpa~cumgpa+tothrs+sat+hsperc+female+season, weight = 1/h_hat, data)
summary(trmgpa_r_WLS)

u2_ur <- residuals(trmgpa)^2
log_u2_ur <- log(u2_ur)
g_hat <- lm(log_u2_ur~crsgpa+cumgpa+tothrs+sat+hsperc+female+season,data)
h_hat <- exp(fitted(g_hat))
trmgpa_ur_WLS <- lm(trmgpa~crsgpa+cumgpa+tothrs+sat+hsperc+female+season, weight = 1/h_hat, data)
summary(trmgpa_ur_WLS)

library(stargazer)
stargazer(trmgpa, trmgpa_ur_WLS, trmgpa_r, trmgpa_r_WLS,
          column.labels = c("OLS1", "WLS1", "OLS2", "WLS2"),
          type = "text",
          keep.stat = c("n","rsq"))

# 5
help(smoke)
smokes <- (smoke$cigs!=0) + 0
table(smokes)
smokes <- lm(smokes~lcigpric+lincome+educ+age+agesq+restaurn+white, smoke)
summary(smokes)
library(lmtest)
coeftest(smokes)
coeftest(smokes,vcov = hccm(smokes, type = "hc0"))
# 1)
se_1 <- coeftest(smokes)[,2]
se_2 <- coeftest(smokes,vcov = hccm(smokes, type = "hc0"))[,2]
matrix(c(se_1, se_2, se_1-se_2),8,3)
# 2)
# Δsmokes/Δeduc = beta_3
beta_3 <- coef(smokes)[4]
4 * beta_3
# 3)
# Δsmokes/Δage = beta_4 + 2 * beta_5
beta_4 <- coef(smokes)[5]
beta_5 <- coef(smokes)[6]
-beta_4 / (2 * beta_5)
# 5)
sample <- data.frame(lcigpric = log(67.44),
                     lincome = log(6500),
                     educ = 16,
                     age = 77,
                     agesq = 77^2,
                     restaurn = 0,
                     white = 0) # smokes = 0
predict(smokes,sample) # -0.003965415 
 
# 8
help(econmath)
male <- subset(econmath, male==1)
score_m <- lm(score~colgpa+act, male)
summary(score_m)
female <- subset(econmath, male==0)
score_f <- lm(score~colgpa+act, female)
summary(score_f)
score_a1 <- lm(score~male+colgpa+act, econmath)
summary(score_a1)
score_a2 <- lm(score~male+colgpa+act+male*colgpa+male*act,econmath)
summary(score_a2)
# 1) Chow test, H0: Regression equations are the same for men and women
# F = [SSR_p-(SSR_1+SSR_2)]/(SSR_1+SSR_2) * [n-2*(k+1)]/((k+1))
score_t <- lm(score~colgpa+act, econmath)
summary(score_t)
SSR_m <- sum(residuals(score_m)^2)
SSR_f <- sum(residuals(score_f)^2)
SSR_t <- sum(residuals(score_t)^2)
df <- summary(score_t)$fstatistic[3]
n <- length(econmath$score)
k <- 2 
F <- (SSR_t-(SSR_m+SSR_f))/(SSR_m+SSR_f) * (n-2*(k+1))/(k+1)
F > qf(0.95,k,df) # TRUE, H0 rejected
F
pf(F,k,df)
# 2) Chow test, H0: The slopes are the same for men and women
# F = [SSR_p-(SSR_1+SSR_2)]/(SSR_1+SSR_2) * (n-2*k)/(k) 
F <- (SSR_t-(SSR_m+SSR_f))/(SSR_m+SSR_f) * (n-2*k)/(k)
F > qf(0.95,k,df) # TRUE, H0 rejected
F
pf(F,k,df)

# Computer Exercises
# C1
help(sleep75)
# sleep = beta_0 + beta_1*totwrk + beta_2*educ + beta_3*age + beta_4*agesq + beta_5*yngkid + beta_6*male + u
# 2)
sleep_OLS <- lm(sleep~totwrk+educ+age+agesq+yngkid+male, sleep75)
summary(sleep_OLS)
u2 <- residuals(sleep_OLS)^2
u2 <- lm(u2~totwrk+educ+age+agesq+yngkid+male, sleep75)
summary(u2)
R2_u2 <- summary(u2)$r.squared
n <- length(sleep75$sleep)
k <- 6
df <- n-k-1
F <- (R2_u2/k)/((1-R2_u2)/df)
F > qf(0.95,k,df) # FALSE, fail to reject H0
F
# 3)
library(lmtest)
library(car) 
coeftest(sleep_OLS, vcov = hccm(sleep_OLS, type = "hc0"))["male",]

# C2
# 1)
help(hprice1)
price <- lm(price~lotsize+sqrft+bdrms, hprice1)
summary(price)
library(lmtest)
library(car) 
coeftest(price)
coeftest(price, vcov = hccm(price, type = "hc0"))
# 2)
lprice <- lm(lprice~llotsize+lsqrft+bdrms, hprice1)
summary(lprice)
coeftest(lprice)
coeftest(lprice, vcov = hccm(lprice, type = "hc0"))

# C3
lprice <- lm(lprice~llotsize+lsqrft+bdrms, hprice1)
library(lmtest)
library(car)
bptest(lprice, ~ fitted(lprice) + I(fitted(lprice)^2))
names(bptest(lprice, ~ fitted(lprice) + I(fitted(lprice)^2)))
bptest(lprice, ~ fitted(lprice) + I(fitted(lprice)^2))["p.value"]
df <- summary(lprice)$fstatistic[3]
k <- 3
pf(0.178415,k,df)

# C4
# 1)
help(vote1)
voteA <- lm(voteA~prtystrA+democA+lexpendA+lexpendB, vote1)
summary(voteA)
u <- residuals(voteA)
u <- lm(u~prtystrA+democA+lexpendA+lexpendB, vote1)
summary(u)
summary(u)$r.squared
# 2)
library(lmtest)
bptest(voteA)
# 3)
library(lmtest)
bptest(voteA, ~fitted(voteA) + I(fitted(voteA)^2))

# C5
help(pntsprd)
# 1)
table(pntsprd$sprdcvr)
# H0: μ = 0.5 at 10% significance level
sprdcvr <- lm(sprdcvr~1, pntsprd)
summary(sprdcvr)
t <- summary(sprdcvr)$coef[3]
abs(t) > qnorm(0.9) # TRUE, H0 rejected
# 2)
table(pntsprd$neutral)
# 3)
sprdcvr_OLS <- lm(sprdcvr~favhome+neutral+fav25+und25, pntsprd)
summary(sprdcvr_OLS)
library(lmtest)
coeftest(sprdcvr_OLS)
coeftest(sprdcvr_OLS, vocv = hccm(sprdcvr_OLS, type = "hc0"))
# 4)
# H0: beta_1 = beta_2 = beta_3 = beta_4 = 0
u_2 <- residuals(sprdcvr)
n <- length(u_2)
u_2 <- lm(u_2~favhome+neutral+fav25+und25, pntsprd)
summary(u_2)
R2_u_2 <- summary(u_2)$r.squared
LM <- n * R2_u_2
LM
df <- (4-1)*(1-1)
LM > qchisq(0.95,df) # TRUE, H0 rejected
# 5)
R2_u_2 <- summary(u_2)$r.squared
n <- length(u_2)
k <- 4
df <- n-k-1
F <- (R2_u_2/k)/((1-R2_u_2)/df)
F > qf(0.95,k,df) # FALSE, fail to reject H0
F


# C6
# 1)
help(crime1)
arr86 <- (crime1$narr86!=0) + 0
arr86_OLS <- lm(arr86~pcnv+avgsen+tottime+ptime86+qemp86,crime1)
summary(arr86_OLS)
y_hat <- fitted(arr86_OLS)
summary(y_hat)
# 2)
h_hat <- y_hat * (1-y_hat)
arr86_WLS <- lm(arr86~pcnv+avgsen+tottime+ptime86+qemp86, weight = 1/h_hat, crime1)
summary(arr86_WLS)
# 3)
# H0: beta_2 = 0, beta_3 = 0
arr86_OLS_r <- lm(arr86~pcnv+ptime86+qemp86, crime1)
summary(arr86_OLS_r)
y_hat_r <- fitted(arr86_OLS_r)
h_hat_r <- y_hat_r * (1-y_hat_r)
arr86_WLS_r <- lm(arr86~pcnv+ptime86+qemp86, weight = 1/h_hat, crime1)
summary(arr86_WLS_r)
SSR_r <- sum(residuals(arr86_WLS_r)^2)
SSR_ur <- sum(residuals(arr86_WLS)^2)
q <- 2
df1 <- summary(arr86_WLS_r)$fstatistic[3]
df2 <- summary(arr86_WLS)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 
F

# C7
help(loanapp)
# 1)
approve_OLS <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,loanapp)
summary(approve_OLS)
library(lmtest)
coeftest(approve_OLS)
se_1 <- coeftest(approve_OLS)["white","Std. Error"]
white_1 <- coeftest(approve_OLS)["white","Estimate"]
conf <- qnorm(0.975)
CI_non <- c(white_1-se_1*conf, white_1+se_1*conf)
coeftest(approve_OLS, vcov = hccm(approve_OLS, type = "hc0"))
se_2 <- coeftest(approve_OLS, vcov = hccm(approve_OLS, type = "hc0"))["white","Std. Error"]
white_2 <- coeftest(approve_OLS, vocv = hccm(approve_OLS, type = "hc0"))["white","Estimate"]
CI_rub <- c(white_2-se_2*conf, white_2+se_2*conf)
matrix(c(CI_non, CI_rub),2,2)
# 2)
y_hat <- fitted(approve_OLS)
summary(y_hat)

# C8
help(gpa1)
# 1)
colGPA_OLS <- lm(colGPA~hsGPA+ACT+skipped+PC, gpa1)
summary(colGPA_OLS)
u <- residuals(colGPA_OLS)
# 2)
library(lmtest)
library(car)
bptest(colGPA_OLS, ~ fitted(colGPA_OLS) + I(fitted(colGPA_OLS)^2))
u2 <- residuals(colGPA_OLS)^2
log_u2 <- log(u2)
g_hat <- lm(log_u2~hsGPA+ACT+skipped+PC, gpa1)
h_hat <- exp(fitted(g_hat))
# 3)
summary(h_hat)
colGPA_WLS <- lm(colGPA~hsGPA+ACT+skipped+PC, weight = 1/h_hat, gpa1)
summary(colGPA_WLS)
summary(colGPA_OLS)$coef[4:5,]
summary(colGPA_WLS)$coef[4:5,]
# 4)
library(lmtest)
coeftest(colGPA_WLS, vcov = hccm(colGPA_WLS, type = "hc0"))

# C9
help(smoke)
# 1)
cigs_OLS <- lm(cigs~lincome+lcigpric+educ+age+agesq+restaurn, smoke)
summary(cigs_OLS)
cigs_OLS_hat <- fitted(cigs_OLS)
# 2)
u2 <- residuals(cigs_OLS)^2
log_u2 <- log(u2)
g_hat <- lm(log_u2~lincome+lcigpric+educ+age+agesq+restaurn, smoke)
h_hat <- exp(fitted(g_hat))
cigs_WLS <- lm(cigs~lincome+lcigpric+educ+age+agesq+restaurn, weight = 1/h_hat, smoke)
summary(cigs_WLS)
u_hat <- residuals(cigs_WLS)^2
y_hat <- fitted(cigs_WLS)
# 3)
library(lmtest)
library(car)
bptest(cigs_WLS, ~ fitted(cigs_WLS) + I(fitted(cigs_WLS)^2))

# C10
help(k401ksubs)
# 1)
e401k <- lm(e401k~inc+incsq+age+agesq+male,k401ksubs)
summary(e401k)
library(lmtest)
coeftest(e401k)
coeftest(e401k, vcov = hccm(e401k, type = "hc0"))
# 2)
library(lmtest)
library(car)
bptest(e401k, ~fitted(e401k) + I(fitted(e401k)^2))
y_hat <- fitted(e401k)
summary(y_hat)
h_hat <- y_hat * (1-y_hat)
e401k_WLS <- lm(e401k~inc+incsq+age+agesq+male, weight = 1/h_hat, k401ksubs)
summary(e401k_WLS)
# 3)
coeftest(e401k, vcov = hccm(e401k, type = "hc0"))
summary(e401k_WLS)$coef

# C11
help(k401ksubs)
# 1)
data <- subset(k401ksubs,fsize==1)
agesq0 <- (data$age-25)^2
nettfa_OLS <- lm(nettfa~inc+agesq0+male+e401k+e401k*inc,data)
summary(nettfa_OLS)
summary(nettfa_OLS)$coef["inc:e401k",]
# 2)
nettfa_WLS <- lm(nettfa~inc+agesq0+male+e401k+e401k*inc, weight = 1/inc, data)
summary(nettfa_WLS)
library(lmtest)
coeftest(nettfa_WLS, vcov = hccm(nettfa_WLS, type = "hc0"))
coeftest(nettfa_WLS, vcov = hccm(nettfa_WLS, type = "hc0"))["inc:e401k",]
# 3)
summary(nettfa_WLS)$coef["e401k",]
summary(nettfa_OLS)$coef["e401k",]
# 4)
inc_2 <- data$inc-30
nettfa_OLS2 <- lm(nettfa~inc+agesq0+male+e401k+e401k*inc_2,data)
summary(nettfa_OLS2)
nettfa_WLS2 <- lm(nettfa~inc+agesq0+male+e401k+e401k*inc_2, weight = 1/inc, data)
summary(nettfa_WLS2)
mean(data$inc)
# Δy/Δe401k = beta_4 + beta_5 * inc_2
beta_4 <- coef(nettfa_WLS2)[5]
beta_5 <- coef(nettfa_WLS2)[7]

# C12
help(meap00_01)
# 1)
math4_OLS <- lm(math4~lunch+lenroll+lexppp, meap00_01)
summary(math4_OLS)
library(lmtest)
library(car)
coeftest(math4_OLS)
coeftest(math4_OLS, vcov = hccm(math4_OLS, type = "hc0"))
# 2)
library(lmtest)
library(car)
bptest(math4_OLS, ~ fitted(math4_OLS) + I(fitted(math4_OLS)^2))
# 3)
u2 <- residuals(math4_OLS)^2
log_u2 <- log(u2)
g_hat <- lm(log_u2~lunch+lenroll+lexppp, meap00_01)
h_hat <- exp(fitted(g_hat))
math4_WLS <- lm(math4~lunch+lenroll+lexppp, weight = 1/h_hat, meap00_01)
summary(math4_WLS)
# 4)
library(lmtest)
library(car)
coeftest(math4_WLS)
coeftest(math4_WLS, vcov = hccm(math4_WLS, type = "hc0"))

# C13
help(fertil2)
# 1)
children <- lm(children~age+agesq+educ+electric+urban, fertil2)
summary(children)
library(lmtest)
library(car)
coeftest(children)
coeftest(children, vcov = hccm(children, type = "hc0"))
# 2)
children_2 <- lm(children~age+agesq+educ+electric+urban+spirit+protest+catholic, fertil2)
summary(children_2)
library(lmtest)
library(car)
coeftest(children_2)
coeftest(children_2, vcov = hccm(children_2, type = "hc0"))
# 3)
u2 <- residuals(children_2)^2
y_hat <- fitted(children_2)
u2 <- lm(u2~y_hat+y_hat^2, fertil2)
summary(u2)
library(lmtest)
library(car)
bptest(u2, ~ fitted(u2) + I(fitted(u2)^2))

# C14
help(beauty)
# 1)
lwage <- lm(lwage~belavg+abvavg+female+educ+exper+expersq, beauty)
summary(lwage)
library(lmtest)
library(car)
coeftest(lwage)
coeftest(lwage, vcov = hccm(lwage, type = "hc0"))
# 2)
lwage2 <- lm(lwage~belavg+abvavg+female+educ+exper+expersq+
               belavg*female+abvavg*female+educ*female+exper*female+expersq*female, beauty)
summary(lwage2)
SSR_r <- sum(residuals(lwage)^2)
SSR_ur <- sum(residuals(lwage2)^2)
q <- 5
df1 <- summary(lwage)$fstatistic[3]
df2 <- summary(lwage2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 
F
library(lmtest)
library(car)
coeftest(lwage2)
coeftest(lwage2, vcov = hccm(lwage2, type = "hc0"))
# 3)
lwage3 <- lm(lwage~belavg+abvavg+female+educ+exper+expersq+
               educ*female+exper*female+expersq*female, beauty)
summary(lwage3)
SSR_r <- sum(residuals(lwage2)^2)
SSR_ur <- sum(residuals(lwage3)^2)
q <- 2
df1 <- summary(lwage2)$fstatistic[3]
df2 <- summary(lwage3)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # FALSE, fail to reject H0
F



