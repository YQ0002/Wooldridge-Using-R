# Chapter 6 
rm(list=ls())
library(wooldridge)

# 6.1
help(bwght)
bwght1 <- lm(bwght~cigs+faminc,bwght)
summary(bwght1)
length(bwght$faminc)
R_2 <- summary(bwght1)$r.squared
SSR_bwght1 <- sum(residuals(bwght1)^2)
SER_bwght1 <- summary(bwght1)$sigma
bwghtlbs <- lm(bwghtlbs~cigs+faminc,bwght)
summary(bwghtlbs)
list(coef(bwght1),coef(bwghtlbs))
coef(bwghtlbs)*16-coef(bwght1)

# Going Further 6.1
fincdol <- bwght$faminc * 1000
bwght2 <- lm(bwght~cigs+fincdol,bwght)
summary(bwght2)
list(coef(bwght1),coef(bwght2))

# Beta Coefficients
# Example 6.1 
help(hprice2)
price <- lm(price~nox+crime+rooms+dist+stratio,hprice2)
summary(price)
zprice <- with(hprice2,(price-mean(price))/sd(price))
znox <- with(hprice2,(nox-mean(nox))/sd(nox))
zcrime <- with(hprice2,(crime-mean(crime))/sd(crime))
zrooms <- with(hprice2,(rooms-mean(rooms))/sd(rooms))
zdist <- with(hprice2,(dist-mean(dist))/sd(dist))
zstratio <- with(hprice2,(stratio-mean(stratio))/sd(stratio))
data <- cbind.data.frame(zprice,znox,zcrime,zrooms,zdist,zstratio)
zprice <- lm(zprice~znox+zcrime+zrooms+zdist+zstratio,data)
summary(zprice)

# Example 6.2 Logarithmic Functional Forms
help(hprice2)
lprice <- lm(lprice~lnox+rooms,hprice2)
summary(lprice)
beta_2 <- coef(lprice)['rooms']
delta_y <- 100*(exp(beta_2)-1)
delta_y


# 6-2a More on Using Logarithmic Functional Forms
help(hprice2)
lprice <- lm(lprice~lnox+rooms,hprice2)
summary(lprice)
# %Δy_hat = 100*(exp(beta_2_hat * Δx2) - 1)
beta_2 <- coef(lprice)[3]
exp(beta_2)-1 # when Δx2 = 1
exp(-beta_2)-1 # when Δx2 = -1

# 6.2b Models with Quadratics
help(wage1)
wage <- lm(wage~exper+expersq,wage1)
summary(wage)
beta_1 <- coef(wage)['exper']
beta_2 <- coef(wage)['expersq']
# Δy = beta_1 + 2 * beta_2 * x
beta_1 + 2 * beta_2 * 1
beta_1 + 2 * beta_2 * 10
# the turning point 
abs(beta_1 / (2 * beta_2))
hist(wage1$wage)

# Example 6.2 
help(hprice2)
ldist <- log(hprice2$dist)
rooms2 <- hprice2$rooms^2
lprice <- lm(lprice~lnox+ldist+rooms+rooms2+stratio,hprice2)
summary(lprice)
coef(lprice)[5]
# turning point of rooms2
beta_3 <- coef(lprice)['rooms']
beta_4 <- coef(lprice)['rooms2']
abs(beta_3 / (2 * beta_4))
hist(hprice2$rooms)
# %Δprice = 100*(beta_3 + 2*beta_4 * rooms) * Δrooms
100*(beta_3 + 2*beta_4 * 5) * 1 # rooms from 5 to 6
100*(beta_3 + 2*beta_4 * 6) * 1 # rooms from 6 to 7

# Example 6.3 Models with Interaction Terms
# y = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2 + u
# sigma_1 = beta_1 + beta_3 * μ2
# sigma_2 = beta_2 + beta_3 * μ1
# y = a0 + sigma_1 * x1 + sigma_2 * x2 + beta_3 * (x1-μ1) * (x2-μ2) + u

help(attend)
priGPA2 <- attend$priGPA^2
ACT2 <- attend$ACT^2
stndfnl <- lm(stndfnl~atndrte+priGPA+ACT+priGPA2+ACT2+priGPA*atndrte,attend)
summary(stndfnl)
# Δstndfnl/Δatnndrte = beta_1 + beta_6 * priGPA
priGPA_avg <- mean(attend$priGPA)
beta_1 <- coef(stndfnl)['atndrte']
beta_6 <- coef(stndfnl)['atndrte:priGPA']
# 1% raises on atndrte increases stndfnl by 0.0077 sd
beta_1 + beta_6 * priGPA_avg
priGPA_avg
priGPA_de <- attend$priGPA - priGPA_avg
stndfnl2 <- lm(stndfnl~atndrte+priGPA+ACT+priGPA2+ACT2+priGPA_de*atndrte,attend)
summary(stndfnl2)
coef(stndfnl2)['atndrte']
summary(stndfnl2)$coef[2,2] # SE
summary(stndfnl2)$coef[2,3] # T value

# Average Partial Effects (APE)
# APE_stndfnl = beta_1 + beta_6 * mean(priGPA)
beta_1 + beta_6 * mean(attend$priGPA)
# APE_priGPA = beta_2 + 2 * beta_4 * mean(atndrte)
coef(stndfnl)
beta_2 <- coef(stndfnl)['priGPA']
beta_4 <- coef(stndfnl)['priGPA2']
beta_2 + 2 * beta_4 * mean(attend$atndrte)

# 6-3a Adjusted R-Squared
# Adjusted R-Squared increases if, and only if, the t statistic on the new variable is greater than one in absolute value.
help(mlb1)
lsalary1 <- lm(lsalary~years+gamesyr+bavg+hrunsyr,mlb1)
lsalary2 <- lm(lsalary~years+gamesyr+bavg+rbisyr,mlb1)
summary(lsalary1)$adj.r.squared
summary(lsalary2)$adj.r.squared

# 6-3b Using Adjusted R-Squared to Choose between Nonnested Models
help(mlb1)
# nonnested models
lsalary1 <- lm(lsalary~years+gamesyr+bavg+hrunsyr,mlb1)
lsalary2 <- lm(lsalary~years+gamesyr+bavg+rbisyr,mlb1)
summary(lsalary1)$r.squared
summary(lsalary2)$r.squared

help(rdchem)
rdintens1 <- lm(rdintens~lsales,rdchem)
rdintens2 <- lm(rdintens~sales+salessq,rdchem)
summary(rdintens1)$r.squared
summary(rdintens2)$r.squared
summary(rdintens1)$adj.r.squared
summary(rdintens2)$adj.r.squared

# Example 6.4 
help(ceosal1)
salary <- lm(salary~sales+roe,ceosal1)
summary(salary)
lsalary <- lm(lsalary~lsales+roe,ceosal1)
summary(lsalary)
summary(salary)$r.squared
summary(lsalary)$r.squared
summary(salary)$adj.r.squared
summary(lsalary)$adj.r.squared

# 6-4 Prediction and Residual Analysis
# 6.4a Confidence Intervals for Predictions

# Example 6.5 Confidence Interval for Predicted College GPA
help(gpa2)
colgpa <- lm(colgpa~sat+hsperc+hsize+hsizesq,gpa2)
summary(colgpa)
sample <- data.frame(sat=1200,hsperc=30,hsize=5,hsizesq=25)
predict(colgpa,sample)
# obtain a confidence interval
sat0 <- gpa2$sat-1200
hsperc0 <- gpa2$hsperc-30
hsize0 <- gpa2$hsize-5
hsizesq0 <- gpa2$hsizesq-25
colgpa0 <- lm(colgpa~sat0+hsperc0+hsize0+hsizesq0,gpa2)
summary(colgpa0)
gpa <- summary(colgpa0)$coef[1,1]
se <- summary(colgpa0)$coef[1,2]
conf <- qnorm(0.975)
c(gpa-se*conf,gpa+se*conf)

# Example 6.6 Confidence Interval for Future College GPA
help(gpa2)
summary(colgpa0)
se_y <- summary(colgpa0)$coef[1,2]
sigma <- sigma(colgpa0)
se_e <- sqrt(se_y^2+sigma^2)
se_y
sigma
se_e
c(gpa-se_e*conf,gpa+se_e*conf)

# 6-4b Residual Analysis
help(hprice1)
price <- lm(price~lotsize+sqrft+bdrms,hprice1)
summary(price)
x <- residuals(price)
summary(x)

# 6-4c Predicting y When log(y) Is the Dependent Variable
# y = exp(σ^2/2) * exp(logy)
# Example 6.7  Predicting CEO Salaries
help(ceosal2)
lsalary <- lm(lsalary~lsales+lmktval+ceoten,ceosal2)
summary(lsalary)
# 1) a0 = n^(-1) * Σexp(u_i)
u_i <- residuals(lsalary)
a0_1 <- mean(exp(u_i))
a0_1 # 1.135661
# 2) a0 = (Σ(m_i^2))^(-1) * (Σ(m_i*y_i))
logy_i <- fitted.values(lsalary)
m_i <- exp(logy_i)
y <- lm(salary~0+m_i,ceosal2)
summary(y)
a0_2 <- coef(y)
a0_2 # 1.116857 
y_i <- ceosal2$salary
sum(m_i^2)^(-1) * sum(m_i*y_i)
# Predict
sample <- data.frame(lsales = log(5000),lmktval = log(10000),ceoten = 10)
x <- predict(lsalary,sample)
est1 <- exp(x) * 1000
est2 <- exp(x) * a0_1 * 1000
est3 <- exp(x) * a0_2 * 1000
est <- c(est1,est2,est3)
est

# Example 6.8  
salary <- lm(salary~sales+mktval+ceoten,ceosal2)
summary(salary)$r.squared
summary(lsalary)$r.squared

lsales0 <- ceosal2$lsales-log(5000)
lmktval0 <- ceosal2$lmktval-log(10000)
ceoten0 <- ceosal2$ceoten-10
lsalary0 <- lm(lsalary~lsales0+lmktval0+ceoten0,ceosal2)
se_y <- summary(lsalary0)$coef[1,2]
se_y
sigma <- sigma(lsalary0)
sigma
se_e <- sqrt(se_y^2+sigma^2)
se_e
ceosal <- summary(lsalary0)$coef[1,1]
ceosal
conf <- qnorm(0.975)
exp(-conf * se_e) * exp(ceosal)
exp(conf * se_e) * exp(ceosal)


# Problems
# 1
rm(list=ls())
library(wooldridge)
help(ceosal1)
sqroe <- ceosal1$roe^2
lsalary <- lm(lsalary~lsales+roe+sqroe,ceosal1)
summary(lsalary)
coef(lsalary)['sqroe']

# 3
help(rdchem)
rdintens <- lm(rdintens~sales+salessq,rdchem)
summary(rdintens)
# 1) the turning point
coef(rdintens)
beta_1 <- coef(rdintens)['sales']
beta_2 <- coef(rdintens)['salessq']
abs(beta_1/beta_2)
# 3)
salesbil <- rdchem$sales/1000
salesbilsq <- salesbil^2
rdintens2 <- lm(rdintens~salesbil+salesbilsq,rdchem)
summary(rdintens2)
coef(rdintens)
coef(rdintens2)

# 4
# 1)
# log(wage) = beta_0 + beta_1 * educ + beta_2 * educ*pareduc + beta_3 * exper + beta_4 * tenure + u
# Δlwage/Δeduc = beta_1 + beta_2 * pareduc
# 2)
help(wage2)
pareduc <- with(wage2,meduc+feduc)
x2 <- wage2$educ*pareduc
lwage <- lm(lwage~educ+x2+exper+tenure,wage2)
summary(lwage)
beta_2 <- coef(lwage)['x2']
beta_2 * (32-24)
# 3) H0: beta_2 = 0 
lwage2 <- lm(lwage~educ+pareduc+educ*pareduc+exper+tenure,wage2)
summary(lwage2)
p_edu_pedu <- summary(lwage2)$coef[6,4]
p_edu_pedu > 0.05 # TRUE, H0 rejected

# 5
help(meap93)
math1 <- lm(math10~totcomp+staff+enroll,meap93)
summary(math1)$r.squared
math2 <- lm(math10~totcomp+staff+enroll+sci11,meap93)
summary(math2)$r.squared

# 6
help(attend)
priGPA2 <- attend$priGPA^2
ACT2 <- attend$ACT^2
stndfnl <- lm(stndfnl~atndrte+priGPA+ACT+priGPA2+ACT2+priGPA*atndrte,attend)
R2_1 <- summary(stndfnl)$r.squared
# Add atndrte2 and ACT*atndrte in lm(stndfnl)
atndrte2 <- attend$atndrte^2
stndfnl2 <- lm(stndfnl~atndrte+priGPA+ACT+priGPA2+ACT2+atndrte2+priGPA*atndrte+ACT*atndrte,attend)
R2_2 <-summary(stndfnl2)$r.squared
c(R2_1,R2_2)
SSR_r <- sum(residuals(stndfnl)^2)
SSR_ur <- sum(residuals(stndfnl2)^2)
q <- 2
df1 <- summary(stndfnl)$fstatistic[3]
df2 <- summary(stndfnl2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.95,df1,df2) # True, H0 rejected 

# 7
help(k401k)
prate1 <- lm(prate~mrate+age+totemp,k401k)
summary(prate1)$adj.r.squared
prate2 <- lm(prate~mrate+age+ltotemp,k401k)
summary(prate2)$adj.r.squared
totempsq <- k401k$totemp^2
prate3 <- lm(prate~mrate+age+totemp+totempsq,k401k)
summary(prate3)$adj.r.squared

# 10
# 1)
help(meapsingle)
math1 <- lm(math4~lexppp+free+lmedinc+pctsgle,meapsingle)
summary(math1)
math2 <- lm(math4~lexppp+free+lmedinc+pctsgle+read4,meapsingle)
summary(math2)
coef(math1)['lexppp']
coef(math2)['lexppp']
# 10% increase in expenditures per student
10 * coef(math1)['lexppp'] / 100
# 2) Beta Coefficients
zmath4 <- with(meapsingle,(math4-mean(math4))/sd(math4))
zlexppp <- with(meapsingle,(lexppp-mean(lexppp))/sd(lexppp))
zfree <- with(meapsingle,(free-mean(free))/sd(free))
zlmedinc <- with(meapsingle,(lmedinc-mean(lmedinc))/sd(lmedinc))
zpctsgle <- with(meapsingle,(pctsgle-mean(pctsgle))/sd(pctsgle))
zread4 <- with(meapsingle,(read4-mean(read4))/sd(read4))
zmath1 <- lm(zmath4~zlexppp+zfree+zlmedinc+zpctsgle)
zmath2 <- lm(zmath4~zlexppp+zfree+zlmedinc+zpctsgle+zread4)
summary(zmath1)
summary(zmath2)
coef(zmath1)
coef(zmath2)

# Computer Exercises
library(wooldridge)

# C1
help(kielmc)
data <- subset(kielmc,y81==1)
dim(kielmc)
dim(data)
# 1) 
lprice <- lm(lrprice~ldist,data)
summary(lprice)
# 2) 
lprice2 <- lm(lrprice~ldist+lintst+larea+lland+rooms+baths+age,data)
summary(lprice2)
coef(lprice)
coef(lprice2)
# 3) 
lprice3 <- lm(lrprice~ldist+lintst+larea+lland+rooms+baths+age+lintstsq,data)
summary(lprice3)
beta_2 <- coef(lprice3)[3]
beta_8 <- coef(lprice3)[9]
# turning point of lintst
x <- abs(beta_2 / (2 * beta_8))
x
exp(x)
# 4) 
ldistsq <- with(data,ldist^2)
lprice4 <- lm(lrprice~ldist+lintst+larea+lland+rooms+baths+age+lintstsq+ldistsq,data)
summary(lprice4)
t <- summary(lprice4)$coef[10,3]
abs(t) > qnorm(0.975) # FALSE

# C2
# 1) 
help(wage1)
lwage <- lm(lwage~educ+exper+expersq,wage1)
summary(lwage)
# 2) 
t_expersq <- summary(lwage)$coef[4,3]
abs(t_expersq) > qnorm(0.995) # TRUE, H0 rejected
# 3) % Δlwage/Δexper = 100*(beta_2 + 2*beta_3 * exper)
beta_2 <- coef(lwage)[3]
beta_3 <- coef(lwage)[4]
100*(beta_2 + 2*beta_3 * 4) * 1 # the fifth year return
100*(beta_2 + 2*beta_3 * 19) * 1 # the twentieth year return
# 4) the turning point
abs(beta_2/(2*beta_3))
hist(wage1$exper)

# C3
# lwage = beta_0 + beta_1 * educ + beta_2 * exper + beta_3 * educ * exper + u
# 1) Δlwage/Δeduc = beta_1 + beta_3 * exper
# 2) H0: beta_3 = 0, H1: beta_3 > 0
# 3) 
help(wage2)
lwage <- lm(lwage~educ+exper+educ*exper,wage2)
summary(lwage)
p_educ_exper <- summary(lwage)$coef[4,4]
p_educ_exper < 0.05
# 4) 
beta_1 <- coef(lwage)['educ']
beta_3 <- coef(lwage)['educ:exper']
coef(lwage)
# Theta <- beta_1 + 3*beta_3
exper0 <- wage2$exper - 10
lwage0 <- lm(lwage~educ+exper0+educ*exper0,wage2)
summary(lwage0)
educ <- summary(lwage0)$coef[1,1]
se <- summary(lwage0)$coef[1,2]
conf <- qnorm(0.975)
c(educ-se*conf,educ+se*conf)

# C4
# 1)
help(gpa2)
sat <- lm(sat~hsize+hsizesq,gpa2)
summary(sat)
# 2) turning point
beta_1 <- coef(sat)['hsize']
beta_2 <- coef(sat)['hsizesq']
a <- abs(beta_1 / (2 * beta_2))
a
hist(gpa2$hsize)
# 4) 
lsat <- log(gpa2$sat)
lsat <- lm(lsat~hsize+hsizesq,gpa2)
summary(lsat)
beta_1 <- coef(lsat)['hsize']
beta_2 <- coef(lsat)['hsizesq']
b <- abs(beta_1 / (2 * beta_2))
b

# C5
help(hprice1)
# 1) 
lprice <- lm(lprice~llotsize+lsqrft+bdrms,hprice1)
summary(lprice)
# 2) 
sample <- data.frame(llotsize=log(20000),lsqrft=log(2500),bdrms=4)
predict(lprice,sample)
# confidence interval
llotsize0 <- hprice1$lotsize-log(20000)
lsqrft0 <- hprice1$lsqrft-log(2500)
bdrms0 <- hprice1$bdrms-4
lprice0 <- lm(lprice~llotsize0+lsqrft0+bdrms0,hprice1)
summary(lprice0)
lprice <- summary(lprice0)$coef[1,1]
se <- summary(lprice0)$coef[1,2]
conf <- qnorm(0.975)
c(lprice-se*conf,lprice+se*conf)
# 3) 
price <- lm(price~lotsize+sqrft+bdrms,hprice1)
summary(price)
summary(lprice)$r.squared
summary(price)$r.squared
summary(lprice)$adj.r.squared
summary(price)$adj.r.squared

# C6
# 1) 
help(vote1)
voteA <- lm(voteA~prtystrA+expendA+expendB+expendA*expendB,vote1)
summary(voteA)
# ΔvoteA/ΔexpendB = beta_3 + beta_4 * expendA
# ΔvoteA/ΔexpendA = beta_2 + beta_4 * expendB
# 2)
beta_4 <- coef(voteA)['expendA:expendB']
p_beta_4 <- summary(voteA)$coef[5,4]
p_beta_4 > 0.05
# 3)
mean(vote1$expendA)
beta_2 <- coef(voteA)['expendA']
beta_3 <- coef(voteA)['expendB']
(beta_3 + beta_4 * 300) * 100
# 4)
(beta_2 + beta_4 * 100) * 100
# 5)
voteA2 <- lm(voteA~prtystrA+expendA+expendB+shareA,vote1)
summary(voteA2)
summary(voteA)$r.squared
summary(voteA2)$r.squared
summary(voteA)$adj.r.squared
summary(voteA2)$adj.r.squared
# 6)
# ΔvoteA2/ΔexpendB = beta_3 + beta_4 * ΔshareA/ΔexpendB
# ΔshareA/ΔexpendB = 100*(expendA/(expendA+expendB)^2)
100*(300/(300+0)^2)

# C7
# 1)
help(attend)
priGPA2 <- attend$priGPA^2
ACT2 <- attend$ACT^2
stndfnl <- lm(stndfnl~atndrte+priGPA+ACT+priGPA2+ACT2+priGPA*atndrte,attend)
summary(stndfnl)
# 2) Δstndfnl/ΔpriGPA = beta_2 + 2 * beta_4 * priGPA + beta_6 * atndrte
beta_2 <- coef(stndfnl)['priGPA']
beta_4 <- coef(stndfnl)['priGPA2']
beta_6 <- coef(stndfnl)['atndrte:priGPA']
beta_2 + 2 * beta_4 * 2.59 + beta_6 * 82
# 3) 
priGPA20 <- (attend$priGPA - 2.59)^2
atndrte0 <- attend$atndrte - 82
stndfnl0 <- lm(stndfnl~atndrte+priGPA+ACT+priGPA20+ACT2+priGPA*atndrte0,attend)
summary(stndfnl0)
coef(stndfnl0)

# C8
# 1)
help(hprice1)
price <- lm(price~lotsize+sqrft+bdrms,hprice1)
summary(price)
sample <- data.frame(lotsize=10000,sqrft=2300,bdrms=4)
x <- predict(price,sample)
round(x)
# 2) 95% confidence interval
lotsize0 <- hprice1$lotsize - 10000
sqrft0 <- hprice1$sqrft - 2300
bdrms0 <- hprice1$bdrms - 4
price0 <- lm(price~lotsize0+sqrft0+bdrms0,hprice1)
summary(price0)
price <- summary(price0)$coef[1,1]
se <- summary(price0)$coef[1,2]
conf <- qnorm(0.975)
c(price-se*conf,price+se*conf)
# 3) 95% CI for price0
se_y <- summary(price0)$coef[1,2]
sigma <- sigma(price0)
se_e <- sqrt(se_y^2+sigma^2)
se_y
sigma
se_e
c(price-se_e*conf,price+se_e*conf)

# C9
# 1)
help(nbasal)
points <- lm(points~exper+age+coll+expersq,nbasal)
summary(points)
# 2) Δpoints/Δexper = beta_1 + 2 * beta_4 * exper
beta_1 <- coef(points)['exper']
beta_4 <- coef(points)['expersq']
beta_1 + 2 * beta_4
# 4) 
points2 <- lm(points~exper+age+coll+expersq+agesq,nbasal)
summary(points2)
coef(points2)['agesq']
# 5) 
lwage <- lm(lwage~points+exper+expersq+age+coll,nbasal)
summary(lwage)
# 6) H0: age and coll are jointly significant
lwage2 <- lm(lwage~points+exper+expersq,nbasal)
summary(lwage2)
SSR_r <- sum(residuals(lwage2)^2)
SSR_ur <- sum(residuals(lwage)^2)
q <- 2
df1 <- summary(lwage2)$fstatistic[3]
df2 <- summary(lwage)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # FALSE, fail to reject H0

# C10
# 1)
help(bwght2)
lbwght <- lm(lbwght~npvis+npvissq,bwght2)
summary(lbwght)
# 2)
coef(lbwght)
beta_1 <- coef(lbwght)['npvis']
beta_2 <- coef(lbwght)['npvissq']
# Δlbwght/Δnpvis = beta_1 + 2 * beta_2 * npvis
abs(beta_1 / (2*beta_2))
hist(bwght2$npvis)
# 4) 
lbwght2 <- lm(lbwght~npvis+npvissq+mage+magesq,bwght2)
summary(lbwght2)
# Δlbwght/Δmage = beta_3 + 2 * beta_4 * mage
coef(lbwght2)
beta_2 <- coef(lbwght2)['mage']
beta_4 <- coef(lbwght2)['magesq']
abs(beta_3 / (2*beta_4))
hist(bwght2$mage)
# 5) 
summary(lbwght)$r.squared
summary(lbwght2)$r.squared
summary(lbwght)$adj.r.squared
summary(lbwght2)$adj.r.squared
# 6) 
lbwght3 <- lm(bwght~npvis+npvissq+mage+magesq,bwght2)
summary(lbwght3)
summary(lbwght2)$r.squared
summary(lbwght3)$r.squared

# C11
# 1)
help(apple)
ecolbs <- lm(ecolbs~ecoprc+regprc,apple)
summary(ecolbs)
# 2)
summary(ecolbs)$coef
# 3)
y <- coef(ecolbs)[1]
y
se <- summary(ecolbs)$coef[1,2]
se
c(y-2*se,y+2*se)
# 4)
summary(ecolbs)$r.squared
# 5)
ecolbs2 <- lm(ecolbs~ecoprc+regprc+faminc+hhsize+educ+age,apple)
summary(ecolbs2)
summary(ecolbs2)$fstatistic
# 6)
ecolbs3 <- lm(ecolbs~ecoprc,apple)
ecolbs4 <- lm(ecolbs~regprc,apple)
coef(ecolbs)
coef(ecolbs3)
coef(ecolbs4)
with(apple,cor(ecoprc,regprc))
# 7)
ecolbs5 <- lm(ecolbs~ecoprc+regprc+faminc+reglbs,apple)
summary(ecolbs5)
summary(ecolbs)$r.squared
summary(ecolbs5)$r.squared
summary(ecolbs)$adj.r.squared
summary(ecolbs5)$adj.r.squared

# C12
# 1)
help(k401ksubs)
data <- subset(k401ksubs,fsize==1)
dim(data)
table(data$age)
# 2)
nettfa <- lm(nettfa~inc+age+agesq,data)
summary(nettfa)
beta_2 <- coef(nettfa)['age']
beta_3 <- coef(nettfa)['agesq']
beta_2 + 2 * beta_3
hist(data$age)
# 3)
beta_2 / (2 * beta_3)
# 4)
agesq0 <- (data$age-25)^2
nettfa0 <- lm(nettfa~inc+age+agesq0,data)
summary(nettfa0)
coef(nettfa0)['age']
# 5)
nettfa02 <- lm(nettfa~inc+agesq0,data)
summary(nettfa02)
summary(nettfa0)$adj.r.squared
summary(nettfa02)$adj.r.squared
# 6)
inc03 <- data$inc-30
nettfa03 <- lm(nettfa~inc03+agesq0,data)
summary(nettfa03)
beta_2 <- coef(nettfa03)['inc03']
beta_3 <- coef(nettfa03)['agesq0']
beta_2 / (2 * beta_3)
# 7)
incsq <- data$inc^2
nettfa04 <- lm(nettfa~inc+incsq+agesq0,data)
summary(nettfa04)
