# Chapter 4 
library(wooldridge)

# Example 4.1
help(wage1)
logwage <- lm(lwage~educ+exper+tenure,wage1)
summary(logwage)
c(qnorm(0.95),qnorm(0.98))

# Example 4.2
help(meap93)
math1 <- lm(math10~totcomp+staff+enroll,meap93)
summary(math1)
math2 <- lm(math10~ltotcomp+lstaff+lenroll,meap93)
summary(math2)

# Example 4.3
help(gpa1)
colgpa <- lm(colGPA~hsGPA+ACT+skipped,gpa1)
summary(colgpa)

# Example 4.5
help(hprice2)
ldist <- exp(hprice2$dist)
logprice <- lm(lprice~lnox+ldist+rooms+stratio,hprice2)
summary(logprice)

# Example 4.6
help(k401k)
prate <- lm(prate~mrate+age+totemp,k401k)
summary(prate)

# Example 4.7
help(jtrain)
data1 <- subset(jtrain,year==1987&union==0)
logscrap1 <- lm(lscrap~hrsemp+lsales+lemploy,data1)
summary(logscrap1)

# Example 4.8
help(rdchem)
logrd <- lm(lrd~lsales+profmarg,rdchem)
n <- length(rdchem$lsales)
k <- 2
df <- n-k-1
summary(logrd)
beta_1 <- coef(logrd)[2]
se_1 <- summary(logrd)$coef[2,2]
t_95 <- qt(0.975,df)
# the 95% confidence interval
c(beta_1-se_1*t_95,beta_1+se_1*t_95)

# 4-5 The F test
help(mlb1)
# Unrestricted Model
lsalary1 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr,mlb1)
n <- length(mlb1$salary)
k <- 5 # Variables 
SSR_ur <- sum(residuals(lsalary1)^2)
R2_ur <- summary(lsalary1)$r.squared
# Restricted Model
lsalary2 <- lm(lsalary~years+gamesyr,mlb1)
SSR_r <- sum(residuals(lsalary2)^2)
R2_r <- summary(lsalary2)$r.squared
# F statistic
q <- 3 # Variables ignored in restricted model
df <- n-k-1 # Degrees of freedom
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
c1 <- qf(0.95,q,df)
F>c1 # TRUE, null hypothesis rejected
c2 <- qf(0.99,q,df)
F>c2 # TRUE, null hypothesis rejected
# also, F = ((R2_ur-R2_r)/q)/((1-R2_ur)/df_ur)

# Example 4.9
help(bwght)
library(tidyr)
data <- drop_na(bwght)
bwght1 <- lm(bwght~cigs+parity+faminc+motheduc+fatheduc,data)
n <- length(data$faminc)
k <- 5
df_ur <- n-k-1
R2_ur <- summary(bwght1)$r.squared
bwght2 <- lm(bwght~cigs+parity+faminc,data)
q <- 2
R2_r <- summary(bwght2)$r.squared
F <- ((R2_ur-R2_r)/q)/((1-R2_ur)/df_ur)
c <- qf(0.95,q,df)
F>c # FALSE, fail to reject H0

# Example 4.10
help(meap93)
# totcomp = salary + benefits = salary * (1 + benefits/salary)
bs <- with(meap93,benefits/salary)
lsalary1 <- lm(lsalary~bs,meap93)
summary(lsalary1)$coef
lsalary2 <- lm(lsalary~bs+lenroll+lstaff,meap93)
summary(lsalary2)$coef
lsalary3 <- lm(lsalary~bs+lenroll+lstaff+droprate+gradrate,meap93)
summary(lsalary3)$coef

# Problems
# 2
help(ceosal1)
# 2) lsalary = beta_0 + beta_1 * lsales + beta_2 * roe + beta_3 * ros + u
lsalary <- lm(lsalary~lsales+roe+ros,ceosal1)
summary(lsalary)
50 * coef(lsalary)['ros']
# 3) H0: ros has no effect on salary at the 10% significance level
summary(lsalary)
t_ros <- summary(lsalary)$coef[4,3]
t_test <- qnorm(0.9)
t_ros > t_test # False, fail to reject H0

# 3
help(rdchem)
# 1) rdintens = beta_0 + beta_1 * lsales + beta_2 * profmarg + u
rdintens <- lm(rdintens~lsales+profmarg,rdchem)
summary(rdintens)
a <- predict(rdintens,data.frame(lsales=1,profmarg=1))
b <- predict(rdintens,data.frame(lsales=1.1,profmarg=1))
1-a/b
# 2) H0: R&D intensity does not change with sales at the 5% and 10% levels
t_sales <- summary(rdintens)$coef[2,3]
t_test1 <- qnorm(0.95)
t_test2 <- qnorm(0.9)
t_sales > t_test1 #FALSE, fail to reject H0
t_sales > t_test2 #TRUE, H0 rejected

# 4
help(rental)
data <- subset(rental,year==90)
dim(rental)
dim(data)
lrent <- lm(lrent~lpop+lavginc+pctstu,data)
summary(lrent)
# 4) H0: pctstu has no effect on rents at the 1% levels
t_pctstu <- summary(lrent)$coef[4,3]
t_test <- qnorm(0.99)
t_pctstu > t_test #TRUE, H0 rejected 

# 5
help(gpa1)
# 1) The 95% confidence interval
colgpa <- lm(colGPA~hsGPA+ACT+skipped,gpa1)
summary(colgpa)
beta_1 <- coef(colgpa)[2]
se_hsGPA <- summary(colgpa)$coef[2,2]
df <- summary(colgpa)$fstatistic[3]
t_95 <- qnorm(0.975)
c(beta_1 - se_hsGPA*t_95,beta_1 + se_hsGPA*t_95)

# 6
help(hprice1)
# price = beta_0 + beta_1 * assess + u
price <- lm(price~assess,hprice1)
n <- length(hprice1$price)
SSR_ur <- sum(residuals(price)^2)
beta_0 <- coef(colgpa)[2]
# 1-1) H0: beta_0 = 0 against the two-sided alternative
k <- 1
df = n-k-1
t_int <- summary(price)$coef[1,3]
t_test <- qnorm(0.975)
t_int > t_test1 #FALSE, fail to reject H0
# 1-2) H0: beta_1 = 1 against the two-sided alternative
beta_1 <- coef(price)['assess']
se_assess <- summary(price)$coef[2,2]
t_assess <- (beta_1 - 1)/se_assess
t_assess > t_test1  #TRUE, H0 rejected 
# 2) F test for joint hypothesis: beta_0 = 0 and beta_1 = 1
q <- 2
SSR_r <- with(hprice1,sum((price-assess)^2))
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
F_test <- qf(0.95,k,df)
F > F_test #TRUE, H0 rejected 
# price2 = beta_0 + beta_1 * assess + beta_2 * lotsize  + beta_3 * sqrft  + beta_4 * bdrms  + u
price2 <- lm(price~assess+lotsize+sqrft+bdrms,hprice1)
summary(price)
# 3) F Test for H0: beta_2 = 0, beta_3 = 0, beta_4 = 0
n <- length(hprice1$price)
k <- 4
df = n-k-1
q <- 3
SSR_r <- sum(residuals(price2)^2)
SSR_ur <- sum(residuals(price)^2)
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
F > F_test #FALSE, fail to reject H0

# 7
help(jtrain)
# 1) Compare new model with Example 4.7
data2 <- subset(jtrain,year==1987) #without union==0
logscrap2 <- lm(lscrap~hrsemp+lsales+lemploy,data2)
summary(logscrap1) 
summary(logscrap2)
# 2) logscrap3 = beta_0 + beta_1 * hrsemp + beta_2 * log(sales/employ) + beta_3 * lemploy + u
lsalemp <- with(data2,log(sales/employ))
logscrap3 <- lm(lscrap~hrsemp+lsalemp+lemploy,data2)
summary(logscrap3)
# 3) H0: beta_3 = 0
t_lemploy <- summary(logscrap3)$coef[2,3]
t_test <- qnorm(0.025)
t_lemploy < t_test # TRUE, H0 rejected
# 4) H0: beta_2 = -1
beta_2 <- summary(logscrap3)$coef[3,1]
se_lsalemp <- summary(logscrap3)$coef[3,2]
t_lsalemp <- (beta_2 + 1)/se_lsalemp
t_test <- qnorm(0.99)
t_lsalemp > t_test # FALSE, fail to reject H0

# 9
help(sleep75)
sleep <- lm(sleep~totwrk+educ+age,sleep75)
summary(sleep)
# 1) T test for educ and age with sales at the 5% level against a two-side laternative
t_educ <- summary(sleep)$coef[3,3]
t_age <- summary(sleep)$coef[4,3]
t_test1 <- qnorm(0.025)
t_educ < t_test1 # FALSE, fail to reject H0
t_test2 <- qnorm(0.975)
t_age > t_test2 # FALSE, fail to reject H0
# sleep2 = beta_0 + beta_1 * totwrk + u
# 2) H0: beta_2 = 0 and beta_3 = 0,
sleep2 <- lm(sleep~totwrk,sleep75)
SSR_r <- sum(residuals(sleep2)^2)
SSR_ur <- sum(residuals(sleep)^2)
n <- length(sleep75$age)
k <- 3
df = n-k-1
q <- 2
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
F_test <- qf(0.95,q,df)
F > F_test # TRUE, H0 rejected
# 3) 
summary(sleep)$r.squared
summary(sleep2)$r.squared

# 10
library(wooldridge)
help(return)
# return = beta_0 + beta_1 * dkr + beta_2 * eps + beta_3 * netinc + + beta_4 * salary + u
return <- lm(return~dkr+eps+netinc+salary,return)
summary(return)
# 1) Jointly significant at the 5% level
q <- summary(return)$fstatistic[2]
df <- summary(return)$fstatistic[3]
F_return <- summary(return)$fstatistic[1]
F_test <- df(0.975,q,df)
F_return > F_test # TRUE, H0 rejected 
# 2) return2 = beta_0 + beta_1 * dkr + beta_2 * eps + beta_3 * lnetinc + + beta_4 * lsalary + u
return2 <- lm(return~dkr+eps+lnetinc+lsalary,return)
summary(return2)
q <- summary(return2)$fstatistic[2]
df <- summary(return2)$fstatistic[3]
F_return2 <- summary(return2)$fstatistic[1]
F_test <- df(0.975,q,df)
F_return2 > F_test # TRUE, H0 rejected 

# 11
help(ceosal2)
# lsalary1 = beta_0 + beta_1 * lsales + u
# lsalary2 = beta_0 + beta_1 * lsales + beta_2 * lmktval + beta_3 * profmarg + u
# lsalary3 = beta_0 + beta_1 * lsales + beta_2 * lmktval + beta_3 * profmarg + beta_4 * ceoten + beta_5 * comten + u
lsalary1 <- lm(lsalary~lsales,ceosal2)
lsalary2 <- lm(lsalary~lsales+lmktval+profmarg,ceosal2)
lsalary3 <- lm(lsalary~lsales+lmktval+profmarg+ceoten+comten,ceosal2)
summary(lsalary1)$coef
summary(lsalary2)$coef
summary(lsalary3)$coef

# 12
help(meap93)
# 1) math1 = beta_0 + beta_1 * lexpend + u
math1 <- lm(math10~lexpend,meap93)
summary(math1)
ex <- c(1,1.1)
lex <- data.frame(log(expend))
predict(math1,lex)
1-predict(math1,lex)[2]/predict(math1,lex)[1]
# 3) math10 = beta_0 + beta_1 * lexpend + beta_2 * lenroll + beta_3 * lnchprg + u
math2 <- lm(math10~lexpend+lenroll+lnchprg,meap93)
summary(math2)
q <- summary(math2)$fstatistic[2]
df <- summary(math2)$fstatistic[3]
t_lexpend <- summary(math2)$coef[2,3]
t_test <- qnorm(0.975)
t_lexpend > t_test # TRUE, H0 rejected 

# 13
help(meapsingle)
# math1 = beta_0 + beta_1 * pctsgle + u
# math2 = beta_0 + beta_1 * pctsgle + beta_2 * free + u
# math3 = beta_0 + beta_1 * pctsgle + beta_2 * free + beta_3 * lmedinc + beta_4 * lexppp + u
# math4 = beta_0 + beta_1 * pctsgle + beta_2 * free + beta_3 * lexppp + u
math1 <- lm(math4~pctsgle,meapsingle)
math2 <- lm(math4~pctsgle+free,meapsingle)
math3 <- lm(math4~pctsgle+free+lmedinc+lexppp,meapsingle)
math4 <- lm(math4~pctsgle+free+lexppp,meapsingle)
# 1) 
summary(math1)$coef
# 2) math3, H0: beta_4 = 0
summary(math3)$fstatistic
q <- summary(math3)$fstatistic[2]
df <- summary(math3)$fstatistic[3]
t_lexppp <- summary(math3)$coef[5,3]
t_test <- qnorm(0.975)
t_lexppp > t_test # TRUE, H0 rejected 

# Computer Exercises
library(wooldridge)

# C1
help(vote1)
# voteA = beta_0 + beta_1 * lexpendA + beta_2 * lexpendB + beta_3 * prtystrA + u
voteA <- lm(voteA~lexpendA+lexpendB+prtystrA,vote1)
summary(voteA)
# 2) Trade off between expendA and expendB
# a = expendA + expendB = expendA(1+expendB/expendA)
# log(a) = lexpendA + log(1+expendB/expendA)
# log(1+expendB/expendA) ≈ expendB/expendA
# lexpendA = beta_0 + beta_1 * expendB/expendA + u
# H0: beta_1 = -1
BA <- with(vote1,expendB/expendA)
lexpendA <- lm(lexpendA~BA,vote1)
summary(lexpendA)
beta_1 <- coef(lexpendA)['BA']
se_BA <- summary(lexpendA)$coef[2,2]
t_BA <- (beta_1 + 1)/se_BA
t_test <- qnorm(0.975)
t_BA > t_test # TRUE, H0 rejected 

# C2
help(lawsch85)
# 1) Problem 4 in Chapter 3, H0: beta_5 = 0
lsalary <- lm(lsalary~LSAT+GPA+libvol+lcost+rank,lawsch85)
summary(lsalary)$coef
t_rank <- summary(lsalary)$coef[6,3]
t_test <- qnorm(0.975)
abs(t_rank) > t_test # TRUE, H0 rejected 
# 2-1) H0: beta_1 = 0
t_LSAT <- summary(lsalary)$coef[2,3]
t_test <- qnorm(0.975)
abs(t_LSAT) > t_test # FALSE, fail to reject H0
# 2-2) H0: beta_2 = 0
t_GPA <- summary(lsalary)$coef[3,3]
t_test <- qnorm(0.975)
abs(t_GPA) > t_test # TRUE, H0 rejected 
# 2-3) H0: beta_1 = 0 and beta_2 = 0
lsalary2 <- lm(lsalary~libvol+lcost+rank,lawsch85)
summary(lsalary2)
SSR_r <- sum(residuals(lsalary2)^2)
SSR_ur <- sum(residuals(lsalary)^2)
q <- 2 
df <- summary(lsalary)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
F_test <- qf(0.95,q,df)
F > F_test # TRUE, H0 rejected 
# 3) lsalary3 = beta_0 + beta_1 * LSAT + beta_2 * GPA + beta_3 * libvol + beta_4 * lcost + beta_5 * rank + beta_6 * clsize  + beta_7 * faculty + u
# H0: beta_6 = 0 and beta_7 = 0 
lsalary3 <- lm(lsalary~libvol+lcost+rank+clsize+faculty,lawsch85)
summary(lsalary3)
SSR_r <- sum(residuals(lsalary)^2)
SSR_ur <- sum(residuals(lsalary3)^2)
q <- 2 
df <- summary(lsalary)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
F_test <- qf(0.95,q,df)
F > F_test # FALSE, fail to reject H0

# C3
# lprice = beta_0 + beta_1 * sqrft + beta_2 * bdrms + u
help(hprice1)
lprice <- lm(lprice~sqrft + bdrms, hprice1)
summary(lprice)
# 1) Theta(θ) = 150 * beta_1 + beta_2
beta_1 <- summary(lprice)$coef[2,1]
beta_2 <- summary(lprice)$coef[3,1]
Theta <- 150 * beta_1 + beta_2
Theta
# 2) beta_2 = Theta - 150 * beta_1 
# lprice = beta_0 + beta_1 * sqrft + (Theta - 150 * beta_1) * bdrms + u
# lprice = beta_0 + beta_1 * (sqrft - 150 * bdrms) + Theta * bdrms + u
# a = sqrft - 150 * bdrms
# lprice = beta_0 + beta_1 * a + Theta * bdrms + u
a <- with(hprice1,sqrft - 150 * bdrms) 
lprice1 <- lm(lprice~a + bdrms, hprice1)
summary(lprice1)
# 3) Standard error for Theta(θ) at the 95% confidence interval
se_bdrms <- summary(lprice1)$coef[3,2]
Theta <- summary(lprice1)$coef[3,1]
df <- summary(lprice1)$fstatistic[3]
t_95 <- qt(0.975,df)
c(Theta - se_bdrms*t_95,Theta + se_bdrms*t_95)

# C4
# Example 4.9
help(bwght)
library(tidyr)
data <- drop_na(bwght)
bwght1 <- lm(bwght~cigs+parity+faminc,data)
summary(bwght1)$r.squared
bwght2 <- lm(bwght~.,data)
summary(bwght2)$r.squared

# C5
help(mlb1)
# lsalary1 = beta_0 + beta_1 * years + beta_2 * gamesyr + beta_3 * bavg + beta_4 * hrunsyr + beta_5 * rbisyr + u
# lsalary2 = beta_0 + beta_1 * years + beta_2 * gamesyr + beta_3 * bavg + beta_4 * hrunsyr + u
# 1)
lsalary1 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr,mlb1)
lsalary2 <- lm(lsalary~years+gamesyr+bavg+hrunsyr,mlb1)
coef(lsalary1)['hrunsyr']
coef(lsalary2)['hrunsyr']
summary(lsalary1)$coef[5,3] # t value of hrunsyr
summary(lsalary2)$coef[5,3]
summary(lsalary1)$coef[5,3] > qnorm(0.975) # FALSE
summary(lsalary2)$coef[5,3] > qnorm(0.975) # TRUE
# 2) lsalary3 = beta_0 + beta_1 * years + beta_2 * gamesyr + beta_3 * bavg + beta_4 * hrunsyr + beta_5 * runsyr + beta_6 * fldperc + beta_7 * sbasesyr + u
lsalary3 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+runsyr+fldperc+sbasesyr,mlb1)
summary(lsalary3)$coef
# 3) H0: beta_5 = 0, beta_6 = 0 and beta_7 = 0
SSR_r = sum(residuals(lsalary2)^2)
SSR_ur = sum(residuals(lsalary3)^2)
q <- 3
df <- summary(lsalary3)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df)
F_test <- qf(0.95,q,df)
F > F_test # TRUE, H0 rejected

# C6
help(wage2)
# lwage = beta_0 + beta_1 * educ + beta_2 * exper + beta_3 * tenure + u
lwage = lm(lwage~educ+exper+tenure,wage2)
summary(lwage)
# 1) a = exper+tenure = exper(1+exper/tenure)
# log(a) = log(exper) + log(1+exper/tenure)
# log(1+exper/tenure) ≈ exper/tenure
# lexper = beta_0 + beta_1 * exper/tenure + u
# H0: beta_1 = -1
expten <- with(wage2,exper/tenure,nm.na=T)
lexper <- with(wage2,log(exper))
df1 <- cbind(data,expten,lexper)
df2 <- subset(df1,expten!=Inf)
lwage2 <- lm(lexper~expten,df2)
summary(lwage2)
beta_1 <- coef(lwage2)['expten']
se_expten <- summary(lwage2)$coef[2,2]
t_expten <- (beta_1 + 1)/se_expten
t_test <- qnorm(0.975)
t_expten > t_test # TRUE, H0 rejected 
# 2) The 95% confidence interval
t_95 <- qnorm(0.975)
c(beta_1-se_expten*t_95,beta_1+se_expten*t_95)

# C7
help(twoyear)
# 1) Max, Min and mean of phsrank
summary(twoyear$phsrank)
# 2) equation(4.26)
# lwage = beta_0 + beta_1 * jc + beta_2 * univ + beta_3 * exper + u
# Theta_1 = beta_1 - beta_2
# lwage = beta_0 + (Theta_1 + beta_2) * jc + beta_2 * univ + beta_3 * exper + u
# lwage = beta_0 + Theta_1 * jc + beta_2 * (jc + univ) + beta_3 * exper + u
# totcoll = jc + univ
# lwage = beta_0 + Theta_1 * jc + beta_2 * totcoll + beta_3 * exper + u
# Add phsrank into (4.26)
# lwage1 = beta_0 + Theta_1 * jc + beta_2 * totcoll + beta_3 * exper + beta_4 * phsrank + u
totcoll <-  with(twoyear,jc + univ)
lwage <- lm(lwage~jc+totcoll+exper+phsrank,twoyear)
summary(lwage)
summary(lwage)$coef[5,3] > qnorm(0.975) # FALSE, fail to reject H0
# 3) H0: beta_4 = 0
lwage1 <- lm(lwage~jc+totcoll+exper,twoyear)
summary(lwage1)
SSR_r = sum(residuals(lwage1)^2)
SSR_ur = sum(residuals(lwage)^2)
q <- 1
df <- summary(lwage1)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df)
F_test <- qf(0.95,q,df)
F > F_test # FALSE, fail to reject H0
# 4) lwage2 = beta_0 + Theta_1 * jc + beta_2 * totcoll + beta_3 * exper + beta_4 * id + u
# H0: beta_4 = 0
lwage2 <- lm(lwage~jc+totcoll+id,twoyear)
summary(lwage2)
t_id <- summary(lwage2)$coef[4,3]
t_95 <- qnorm(0.975)
t_id > t_95 # FALSE, fail to reject H0
# two-sided p-value
p_id <- summary(lwage2)$coef[4,4]
p_id

# C8
help(k401ksubs)
# 1) single-person households (so fsize = 1)
data <- subset(k401ksubs,fsize==1)
length(data$e401k)
# 2) nettfa = beta_0 + beta_1 * inc + beta_2 * age + u
nettfa <- lm(nettfa~inc+age,data)
summary(nettfa)
# 4) H0: beta_2 = 1
beta_2 <- coef(nettfa)['age']
se_age <- summary(nettfa)$coef[3,2]
t_age <- (beta_2 - 1)/se_age
t_test <- qnorm(0.995)
t_age > t_test #FALSE, fail to reject H0
# 5) nettfa2 = beta_0 + beta_1 * inc + u
nettfa2 <- lm(nettfa~inc,data)
summary(nettfa2)
coef(nettfa)['inc']
coef(nettfa2)['inc']

# C9
help(discrim)
# lpsoda = beta_0 + beta_1 * prpblck + beta_2 * lincome + beta_3 * prppov + u
lpsoda <- lm(lpsoda~prpblck+lincome+prppov,discrim)
summary(lpsoda)
# 1-1) H0: beta_1 = 0 against the 5% level of two-sided alternative
t_prpblck <- summary(lpsoda)$coef[2,3]
t_prpblck > qnorm(0.975) # TRUE, H0 rejected 
# 1-2) H0: beta_1 = 0 against the 1% level of two-sided alternative
t_prpblck > qnorm(0.995) # FALSE, fail to reject H0
# 2) correlation between lincome and prppov
with(discrim,cov(lincome,prppov,use = "complete.obs"))
summary(lpsoda)$coef
# 3) lpsoda2 = beta_0 + beta_1 * prpblck + beta_2 * lincome + beta_3 * prppov + beta_4 * lhseval + u
lpsoda2 <- lm(lpsoda~prpblck+lincome+prppov+lhseval,discrim)
# two-sided p-value for H0: beta_4 = 0 
summary(lpsoda2)$coef
t_lhseval <- summary(lpsoda2)$coef[5,3]
p_lhseval <- summary(lpsoda2)$coef[5,4]
# 4) p-value for H0: beta_2 = 0 and beta_3 = 0 in lpsoda2
summary(lpsoda2)$coef
lpsoda3 <- lm(lpsoda~prpblck+lhseval,discrim)
SSR_r <- sum(residuals(lpsoda3)^2)
SSR_ur <- sum(residuals(lpsoda2)^2)
df1 <- summary(lpsoda3)$fstatistic[3]
df2 <- summary(lpsoda2)$fstatistic[3]
q <- 2
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 

# C10
help(elem94_95)
# 1) lavgsal = beta_0 + beta_1 * bs + u 
lavgsal <- lm(lavgsal~bs,elem94_95)
summary(lavgsal)
# H0: beta_1 = 0
t_bs <- summary(lavgsal)$coef[2,3]
abs(t_bs) > qnorm(0.975) # TRUE, H0 rejected 
# H0: beta_1 = -1
beta_1 <- coef(lavgsal)['bs']
se_bs <- summary(lavgsal)$coef[2,2]
t_bs2 <- (beta_1 + 1)/se_bs
t_bs2 > qnorm(0.975) # FALSE, fail to reject H0
# 2) lavgsal2 = beta_0 + beta_1 * bs + beta_2 * lenrol + beta_3 * lstaff + u 
lavgsal2 <- lm(lavgsal~bs+lenrol+lstaff,elem94_95)
summary(lavgsal2)
coef(lavgsal)['bs']
coef(lavgsal2)['bs']
# 5) lavgsal3 = beta_0 + beta_1 * bs + beta_2 * lenrol + beta_3 * lstaff + beta_4 * lunch + u 
lavgsal3 <- lm(lavgsal~bs+lenrol+lstaff+lunch,elem94_95)
summary(lavgsal3)

# C11
help(htv)
# 1) educ = beta_0 + beta_1 * motheduc + beta_2 * fatheduc + beta_3 * abil + beta_4 * abil^2 + u
abil_2 <- htv$abil^2
data <- cbind(htv,abil_2)
educ <- lm(educ~motheduc+fatheduc+abil+abil_2,data)
summary(educ)
# 2) H0: beta_1 = beta_2
# educ = beta_0 + beta_1 * (motheduc + fatheduc) + beta_3 * abil + beta_4 * abil^2 + u
# totedu = motheduc + fatheduc
# educ = beta_0 + beta_1 * toteduc + beta_3 * abil + beta_4 * abil^2 + u
totedu <- with(data,motheduc + fatheduc)
educ <- lm(educ~totedu+abil+abil_2,data)
summary(educ)
beta_1 <- coef(educ)['totedu']
t_educ <- summary(educ)$coef[2,3]
t_educ > qnorm(0.975) # TRUE, H0 rejected 
p_educ <- summary(educ)$coef[2,4]
p_educ
# 3) educ2 = beta_0 + beta_1 * motheduc + beta_2 * fatheduc + beta_3 * abil + beta_4 * abil^2 + beta_5 * tuit17 + beta_6 * tuit18 + u
educ2 <- lm(educ~motheduc+fatheduc+abil+abil_2+tuit17+tuit18,data)
summary(educ2)
# H0: beta_5 = 0 and beta_6 = 0
SSR_r <- sum(residuals(educ)^2)
SSR_ur <- sum(residuals(educ2)^2)
q <- 2
df1 <- summary(educ)$fstatistic[3]
df2 <- summary(educ2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 
# 4) correlation between tuit17 and tuit18
with(data,cor(tuit17,tuit18))
avgtuit <- with(data,(tuit17+tuit18)/2)
educ3 <- lm(educ~motheduc+fatheduc+abil+abil_2+avgtuit,data)
summary(educ3)
# 5) H0: beta_5 = 0 in educ3
t_avgtuit <- summary(educ3)$coef[6,3]
t_avgtuit > qnorm(0.975) # FALSE, fail to reject H0

# C12
help(econmath)
# 1) 
colgpa <- lm(colgpa~hsgpa+actmth+acteng,econmath)
summary(colgpa)
# 2) 
beta_1 <- coef(colgpa)['hsgpa']
se_1 <- summary(colgpa)$coef[2,2]
t <- qnorm(0.343)
a <- c(beta_1,beta_1+se_1*t)
1/a
# 3) H0: beta_2 = beta_3
# colgpa = beta_0 + beta_1 * hsgpa + beta_2 * actmth + beta_3 * acteng + u
# beta_2 = beta_3
# colgpa = beta_0 + beta_1 * hsgpa + beta_2 * (actmth + acteng) + u
# totact = actmth + acteng
# colgpa = beta_0 + beta_1 * hsgpa + beta_2 * totact + u
totact <- with(econmath, actmth + acteng)
colgpa <- lm(colgpa~hsgpa+totact,econmath)
summary(colgpa)
t_totact <- summary(colgpa)$coef[3,3]
t_totact > qnorm(0.975) # TRUE, H0 rejected 

# C13
help(gpa1)
# colgpa = beta_0 + beta_1 * PC + beta_2 * hsGPA + beta_3 * ACT + u
# 1) 95% confidence interval for beta_1
colgpa <- lm(colGPA~PC+hsGPA+ACT,gpa1)
summary(colgpa)
beta_1 <- coef(colgpa)['PC']
t_95 <- qnorm(0.975)
c(beta_1-se_1*t_95,beta_1+se_1*t_95)
# H0: beta_1 = 0
t_pc <- summary(colgpa)$coef[2,3]
t_pc > qnorm(0.975)# TRUE, H0 rejected 
# 2) statistical significance of beta_2 and beta_3
summary(colgpa)$coef
t_hsgpa <- summary(colgpa)$coef[3,3]
t_hsgpa > qnorm(0.975)# TRUE, H0 rejected 
t_act <- summary(colgpa)$coef[4,3]
t_act > qnorm(0.975)# FALSE, fail to reject H0
# 3) colgpa2 = beta_0 + beta_1 * PC + beta_2 * hsGPA + beta_3 * ACT + beta_4 * fathcoll + beta_5 * mothcoll + u
# H0: beta_4 = 0 and beta_5 = 0
colgpa2 <- lm(colGPA~PC+hsGPA+ACT+fathcoll+mothcoll,gpa1)
summary(colgpa2)
SSR_r <- sum(residuals(colgpa)^2)
SSR_ur <- sum(residuals(colgpa2)^2)
q <- 2
df1 <- summary(colgpa)$fstatistic[3]
df2 <- summary(colgpa2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2)# FALSE, fail to reject H0

# C14
help(jtrain98)
# earn1 = beta_0 + beta_1 * train + beta_2 * earn96 + beta_3 * educ + beta_4 * age + beta_5 * married + u
earn1 <- lm(earn98~train+earn96+educ+age+married,jtrain98)
# 1) earn98 = beta_0 + beta_1 * train + beta_2 * earn96 + beta_3 * educ + beta_4 * age + beta_5 * married + beta_6 * unem96 + u
earn2 <- lm(earn98~train+earn96+educ+age+married+unem96,jtrain98)
summary(earn2)
# 2) 
coef(earn1)['train']
coef(earn2)['train']
# 3) correlation between earn96 and unem96
with(jtrain98,cor(earn96,unem96))
# 4) H0: beta_6 = 0
t_unem <- summary(earn2)$coef[7,3]
t_unem > qnorm(0.975)# FALSE, fail to reject H0
