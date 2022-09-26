# Chapter 3 
library(wooldridge)

# Example 3.1
help(gpa1)
colGPA <- lm(colGPA~hsGPA+ACT,data=gpa1)
coef(colGPA)

# Example 3.2
help(wage1)
lwage <- lm(lwage~educ+exper+tenure,data=wage1)
coef(lwage)


# Example 3.3
help(k401k)
prate <- lm(prate~mrate+age,k401k)
coef(prate)

# Example 3.4
length(gpa1$colGPA)
summary(colGPA)$r.squared

# Example 3.5
help(crime1)
narr86 <- lm(narr86~pcnv+ptime86+qemp86,crime1)
coef(narr86)
length(crime1$narr86)
summary(narr86)$r.squared
# with new parameter 'avgsen'
narr86 <- lm(narr86~pcnv+avgsen+ptime86+qemp86,crime1)
coef(narr86)
summary(narr86)$r.squared

# Example 3.7
help(jtrain98)
earn98 <- lm(earn98~train,jtrain98)
coef(earn98)
length(jtrain98$train)
summary(earn98)$r.squared
# multiple regression
earn98 <- lm(earn98~train+earn96+educ+age+married,jtrain98)
coef(earn98)
summary(earn98)$r.squared

# Problems
# 1 
help(gpa2)
colgpa <- lm(colgpa~hsperc+sat,gpa2)
coef(colgpa)

# 2) Predicted college GPA when hsperc=20 and sat=1050
a <- data.frame(hsperc=20,sat=1050)
predict(colgpa,a)
# 3) Predict colgpa when sat raises 140
b <- data.frame(hsperc=10,sat=c(0,150))
predict(colgpa,b)
# 4) SAT scores lead to raising of 0.5 point of colgpa when hsperc fixed
b0 <- colgpa$coef['(Intercept)']
b1 <- colgpa$coef['hsperc']
b2 <- colgpa$coef['sat']
p <- 0.5/b2
p

# 2 
help(wage2)
educ <- lm(educ~sibs+meduc+feduc,wage2)
coef(educ)
length(wage2$educ)
summary(educ)$r.squared

# 1) sibs lead to raising of 1 year educ when meduc and feduc fixed
b1 <- educ$coef['sibs']
q <- 1/b1
q
# 3) Predict educ when sibs=0, meduc=feduc=12 and meduc=feduc=16
c <- data.frame(sibs=0,meduc=c(12,16),feduc=c(12,16))
predict(educ,c)

# 3
help(sleep75)
sleep <- lm(sleep~totwrk+educ+age,sleep75)
coef(sleep)
length(sleep75$age)
summary(sleep)$r.squared

# 3) Predict fall of sleep when totwrk more than 5 hours
b1 <- sleep$coef['totwrk']
d <- 5 * b1
d

# 4
help(lawsch85)
lsalary <- lm(lsalary~LSAT+GPA+libvol+lcost+rank,lawsch85)
coef(lsalary)
summary(lsalary)$r.squared

# 3) GPA lead to 1 point change of lsalary
b2 <- lsalary$coef['GPA']
e <- 1 / b2
e

# 9
help(hprice2)
logprice1 <- lm(lprice~lnox,hprice2)
coef(logprice1)
summary(logprice1)$r.squared
logprice2 <- lm(lprice~lnox+rooms,hprice2)
coef(logprice2)
summary(logprice2)$r.squared

# 15
help(mlb1)
lsalary1 <- lm(lsalary~years,mlb1)
n = length(mlb1$salary)
summary(lsalary1)
# SSR(The sum of of squared residual)
SSR <- sum(residuals(lsalary1)^2)
SSR
# SSE(The explained sum of squares)
SSE <- sum((fitted(lsalary1)-mean(mlb1$years))^2)
SSE
# SST = SSE + SSR
SSE + SSR
# SER(The Standard Error of the Regression)
SER <- sqrt(SSR / (n-2))
SER
# multiple regression
lsalary2 <- lm(lsalary~years+rbisyr,mlb1)
summary(lsalary2)
# SSR
sum(residuals(lsalary2)^2)
# SER
sqrt(sum(residuals(lsalary2)^2) / (n-2))

# 16
help(lawsch85)
lsalary <- lm(lsalary~rank+GPA,lawsch85)
summary(lsalary)$r.squared
lsalary <- lm(lsalary~rank+GPA+age,lawsch85)
summary(lsalary)$r.squared

# Computer Exercises
library(wooldridge)

# C1
# 3) Estimate bwght OLS with and without faminc
help(bwght)
bwght1 <- lm(bwght~cigs,bwght)
coef(bwght1)
summary(bwght1)$r.squared
bwght2 <- lm(bwght~cigs+faminc,bwght)
coef(bwght2)
summary(bwght2)$r.squared

# C2
# 1) price = beta_0 + beta_1 * sqrft + beta_2 * bdrms + u
help(hprice1)
price <- lm(price~sqrft + bdrms, hprice1)
summary(price)
# 2) Estimate increase in price for a house with one more bedroom
coef(price)['bdrms']
# 3) Estimate increase in price for a house with additional 140 square feet bedroom 
predict(price, data.frame(sqrft=140, bdrms=1))
# 4) R^2
summary(price)$r.squared
# 5) Predict price for sqrft = 2438 and bdrms = 4
predict(price, data.frame(sqrft=2438, bdrms=4))

# C3
help(ceosal2)
# 1) Estimate a model relating annual salary to firm sales and market value
salary <- lm(salary~sales+mktval,ceosal2)
salary
summary(salary)$r.squared
# 3) Add ceoten to the model
salary <- lm(salary~ceoten+sales+mktval,ceosal2)
coef(salary)['ceoten']
summary(salary)$r.squared
# 4) Correlation coefficient between log(mktval) and profits
with(ceosal2,cor(lmktval,profits))

# C4
help(attend)
# 1) Max, min and mean of atndrte, priGPA and ACT
with(attend,c(max(atndrte),max(priGPA),max(ACT)))
with(attend,c(min(atndrte),min(priGPA),min(ACT)))
with(attend,c(mean(atndrte),mean(priGPA),mean(ACT)))
# 2) atndrte = beta_0 + beta_1 * priGPA + beta_2 * ACT + u
atndrte <- lm(atndrte~priGPA+ACT,attend)     
coef(atndrte)['(Intercept)']
# 3) Slope
coef(atndrte)[c('priGPA','ACT')]
# 4) Predict atndrte when priGPA = 3.65 and ACT = 20
predict(atndrte,data.frame(priGPA=3.65,ACT=20))
# 5) Predict atndrte when priGPA = 3.1 and ACT = 21 and 26
predict(atndrte,data.frame(priGPA=3.1,ACT=c(21,26)))

# C5
# Example 3.2
help(wage1)
lwage <- lm(lwage~educ+exper+tenure,wage1)
r1 <- residuals(lwage)
lwage <- lm(lwage~r1,wage1)
coef(lwage)['r1']

# C6
help(wage2)
# 1) OLR of IQ on educ
IQ <- lm(IQ~educ,wage2)
summary(IQ)$r.squared
delta_1 <- coef(IQ)['educ']
delta_1
# 2) OLR of log(wage) on educ
lwage1 <- lm(lwage~educ,wage2)
beta_1 <- coef(lwage1)['educ']
beta_1
# 3) OLR of log(wage) on educ and IQ
lwage2 <- lm(lwage~educ+IQ,wage2)
beta_1_hat <- coef(lwage2)['educ']
beta_2_hat <- coef(lwage2)['IQ']
beta_1_hat
beta_2_hat
# 4) Verify that beta_1 = beta_1_hat + beta_2_hat * delta_1
beta_1
beta_1_hat + beta_2_hat * delta_1

# C7
help(meap93)
# 1) math10 = beta_0 + beta_1 * log(expend) + beta_2 * lnchprg + u
math10 <- lm(math10~lexpend+lnchprg,meap93)
with(meap93,length(lnchprg))
summary(math10)$r.squared
# 3) 
coef(math10)
# 4) Correlation between lexpend and lnchprg
with(meap93,cor(lexpend,lnchprg))

# C8
help(discrim)
# 1) Mean and SD of prpblck and income
mean(discrim$prpblck,na.rm=T)
mean(discrim$income,na.rm=T)
sd(discrim$prpblck,na.rm=T)
sd(discrim$income,na.rm=T)
# 2) psoda = beta_0 + beta_1 * prpblck + beta_2 * income + u
psoda <- lm(psoda~prpblck+income,discrim)
summary(psoda)
coef(psoda)['prpblck']
# 4) log(psoda) = beta_0 + beta_1 * prpblck + beta_2 * income + u
lpsoda <- lm(lpsoda~prpblck+lincome,discrim)
coef(lpsoda)['prpblck']
0.2*coef(lpsoda)['prpblck']
# 5) Add the variable prppov to log(psoda) 
lpsoda <- lm(lpsoda~prpblck+prppov+lincome,discrim)
coef(lpsoda)['prpblck']
# 6) Correlation between log(income) and prppov
# install.packages('tidyr')
library(tidyr)
c8 <- drop_na(discrim)
with(c8,cor(lincome,prppov))

# C9
help(charity) 
# 1) gift = beta_0 + beta_1 * mailsyear + beta_2 * giftlast + beta_3 * propresp + u
gift1 <- lm(gift~mailsyear+giftlast+propresp,charity)
summary(gift1)$r.squared
gift2 <- lm(gift~mailsyear+propresp,charity)
summary(gift2)$r.squared
coef(gift1)
# 3) Add the variable avggift to gift1 and estimate effect of mailsyear
gift3 <- lm(gift~mailsyear+giftlast+avggift+propresp,charity)
summary(gift3)$r.squared
coef(gift1)['mailsyear']
coef(gift3)['mailsyear']
# 4) Estimate effects on giftlast
coef(gift1)['giftlast']
coef(gift3)['giftlast']

# C10
help(htv)
# 1-1) Range of the educ
with(htv,c(min(educ),max(educ)))
# 1-2) Percentage of men completed twelfth grade but no higher grade 
with(htv,length(educ[educ==12])/length(educ))
# 2) educ = beta_0 + beta_1 * motheduc + beta_2 * fatheduc + u
educ1 <- lm(educ~motheduc+fatheduc,htv)
summary(educ1)$r.squared
# 3) Add the variable abil to eudc1
educ2 <- lm(educ~motheduc+fatheduc+abil,htv)
summary(educ2)$r.squared
# 4) educ = beta_0 + beta_1 * motheduc + beta_2 * fatheduc + beta_3 * abil + beta_4 * abil^2 + u
abil_2 <- htv$abil^2
educ3 <- lm(educ~motheduc+fatheduc+abil+abil_2,htv)
summary(educ3)

# C11
help(meapsingle)
# 1) OLR of math1 on pctsgle
math1 <- lm(math4~pctsgle,meapsingle)
summary(math1)
# 2) Add the variables lmedinc and free to math1
math2 <- lm(math4~pctsgle+lmedinc+free,meapsingle)
summary(math2)
coef(math1)['pctsgle']
coef(math2)['pctsgle']
# 3) Correlation between lmedinc and free
with(meapsingle,cor(lmedinc,free))
# 4) Find the Variance Inflation Factor (VIF) for math2
# install.packages('car')
library(car)
vif(math2)
help(vif)

# C12
help(econmath)
# 1-1) How many students received a perfect score for the course
with(econmath,length(attexc[attexc==1]))
# 1-2) average score
mean(econmath$score)
# 1-3) means and standard deviations of actmth and acteng
library(tidyr)
c12 <- drop_na(econmath)
with(c12,c(mean(actmth),mean(acteng)))
with(c12,c(sd(actmth),sd(acteng)))
# 2) OLR of score to colgpa, actmth, and acteng
score <- lm(score~colgpa+actmth+acteng,econmath)
summary(score)

# C13
help(gpa1)
# 1) colGPA = beta_0 + beta_1 * PC + u
colgpa1 <- lm(colGPA~PC,gpa1)
coef(colgpa1)
# 2) Add the variables hsGPA and ACT to colgpa1
colgpa2 <- lm(colGPA~PC+hsGPA+ACT,gpa1)
coef(colgpa2)
# 5)
with(gpa1,cor(hsGPA,ACT))
