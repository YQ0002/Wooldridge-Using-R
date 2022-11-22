# Chapter 7 
rm(list=ls())
library(wooldridge)

# 7-2 A Single Dummy Independent Variable
# Example 7.1 Hourly Wage Equation
help(wage1)
wage <- lm(wage~female+educ+exper+tenure,wage1)
summary(wage)
coef(wage)['female']
wage2 <- lm(wage~female,wage1)
summary(wage2)
coef(wage2)['female']

# Example 7.2 
help(gpa1)
colGPA <- lm(colGPA~PC+hsGPA+ACT,gpa1)
summary(colGPA)
coef(colGPA)['PC']
colGPA2 <- lm(colGPA~PC,gpa1)
summary(colGPA2)
coef(colGPA2)['PC']

# Example 7.3
help(jtrain)
data <- subset(jtrain,year==1988)
hrsemp <- lm(hrsemp~grant+lsales+lemploy,data)
summary(hrsemp)
coef(hrsemp)['grant']
hist(data$grant)

# 7-2a The Dependent Variable Is log(y)
# Example 7.4 
help(hprice1)
lprice <- lm(lprice~llotsize+lsqrft+bdrms+colonial,hprice1)
summary(lprice)
coef(lprice)['colonial']

# Example 7.5 
help(wage1)
lwage <- lm(lwage~female+educ+exper+expersq+tenure+tenursq,wage1)
summary(lwage)
x <- coef(lwage)['female']
# lwage_F - lwage_M = 0.296511 
# (wage_F - wage_M)/wage_M = exp(lwage_F - lwage_M)-1
exp(x) - 1

# 7-3 Using Dummy Variables for Multiple Categories
# Example 7.6 
# Choose single men as base groups
help(wage1)
marrmale <- (wage1$female==0 & wage1$married==1) + 0
marrfem <- (wage1$female==1 & wage1$married==1) + 0
singfem <- (wage1$female==1 & wage1$married==0) + 0
lwage1 <- lm(lwage~marrmale+marrfem+singfem+educ+exper+expersq+tenure+tenursq,wage1)
summary(lwage1)  
coef(lwage1)
# Choose married women as base groups
marrmale <- (wage1$female==0 & wage1$married==1) + 0
singmale <- (wage1$female==0 & wage1$married==0) + 0
singfem <- (wage1$female==1 & wage1$married==0) + 0
lwage2 <- lm(lwage~marrmale+singmale+singfem+educ+exper+expersq+tenure+tenursq,wage1)
summary(lwage2) 
coef(lwage2)

# 7-3a
# Example 7.7 Effects of Physical Attractiveness on Wage
help(beauty)
table(beauty$female)
data1 <- subset(beauty,female==0)
lwage1 <- lm(lwage~belavg+abvavg+.,data1)
summary(lwage1)
data2 <- subset(beauty,female==1)
lwage2 <- lm(lwage~belavg+abvavg+.,data2)
summary(lwage2)

# Example 7.8 Effects of Law School Rankings on Starting Salaries
help(lawsch85)
length(lawsch85$rank)
summary(lawsch85$rank)
labels <- c(0,10,25,40,60,100,175)
rank0 <- cut(lawsch85$rank,labels)
table(rank0)
# Choose (100,175] as base groups
top10 <- (rank0=="(0,10]") + 0
r11_25 <- (rank0=="(10,25]") + 0
r26_40 <- (rank0=="(25,40]") + 0
r41_60 <- (rank0=="(40,60]") + 0
r61_100 <- (rank0=="(60,100]") + 0
lsalary1 <- lm(lsalary~top10+r11_25+r26_40+r41_60+r61_100+LSAT+GPA+llibvol+lcost,lawsch85)
summary(lsalary1)
coef(lsalary1)
exp(coef(lsalary1)['top10']) - 1
lsalary2 <- lm(lsalary~rank+LSAT+GPA+llibvol+lcost,lawsch85)
summary(lsalary2)
coef(lsalary2)
summary(lsalary1)$adj.r.squared
summary(lsalary2)$adj.r.squared

# 7-4 Interactions Involving Dummy Variables
# 7-4a
# Example 7.10 Log Hourly Wage Equation
help(wage1)
lwage <- lm(lwage~female+educ+female*educ+exper+expersq+tenure+tenursq,wage1)
summary(lwage)
coef(lwage)['female']
# Δlwage/Δfemale = beta_1 + beta_3 * educ
with(wage1,cor(female,female*educ))

# Example 7.11 Effects of Race on Baseball Player Salaries
help(mlb1)
lsalary1 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+black+hispan+blckpb+hispph,mlb1)
summary(lsalary1)
lsalary2 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar,mlb1)
summary(lsalary2)
# H0: beta_9\10\11\12 = 0
summary(lsalary1)$adj.r.squared
summary(lsalary2)$adj.r.squared
SSR_r <- sum(residuals(lsalary2)^2)
SSR_ur <- sum(residuals(lsalary1)^2)
q <- 4
df1 <- summary(lsalary2)$fstatistic[3]
df2 <- summary(lsalary1)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 
F

# 7-4c Testing for Differences in Regression Functions across Groups
help(gpa3)
data <- subset(gpa3,term==2)
cumgpa1 <- lm(cumgpa~female+sat+female*sat+hsperc+female*hsperc+tothrs+female*tothrs,data)
summary(cumgpa1)
# H0: beta_1\3\5\7 = 0
cumgpa2 <- lm(cumgpa~sat+hsperc+tothrs,data)
summary(cumgpa2)
summary(cumgpa2)$r.squared
SSR_r <- sum(residuals(cumgpa2)^2)
SSR_ur <- sum(residuals(cumgpa1)^2)
q <- 4
df1 <- summary(cumgpa2)$fstatistic[3]
df2 <- summary(cumgpa1)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 
F
cumgpa3 <- lm(cumgpa~female+sat+hsperc+tothrs,data)
summary(cumgpa3)

# Chow test
# F = [SSR_p-(SSR_1+SSR_2)]/(SSR_1+SSR_2) * [n-2*(k+1)]/((k+1))

# 7-5 A Binary Dependent Variable: The Linear Probability Model
help(mroz)
inlf <- lm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,mroz)
summary(inlf)$coef

# Example 7.12 A Linear Probability Model of Arrests
help(crime1)
arr86 <- (crime1$narr86!=0) + 0
table(arr86)
data <- cbind(crime1,arr86)
arr86<- lm(arr86~pcnv+avgsen+tottime+ptime86+qemp86,data)
summary(arr86)
summary(arr86)$coef
arr86_2<- lm(arr86~pcnv+avgsen+tottime+ptime86+qemp86+black+hispan,data)
summary(arr86_2)
summary(arr86_2)$coef
sample <- data.frame(pcnv=0,avgsen=0,tottime=0,ptime86=0,qemp86=4,black=1,hispan=0)
predict(arr86_2,sample)

# 7-6 More on Policy Analysis and Program Evaluation
help(jtrain)
data <- subset(jtrain,year==1988)
lscrap <- lm(lscrap~grant+lsales+lemploy,data)
summary(lscrap)

# Example 7.13 Ev aluating a Job Training Program using Unrestricted Regression Adjustment
help(jtrain98)
earn98 <- lm(earn98~train+earn96+educ+age+married,jtrain98)
summary(earn98)
earn96_0 <- with(jtrain98,earn96-mean(earn96))
educ_0 <- with(jtrain98,educ-mean(educ))
age_0 <- with(jtrain98,age-mean(age))
married_0 <- with(jtrain98,married-mean(married))
data <- cbind(jtrain98,earn96_0,educ_0,age_0,married_0)
earn98_1 <- lm(earn98~train+earn96+educ+age+married+train*earn96_0+train*educ_0+train*age_0+train*married_0,data)
summary(earn98_1)

# Problems
# 1
help(sleep75)
sleep <- lm(sleep~totwrk+educ+age+agesq+male,sleep75)
summary(sleep)
# 1) 
coef(sleep)['male']
summary(sleep)$coef[6,4]
# 2) 
coef(sleep)['totwrk']
summary(sleep)$coef[2,4]
# 3 H0: beta_3 = 0 and beta_4 = 0
sleep2 <- lm(sleep~totwrk+educ,sleep75)
summary(sleep2)
SSR_ur <- sum(residuals(sleep)^2)
SSR_r <- sum(residuals(sleep2)^2)
q <- 2
df1 <- summary(sleep2)$fstatistic[3]
df2 <- summary(sleep)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 
F

# 2
help(bwght)
lbwght1 <- lm(lbwght~cigs+lfaminc+parity+male+white,bwght)
summary(lbwght1)
lbwght2 <- lm(lbwght~cigs+lfaminc+parity+male+white+motheduc+fatheduc,bwght)
summary(lbwght2)
# 1) 
coef(lbwght1)['cigs'] * 10
# 2) 
coef(lbwght1)['white']
summary(lbwght1)$coef[6,4]
# 3) 
coef(lbwght2)['motheduc']
summary(lbwght2)$coef[7,4]

# 3
help(gpa2)
sat <- lm(sat~hsize+hsizesq+female+black+female*black,gpa2)
summary(sat)
# 1) 
coef(sat)['hsizesq']
summary(sat)$coef[3,4]
# Δsat = beta_1 + beta_ 2 * hsize
beta_1 <- coef(sat)['hsize']
beta_2 <- coef(sat)['hsizesq']
abs(beta_1/(2*beta_2))
# 2) 
coef(sat)['female']
summary(sat)$coef[4,4]
# 3) 
coef(sat)['black']
summary(sat)$coef[5,4]
# 4) 
# Taking nonblack females as base groups
blackmale <- (gpa2$female==0 & gpa2$black==1) + 0
blackfem <- (gpa2$female==1 & gpa2$black==1) + 0
nonblackfem <- (gpa2$female==1 & gpa2$black==0) + 0
sat2 <- lm(sat~hsize+hsizesq+blackmale+blackfem+nonblackfem,gpa2)
summary(sat2)
coef(sat2)['blackmale']

# 4
help(ceosal1)
lsalary <- lm(lsalary~lsales+roe+finance+consprod+utility,ceosal1)
summary(lsalary)
# 1)
beta_5 <- coef(lsalary)['utility']
beta_5
t_utility <- summary(lsalary)$coef[6,4]
t_test <- qnorm(0.995)
t_utility > t_test #FALSE, fail to reject H0 at the 1% level
# 2)
exp(beta_5) - 1
# 3)
lsalary2 <- lm(lsalary~lsales+roe+indus+consprod+utility,ceosal1)
summary(lsalary2)
beta_4 <- coef(lsalary2)['consprod']
beta_4
t_consprod <- summary(lsalary2)$coef[5,4]
t_test <- qnorm(0.975)
t_consprod > t_test #FALSE, fail to reject H0

# 5
help(gpa1)
# 1)
colGPA <- lm(colGPA~PC+hsGPA+ACT,gpa1)
summary(colGPA)
coef(colGPA)['(Intercept)']
coef(colGPA)['PC']
# colGPA = beta_0 + δ_0 * (1 - PC) + beta_1 * hsGPA + beta_2 * ACT
# colGPA = (beta_0 + δ_0) + δ_0 * noPC + beta_1 * hsGPA + beta_2 * ACT
noPC = 1 - gpa1$PC
colGPA2 <- lm(colGPA~noPC+hsGPA+ACT,gpa1)
summary(colGPA2)
coef(colGPA2)['hsGPA'] # δ_0
coef(colGPA2)['noPC'] # beta_0 + δ_0
# 2)
summary(colGPA)$r.squared
summary(colGPA2)$r.squared

# 7
help(mroz)
# 1) 
inlf <- lm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,mroz)
summary(inlf)
# inlf = 1 - outlf
# 1 - outlf = beta_0 + beta_1 * nwifeinc + beta_2 * educ + ···
# outlf = (1-beta_0) - beta_1 * nwifeinc - beta_2 * educ - ···
outlf = 1 - mroz$inlf
outlf <- lm(outlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,mroz)
summary(outlf)
# 2)
coef(inlf)['(Intercept)'] 
coef(outlf)['(Intercept)'] 
summary(inlf)$coef
summary(outlf)$coef
# 3)
summary(inlf)$r.squared
summary(outlf)$r.squared

# 8
# 1) 
# lwage <- lm(lwage~usage+educ+exper+expersq+female)
# 2) 
# lwage <- lm(lwage~usage+educ+exper+expersq+female*usage)
# 3) 
# lwage <- lm(lwage~luse+muse+huse+educ+exper+expersq+female)

# 9 
help(twoyear)
# 3) 
lwage <- lm(lwage~female+totcoll+female*totcoll,twoyear)
summary(lwage)
beta_1 <- coef(lwage)['female']
beta_3 <- coef(lwage)['female:totcoll'] 
abs(beta_1/beta_3)
# Δlwage = beta_1 + beta_3 * totcoll
beta_1 + beta_3*4

# 11 
help(econmath)
score1 <- lm(score~colgpa,econmath)
summary(score1)
score2 <- lm(score~male+colgpa,econmath)
summary(score2)
score3 <- lm(score~male+colgpa+male*colgpa,econmath)
summary(score3)
colgpa0 <- with(econmath,colgpa-mean(colgpa))
score4 <- lm(score~male+colgpa+male*colgpa0,econmath)
summary(score4)
# 1) 
beta_1 <- coef(score2)['male'] 
se_1 <- summary(score2)$coef[2,2]
t_95 <- qnorm(0.975)
c(beta_1-se_1*t_95,beta_1+se_1*t_95)
# 2) 
coef(score2)['male'] 
summary(score2)$coef[2,4]
# H0: beta_2 of score2 = 0
SSR_r <- sum(residuals(score1)^2)
SSR_ur <- sum(residuals(score2)^2)
q <- 1
df1 <- summary(score1)$fstatistic[3]
df2 <- summary(score2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected
# 3) 
coef(score2)['male'] 
coef(score3)['male'] 
coef(score4)['male'] 

# 12 
help(mlb1)
lsalary1 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+black+hispan+blckpb+hispph,mlb1)
summary(lsalary1)
percblck0 <- with(mlb1,percblck-mean(percblck,na.rm=TRUE))
perchisp0 <- with(mlb1,perchisp-mean(perchisp,na.rm=TRUE))
lsalary2 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+black+hispan+black*percblck0+hispan*perchisp0,mlb1)
summary(lsalary2)
# 1) 
coef(lsalary1)['black'] 
coef(lsalary1)['hispan'] 
coef(lsalary2)['black'] 
coef(lsalary2)['hispan'] 
# 2) 
summary(lsalary2)$coef
# H0: beta_9 and beta_9 = 0
lsalary3 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar,mlb1)
summary(lsalary3)
SSR_r <- sum(residuals(lsalary3)^2)
SSR_ur <- sum(residuals(lsalary2)^2)
q <- 2
df1 <- summary(lsalary3)$fstatistic[3]
df2 <- summary(lsalary2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 

# Computer Exercises
rm(list=ls())
library(wooldridge)

# C1
help(gpa1)
colGPA1 <- lm(colGPA~PC+hsGPA+ACT,gpa1)
summary(colGPA1)
# 1) 
colGPA2 <- lm(colGPA~PC+hsGPA+ACT+fathcoll+mothcoll,gpa1)
summary(colGPA)
coef(colGPA)['PC'] 
# 2) 
# H0: beta_4 and beta_5 = 0
SSR_r <- sum(residuals(colGPA1)^2)
SSR_ur <- sum(residuals(colGPA2)^2)
q <- 2
df1 <- summary(colGPA1)$fstatistic[3]
df2 <- summary(colGPA2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # FALSE, fail to reject H0
F
# 3) 
hsGPAsq <- gpa1$hsGPA^2
colGPA3 <- lm(colGPA~PC+hsGPA+ACT+fathcoll+mothcoll+hsGPAsq,gpa1)
summary(colGPA3)
coef(colGPA3)['hsGPAsq'] 

# C2
help(wage2) 
# 1) 
lwage1 <- lm(lwage~educ+exper+tenure+married+black+south+urban,wage2) 
summary(lwage1)
coef(lwage1)['black']
summary(lwage1)$coef[6,4]
# 2) 
expersq <- wage2$exper^2
tenuresq <- wage2$tenure^2
lwage2 <- lm(lwage~educ+exper+tenure+married+black+south+urban+expersq+tenuresq,wage2) 
summary(lwage2)
# H0: beta_8 and beta_9 ≠ 0
SSR_r <- sum(residuals(lwage1)^2)
SSR_ur <- sum(residuals(lwage2)^2)
q <- 2
df1 <- summary(lwage1)$fstatistic[3]
df2 <- summary(lwage2)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.9,df1,df2) # TRUE, H0 rejected 
F
# 3) 
lwage3 <- lm(lwage~educ+exper+tenure+married+black+south+urban+black*educ,wage2) 
summary(lwage3)
coef(lwage3)['educ:black']
summary(lwage3)$coef[9,4]
# 4) 
singblack <- (wage2$married==0 & wage2$black==1) + 0
singnonb <- (wage2$married==0 & wage2$black==0) + 0
marrblack <- (wage2$married==1 & wage2$black==1) + 0
lwage4 <- lm(lwage~educ+exper+tenure+singblack+singnonb+marrblack+south+urban,wage2) 
summary(lwage4)
coef(lwage4)['marrblack']

# C3
help(mlb1)
# 1) 
lsalary1 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+shrtstop+catcher,mlb1)
summary(lsalary1)
coef(lsalary1)['catcher']
summary(lsalary1)$coef[14,4]
exp(coef(lsalary1)['catcher'])-1
# 2) 
# H0: beta_9/10/11/12/13 = 0
lsalary2 <- lm(lsalary~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar,mlb1)
summary(lsalary2)
SSR_r <- sum(residuals(lsalary2)^2)
SSR_ur <- sum(residuals(lsalary1)^2)
q <- 5
df1 <- summary(lsalary2)$fstatistic[3]
df2 <- summary(lsalary1)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 
F

# C4  
help(gpa2)
# 1) 
colgpa1 <- lm(colgpa~hsize+hsizesq+hsperc+sat+female+athlete,gpa2)
summary(colgpa1)
# 2) 
coef(colgpa1)['athlete']
colgpa2 <- lm(colgpa~hsize+hsizesq+hsperc+female+athlete,gpa2)
summary(colgpa2)
coef(colgpa2)['athlete']
# 3) 
femath <- (gpa2$female==1 & gpa2$athlete==1) + 0
maleath <- (gpa2$female==0 & gpa2$athlete==1) + 0
malenona <- (gpa2$female==0 & gpa2$athlete==0) + 0
colgpa3 <- lm(colgpa~hsize+hsizesq+hsperc+sat+femath+maleath+malenona,gpa2)
summary(colgpa3)
coef(colgpa3)['femath']
summary(colgpa3)$coef[6,4]

# C5 
help(ceosal1)
lsalary1 <- lm(lsalary~lsales+roe+ros,ceosal1)
summary(lsalary1)
rosneg <- (ceosal1$ros<0) + 0
lsalary2 <- lm(lsalary~lsales+roe+rosneg,ceosal1)
summary(lsalary2)
coef(lsalary1)['ros']
coef(lsalary2)['rosneg']
summary(lsalary2)$coef[4,4]

# C6 
help(sleep75)
sleep1 <- lm(sleep~totwrk+educ+age+agesq+yngkid,sleep75)
summary(sleep1)
# 1) 
sleep2 <- lm(sleep~totwrk+educ+age+agesq+yngkid+male,sleep75)
summary(sleep2)
coef(sleep2)['male']
summary(sleep2)$coef[7,4]
# 2) Chow test
# F = [SSR_p-(SSR_1+SSR_2)]/(SSR_1+SSR_2) * [n-2*(k+1)]/((k+1))
data1 <- subset(sleep75,male==1)
sleep2_m <- lm(sleep~totwrk+educ+age+agesq+yngkid+male,data1)
data2 <- subset(sleep75,male==0)
sleep2_f <- lm(sleep~totwrk+educ+age+agesq+yngkid+male,data2)
SSR_m <- sum(residuals(sleep2_m)^2)
SSR_f <- sum(residuals(sleep2_f)^2)
SSR_t <- sum(residuals(sleep2)^2)
df <- summary(sleep2)$fstatistic[3]
n <- length(sleep75$sleep)
k <- 5
F <- (SSR_t-(SSR_m+SSR_f))/(SSR_m+SSR_f) * (n-2*(k+1))/(k+1)
F_test <- qf(0.95,k,df)
F > F_test #FALSE, fail to reject H0
F
# 3) Chow test # allow for a different intercept
# F = [SSR_p-(SSR_1+SSR_2)]/(SSR_1+SSR_2) * (n-2*k)/(k) 
F <- (SSR_t-(SSR_m+SSR_f))/(SSR_m+SSR_f) * (n-2*k)/(k)
F > F_test #FALSE, fail to reject H0
F

# C7 
help(wage1)
lwage <- lm(lwage~female+educ+female*educ+exper+expersq+tenure+tenursq,wage1)
summary(lwage)
# 1) when educ = 12.5
# Δlwage/Δfemale = beta_1 + beta_3 * educ
beta_1 <- coef(lwage)['female']
beta_3 <- coef(lwage)['female:educ']
delta_1 <- beta_1 + beta_3 * 12.5
delta_2 <- beta_1 + beta_3 * 0
c(delta_1,delta_2)
delta_1 - delta_2
# 2) 
educ0 <- with(wage1,educ-12.5)
lwage2 <- lm(lwage~female+educ+female*educ0+exper+expersq+tenure+tenursq,wage1)
summary(lwage2)
coef(lwage)['female2']
summary(lwage2)$coef[2,2]
# 3) 
summary(lwage2)$coef[2,4]
hist(educ0)

# C8 
help(loanapp)
# 2)
approve1 <- lm(approve~white,loanapp)
summary(approve1)
coef(approve1)['white']
# 3)
approve2 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,loanapp)
summary(approve2)
coef(approve2)['white']
# 4)
approve3 <- lm(approve~white+hrat+obrat+white*obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,loanapp)
summary(approve3)
coef(approve3)['white:obrat']
# 5) when obrat = 32
obrat0 <- with(loanapp,obrat-32)
approve4 <- lm(approve~white+hrat+obrat+white*obrat0+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,loanapp)
summary(approve4)
beta_1 <- coef(approve4)['white']
se_1 <- summary(approve4)$coef[2,2]
t_95 <- qnorm(0.975)
c(beta_1-t_95*se_1,beta_1+t_95*se_1)

# C9 
help(k401ksubs)
# 1)
table(k401ksubs$e401k)
nrow(subset(k401ksubs,e401k==1))/length(k401ksubs$e401k)
# 2)
e401k <- lm(e401k~inc+incsq+age+agesq+male,k401ksubs)
summary(e401k)
# 4)
e401k_fit <- fitted(e401k,e401k)
summary(e401k_fit)
hist(e401k_fit)
# 5)
labels <- c(0,0.5,1)
e401k_0 <- cut(e401k_fit,labels)
table(e401k_0)
e401k_1 <- (e401k_0=="(0.5,1]") + 0
table(e401k_1)
# 6)
data <- cbind(k401ksubs,e401k_1)
nrow(subset(data,e401k==0&e401k_1==0))/nrow(subset(k401ksubs,e401k==0))
nrow(subset(data,e401k==1&e401k_1==1))/nrow(subset(k401ksubs,e401k==1))
# 8)
e401k2 <- lm(e401k~inc+incsq+age+agesq+male+pira,k401ksubs)
summary(e401k2)
coef(e401k2)['pira']

# C10 
help(nbasal)
# 1) use centers as the base group
points <- lm(points~exper+expersq+guard+forward,nbasal)
summary(points)
# 3)
coef(points)['guard']
summary(points)$coef[4,4]
# 4)
points2 <- lm(points~exper+expersq+guard+forward+marr,nbasal)
summary(points2)
coef(points2)['marr']
# 5)
points3 <- lm(points~exper+expersq+guard+forward+marr+marr*exper+marr*expersq,nbasal)
summary(points3)
# 6)
assists <- lm(assists~exper+expersq+guard+forward+marr,nbasal)
summary(assists)
coef(points2)
coef(assists)

# C11 
help(k401ksubs)
# 1)
summary(k401ksubs$nettfa)
sd(k401ksubs$nettfa)
# 2)
nettfa <- lm(nettfa~e401k,k401ksubs)
summary(nettfa)
coef(nettfa)['e401k']
# 3)
nettfa1 <- lm(nettfa~inc+incsq+age+agesq+e401k,k401ksubs)
summary(nettfa1)
coef(nettfa1)['e401k']
# 4)
age0 <- k401ksubs$age - 41
age0sq <- (k401ksubs$age - 41)^2
nettfa2 <- lm(nettfa~inc+incsq+age+agesq+e401k+e401k*age0+e401k*age0sq,k401ksubs)
summary(nettfa2)
coef(nettfa2)['e401k:age0']
summary(nettfa2)$coef
# 5)
coef(nettfa1)['e401k']
coef(nettfa2)['e401k']
# 6) 
table(k401ksubs$fsize)
labels <- c(0,1,2,3,4,5)
fsize <- cut(k401ksubs$fsize,labels)
table(fsize)
fsize1 <- (fsize=="(0,1]") + 0
fsize2 <- (fsize=="(1,2]") + 0
fsize3 <- (fsize=="(2,3]") + 0
fsize4 <- (fsize=="(3,4]") + 0
fsize5 <- (fsize=="(4,5]") + 0 # base group
nettfa3 <- lm(nettfa~inc+incsq+age+agesq+e401k+fsize1+fsize2+fsize3+fsize4,k401ksubs)
summary(nettfa3)
SSR_r <- sum(residuals(nettfa1)^2)
SSR_ur <- sum(residuals(nettfa3)^2)
q <- 4
df1 <- summary(nettfa1)$fstatistic[3]
df2 <- summary(nettfa3)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/df2)
F > qf(0.975,df1,df2) # TRUE, H0 rejected 
F
# 7) 
# SSR_ur = SSR_1 + SSR_2 + SSR_3 + SSR_4 + SSR_5  
nettfa_ur <- lm(nettfa~inc+incsq+age+agesq+e401k,k401ksubs)
data_1 <- subset(k401ksubs,fsize1==1)
nettfa_1 <- lm(nettfa~inc+incsq+age+agesq+e401k,data_1)
data_2 <- subset(k401ksubs,fsize2==1)
nettfa_2 <- lm(nettfa~inc+incsq+age+agesq+e401k,data_2)
data_3 <- subset(k401ksubs,fsize3==1)
nettfa_3 <- lm(nettfa~inc+incsq+age+agesq+e401k,data_3)
data_4 <- subset(k401ksubs,fsize4==1)
nettfa_4 <- lm(nettfa~inc+incsq+age+agesq+e401k,data_4)
data_5 <- subset(k401ksubs,fsize5==1)
nettfa_5 <- lm(nettfa~inc+incsq+age+agesq+e401k,data_5)
SSR_ur <- sum(residuals(nettfa_ur)^2)
SSR_1 <- sum(residuals(nettfa_1)^2)
SSR_2 <- sum(residuals(nettfa_2)^2)
SSR_3 <- sum(residuals(nettfa_3)^2)
SSR_4 <- sum(residuals(nettfa_4)^2)
SSR_5 <- sum(residuals(nettfa_5)^2)
SSR_all <- SSR_1+SSR_2+SSR_3+SSR_4+SSR_5
n <- length(k401ksubs$e401k)
df <- n - 30
summary(nettfa_ur)$fstatistic
k <- 20
F <- (SSR_ur-SSR_all)/(SSR_all) * (df-2*(k))/(k)
F

# C12 
help(beauty)
# 1) 
length(beauty$female)
with(beauty,table(looks,female))
f <- nrow(subset(beauty,female==1))
m <- nrow(subset(beauty,female==0))
f_1 <- nrow(subset(beauty,female==1&looks>3))
m_1 <- nrow(subset(beauty,female==0&looks>3))
f_1/f
m_1/m
f_2 <- nrow(subset(beauty,female==1&looks<3))
m_2 <- nrow(subset(beauty,female==0&looks<3))
f_2/f
m_2/m
# 2) 
abvavg <- lm(abvavg~female,beauty)
summary(abvavg)
coef(abvavg)['female']
summary(abvavg)$coef[2,4]
# 3) 
data_f <- subset(beauty,female==1)
lwage_f <- lm(lwage~belavg+abvavg,data_f)
summary(lwage_f)$coef
coef(lwage_f)['belavg']
data_m <- subset(beauty,female==0)
lwage_m <- lm(lwage~belavg+abvavg,data_m)
summary(lwage_m)$coef
coef(lwage_m)['belavg']
# 4) 
coef(lwage_f)['abvavg']
summary(lwage_f)$coef[3,4] 
# 5) 
lwage_f2 <- lm(lwage~belavg+abvavg+educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service,data_f)
summary(lwage_f2)
lwage_m2 <- lm(lwage~belavg+abvavg+educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service,data_m)
summary(lwage_m2)
c(coef(lwage_f)['belavg'],coef(lwage_f2)['belavg'])
c(coef(lwage_f)['abvavg'],coef(lwage_f2)['abvavg'])
c(coef(lwage_m)['belavg'],coef(lwage_m2)['belavg'])
c(coef(lwage_m)['abvavg'],coef(lwage_m2)['abvavg'])
# 6) 
lwage_2 <- lm(lwage~belavg+abvavg+educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service,beauty)
summary(lwage_2)
SSR_f <- sum(residuals(lwage_f2)^2)
SSR_m <- sum(residuals(lwage_m2)^2)
SSR_p <- sum(residuals(lwage_2)^2)
SSR_f
SSR_m
SSR_p
n <- length(beauty$wage)
k <- summary(lwage_2)$fstatistic[2]
df <- summary(lwage_2)$fstatistic[3]
F <- (SSR_p-(SSR_f+SSR_m))/(SSR_f+SSR_m) * (n-2*(k+1))/(k+1)
F_test <- qf(0.95,k,df)
F > F_test #TRUE
F

# C13 
help(apple)
# 1) 
summary(apple$ecolbs)
ecobuy <- (apple$ecolbs==0) + 0
table(ecobuy)
data <- cbind(apple,ecobuy)
nrow(subset(data,ecobuy==0)) / length(data$ecobuy)
# 2) 
ecobuy1 <- lm(ecobuy~ecoprc+regprc+faminc+hhsize+educ+age,data)
summary(ecobuy1)
# 3) beta_3/4/5/6 = 0
ecobuy2 <- lm(ecobuy~ecoprc+regprc,data)
summary(ecobuy2)
SSR_r <- sum(residuals(ecobuy2)^2)
SSR_ur <- sum(residuals(ecobuy1)^2)
n <- length(data$ecobuy)
df <- summary(ecobuy1)$fstatistic[3]
q <- 4
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
F_test <- qf(0.95,q,df)
F > F_test # TRUE, H0 rejected
F
# 4) 
lfaminc <- log(data$faminc)
ecobuy3 <- lm(ecobuy~ecoprc+regprc+lfaminc+hhsize+educ+age,data)
summary(ecobuy3)
summary(ecobuy1)$r.squared
summary(ecobuy3)$r.squared
# 5) 
efit <- fitted(ecobuy3,ecobuy)
summary(efit)
# 6) 
data2 <- cbind(data,efit)
pred1 <- nrow(subset(data2,ecobuy==1&efit>0.5)) 
pred0 <- nrow(subset(data2,ecobuy==0&efit<0.5)) 
ecobuy1 <- nrow(subset(data2,ecobuy==1))
ecobuy0 <- nrow(subset(data2,ecobuy==0))
pred1 / ecobuy1 
pred0 / ecobuy0 
(pred1+pred0) / (ecobuy0+ecobuy1)

# C14 
help(charity)
# 1) 
respond <- lm(respond~resplast+avggift,charity)
summary(respond)
coef(respond)['resplast']
# 2) 
coef(respond)['avggift']
summary(respond)$coef[3,4]
# 3) 
respond2 <- lm(respond~resplast+avggift+propresp,charity)
summary(respond2)
coef(respond2)['propresp']
summary(respond2)$coef[4,4]
# 4) 
coef(respond)['resplast']
coef(respond2)['resplast']
# 5) 
respond3 <- lm(respond~resplast+avggift+propresp+mailsyear,charity)
summary(respond3)
coef(respond3)['mailsyear']
summary(respond3)$coef[5,4]
hist(charity$mailsyear)

# C15 
# 1) 
help(fertil2)
summary(fertil2$children)
nrow(subset(fertil2,children==mean(fertil2$children)))
# 2) 
nrow(subset(fertil2,electric==1)) / length(fertil2$electric)
# 3) 
with(subset(fertil2,electric==1),mean(children))
with(subset(fertil2,electric==0),mean(children))
children1 <- lm(children~electric,fertil2)
summary(children1)
coef(children1)['electric']
# 5) 
children2 <- lm(children~electric+agesq+urban+spirit+protest+catholic,fertil2)
summary(children2)
summary(children2)
coef(children2)['electric']
# 6) 
children3 <- lm(children~electric+electric*educ+agesq+urban+spirit+protest+catholic,fertil2)
summary(children3)
coef(children3)['electric']
# 7) 
educ_0 <- fertil2$educ - 7
children4 <- lm(children~electric+electric*educ_0+agesq+urban+spirit+protest+catholic,fertil2)
summary(children4)
coef(children3)['electric']
coef(children4)['electric']

# C16
help(catholic)
# 1) 
table(catholic$cathhs)
nrow(subset(catholic,cathhs==1)) / length(catholic$id)
mean(catholic$cathhs)
# 2) 
matn12 <- lm(math12~cathhs,catholic)
summary(matn12)
coef(matn12)['cathhs']
# 3) 
matn12_2 <- lm(math12~cathhs+lfaminc+motheduc+fatheduc,catholic)
summary(matn12_2)
coef(matn12)['cathhs']
coef(matn12_2)['cathhs']
# 5) 
matn12_3 <- lm(math12~cathhs+lfaminc+motheduc+fatheduc+cathhs*lfaminc+cathhs*motheduc+cathhs*fatheduc,catholic)
summary(matn12_3)
summary(matn12_3)$coef
SSR_r <- sum(residuals(matn12_2)^2)
SSR_ur <- sum(residuals(matn12_3)^2)
n <- length(catholic$id)
q <- 3
df <- summary(matn12_3)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
F_test <- qf(0.95,q,df)
F > F_test # FALSE
F
# 6) 
coef(matn12_3)['cathhs']

# C17
help(jtrain98)
# 1) 
table(jtrain98$unem98)
nrow(subset(jtrain98,unem98==1)) / length(jtrain98$unem98)
table(jtrain98$unem96)
nrow(subset(jtrain98,unem96==1)) / length(jtrain98$unem96)
# 2) 
unem98 <- lm(unem98~train,jtrain98)
summary(unem98)
coef(unem98)['train']
summary(unem98)$coef[2,4]
# 3) 
unem98_2 <- lm(unem98~train+earn96+educ+age+married,jtrain98)
summary(unem98_2)
coef(unem98_2)['train']
# 4) 
earn96_0 <- with(jtrain98,earn96-mean(earn96))
educ_0 <- with(jtrain98,educ-mean(educ))
age_0 <- with(jtrain98,age-mean(age))
married_0 <- with(jtrain98,married-mean(married))
unem98_3 <- lm(unem98~train+earn96+educ+age+married+train*earn96_0+train*educ_0+train*age_0+train*married_0,jtrain98)
summary(unem98_3)
coef(unem98_2)['train']
coef(unem98_3)['train']
# 5)
SSR_r <- sum(residuals(unem98_2)^2)
SSR_ur <- sum(residuals(unem98_3)^2)
n <- length(jtrain98$train)
q <- 4
df <- summary(unem98_3)$fstatistic[3]
F <- ((SSR_r-SSR_ur)/q)/(SSR_ur/(df))
F_test <- qf(0.95,q,df)
F > F_test # FALSE
F
# 6)
data <- cbind(jtrain98,earn96_0,educ_0,age_0,married_0)
data1 <- subset(data,unem98==1)
unem98_4 <- lm(unem98~train+earn96+educ+age+married+train*earn96_0+train*educ_0+train*age_0+train*married_0,data1)
data2 <- subset(data,unem98==0)
unem98_5 <- lm(unem98~train+earn96+educ+age+married+train*earn96_0+train*educ_0+train*age_0+train*married_0,data2)
SSR_1 <- sum(residuals(unem98_4)^2)
SSR_2 <- sum(residuals(unem98_5)^2)
SSR_p <- sum(residuals(unem98_3)^2)
SSR_1
SSR_2
SSR_p
k <- 9
df <- summary(unem98_3)$fstatistic[3]
F <- (SSR_p-(SSR_1+SSR_2))/(SSR_1+SSR_2) * (n-2*(k+1))/(k+1)
F_test <- qf(0.95,k,df)
F > F_test #TRUE
F

F <- (SSR_p-(SSR_1+SSR_2))/(SSR_1+SSR_2) * (n-2*(k))/(k)
F > F_test #TRUE
F

