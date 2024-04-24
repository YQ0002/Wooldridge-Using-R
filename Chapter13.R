# Chapter 13 
rm(list=ls())
library(wooldridge)

# 13-1 Pooling Independent Cross Sections across Time
# Example 13.1
help(fertil1)
kids <- lm(kids~educ+age+agesq+black+east+northcen+west+farm+othrural+town+smcity+
             y74+y76+y78+y80+y82+y84, fertil1)
summary(kids)
# H0: Homoscedasticity
library(lmtest)
# bp test, p-value = 6.098e-06, H0 rejected
bptest(kids) 
# white test, p-value = 9.56e-07, H0 rejected
bptest(kids, ~fitted(kids)+I(fitted(kids)^2)) 

# Example 13.2
help(cps78_85)
lwage <- lm(lwage~y85+educ+y85*educ+exper+expersq+
              union+female+y85*female, cps78_85)
summary(lwage)
# Δlwage/Δeduc = beta_2 + beta_3 * y85
coef(lwage)["educ"]
coef(lwage)["educ"] + coef(lwage)["y85:educ"]
summary(lwage)$coef["y85:educ",]
# Δlwage/Δfemale = beta_7 + beta_8 * y85
coef(lwage)["female"]
coef(lwage)["female"] + coef(lwage)["y85:female"]
summary(lwage)$coef["y85:female",]

# 13-2 Policy Analysis with Pooled Cross Sections
# Example 13.4
help(injury)
table(injury$ky)
ky_injury <- subset(injury, ky==1)
ldurat <- lm(ldurat~afchnge+highearn+afchnge*highearn, ky_injury)
summary(ldurat)
# Δldurat/Δhighearn = beta_2 + beta_3 * afchnge
coef(ldurat)["afchnge:highearn"]
summary(ldurat)$coef["highearn",]

# 13-3 Two-Period Panel Data Analysis
help(crime2)
table(crime2$year)
crmrte1 <- lm(crmrte~unem, subset(crime2, year==87))
summary(crmrte1)
# y_it = beta_0 + σ_0 * d2_t + beta_1 * x_it + a_i + u_it, t = 1,2
# the fixed effects: a_i (or unobserved effects)
# the composite error: v_it = a_i + u_it
# y_it = beta_0 + σ_0 * d2_t + beta_1 * x_it + v_it, t = 1,2
crmrte2 <- lm(crmrte~d87+unem, crime2)
summary(crmrte2)
# y_i2 = (beta_0 + σ_0) + beta_1 * x_i2 + a_i + u_i2, t = 2
# y_i1 = beta_0 + beta_1 * x_i1 + a_i + u_i1, t = 1
# Assuming E(Δu_i|Δx_i) = 0
# (y_i2 - y_i1) = σ_0 + beta_1 * (x_i2 - x_i1) + (u_i2 - u_i1)
# Δy_i = σ_0 + beta_1 * Δx_i + Δu_i
crime2_82 <- subset(crime2, year==82)
crime2_87 <- subset(crime2, year==87)
crmrte_delta <- c(crime2_87$crmrte - crime2_82$crmrte)
unem_delta <- c(crime2_87$unem - crime2_82$unem)
crmrte3 <- lm(crmrte_delta~unem_delta, crime2)
summary(crmrte3)

# Example 13.5
help(slp75_81)
length(slp75_81$age75)
slpnap1 <- lm(slpnap75~totwrk75+educ75+marr75+yngkid75+gdhlth75, slp75_81)
summary(slpnap1)
slpnap2 <- lm(slpnap81~totwrk81+educ81+marr81+yngkid81+gdhlth81, slp75_81)
summary(slpnap2)
# slpnap_it = beta_0 + σ_0 * d_81_t + beta_1 * totwrk_it + beta_2 * educ_it + beta_3 * marr_it + beta_4 * yngkid_it + beta_5 * gdhlth_it + a_i + u_it, t = 1,2
slpnap_delta <- with(slp75_81, slpnap81 - slpnap75)
totwrk_delta <- with(slp75_81, totwrk81 - totwrk75)
educ_delta <- with(slp75_81, educ81 - educ75)
marr_delta <- with(slp75_81, marr81 - marr75)
yngkid_delta <- with(slp75_81, yngkid81 - yngkid75)
gdhlth_delta <- with(slp75_81, gdhlth81 - gdhlth75)
slpnap3 <- lm(slpnap_delta~totwrk_delta+educ_delta+marr_delta+yngkid_delta+gdhlth_delta, slp75_81)
summary(slpnap3)
# H0: beta_2 = beta_3 = beta_4 = beta_5 = 0
slpnap4 <- lm(slpnap_delta~totwrk_delta, slp75_81)
summary(slpnap4)
library(lmtest)
anova(slpnap3, slpnap4)
# p = 0.4857, fail to reject H0 at the 5% level
help(anova)

# Example 13.6
help(crime3)
table(crime3$year)
# lcrime_it = beta_0 + σ_0 * d_78_t + beta_1 * clrprc1_it + beta_2 * clrprc2_it + a_i + u_it, t = 1,2
cirme3_72 <- subset(crime3, year==72)
cirme3_78 <- subset(crime3, year==78)
lcrime_delta <- c(cirme3_78$lcrime - cirme3_72$lcrime)
clrprc1_delta <- c(cirme3_78$clrprc1 - cirme3_72$clrprc1)
clrprc2_delta <- c(cirme3_78$clrprc2 - cirme3_72$clrprc2)
lcrime1 <- lm(lcrime_delta~clrprc1_delta+clrprc2_delta)
summary(lcrime1)

# 13-4 Policy Analysis with Two-Period Panel Data
help(jtrain)
table(jtrain$year)
# scrap_it = beta_0 + σ_0 * y_88_t + beta_1 * grant_it + a_i + u_it, t = 1,2
jtrain_87 <- subset(jtrain, year==1987)
jtrain_88 <- subset(jtrain, year==1988)
scrap_delta <- c(jtrain_88$scrap - jtrain_87$scrap)
grant_delta <- c(jtrain_88$grant - jtrain_87$grant)
scrap1 <- lm(scrap_delta~grant_delta)
summary(scrap1)
# Δscrap/Δgrant = -0.7394
lscrap_delta <- c(jtrain_88$lscrap - jtrain_87$lscrap)
lscrap1 <- lm(lscrap_delta~grant_delta)
summary(lscrap1)
library(stargazer)
stargazer(scrap1, lscrap1, type = "text")
# Δlscrap/Δgrant = -0.317
# Δscrap/Δgrant = exp(-0.317)
exp(-0.317) - 1 # -0.2716692

# Example 13.7
help(traffic1)
dthrte1 <- lm(cdthrte~copen+cadmn, traffic1)
summary(dthrte1)
table(traffic1$cadmn, traffic1$state)
coef(dthrte1)["copen"]

# 13-5 Differencing with More Than Two Time Periods

# y_it = beta_0 + σ_0 * d2_t + beta_1 * x_it + a_i + u_it, t = 1,2,3
# Cov(x_itj, u_is) = 0, for all t, s and j
# When t = 3
# Δy_it = σ_2*Δd2_t + σ_3*Δd3_t + beta_1*Δx_it1 + ...+ beta_k*Δx_itk + Δu_it
# Δy_it = a_0 + a_3*d3_t + beta_1*Δx_it1 +...+ beta_k*Δx_itk + Δu_it, t = 2,3
# Modle after first differencing: 
# Δy_it = a_0 + a_3*d3_t + a_4*d4_t + ... + a_t*dT_t + 
#         beta_1*Δx_it1 +...+ beta_k*Δx_itk + Δu_it, t = 2,3,...,T

# Example 13.8
help(ezunem)
table(ezunem$year)
# luclms = θ_t + beta_1 * ez_it + a_i + u_it
luclms <- lm(luclms~ez, ezunem)
summary(luclms)
# Δluclms = a_0 + a_1*d82_t + ...+ a_7*d88_t+ beta_1*Δez_it + Δu_it
table(ezunem$year, ezunem$cez)
guclms <- lm(guclms~d82+d83+d84+d85+d86+d87+d88+cez, ezunem)
summary(guclms)
# Δguclms/Δcez = -0.18188
# Δuclms/Δcez = exp(-0.18188)
exp(-0.18188) - 1 # -0.1662986
library(lmtest)
# bp test, p-value = 0.5459, Fail to reject H0
bptest(guclms)

# Example 13.9
help(crime4)
table(crime4$year)
clcrmrte <- lm(clcrmrte~d83+d84+d85+d86+d87+clprbarr+clprbcon+clprbpri+clavgsen+clpolpc, crime4)
summary(clcrmrte)
library(lmtest)
# bp test, p-value = 0.363, Fail to reject H0
bptest(clcrmrte)
# white test
clcrmrte_hat <- fitted(clcrmrte)
clcrmrte_hatsq <- clcrmrte_hat^2
u2_clcrmrte <- residuals(clcrmrte)^2
u2_LM <- lm(u2_clcrmrte~clcrmrte_hat+clcrmrte_hatsq)
summary(u2_LM)
# F-statistic: 75.48 on 2 and 537 DF,  p-value: < 2.2e-16











