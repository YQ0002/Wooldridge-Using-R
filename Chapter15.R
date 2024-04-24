# Chapter 15
rm(list=ls())
library(wooldridge)

# Methods for time-constant omitted variables
# Fixed effects(FE) and first differencing(FD) 

# Methods for time-varying omitted variables
# The method of instrumental variables (IV)
# The method of two stage least squares (2SLS or TSLS)

# 15-1 Motivation: Omitted Variables in a Simple Regression Model

# Options 1. Ignore but get biased and inconsistent estimators
# Options 2. Find a proxy variable for substitute
# Options 3. Assume omitted variables do not change over time, use FE and FD

# Options 4. Leave the unobserved variable in the error term, recognize it with IV
# y = beta_0 + beta_1 * x + u
# Cov(x, u) ≠ 0
# Assuming an observable variable z that satisfies: 
# (1) Cov(z,u) = 0, Instrument exogeneity
# (2) Cov(z,x) ≠ 0, Instrument relevance
# Call z an Instrumental Variable for x, or simplified as an instrument for x

# x = pai_0 + pai_1 * z + v
# pai_1 = Cov(z,x)/Var(z)
# Assuming(2) exists when pai_1 ≠ 0
# H0: pai_1 = 0

# Cov(z,y) = beta_1 * Cov(z,x) + Cov(z,u), (1), (2)
# beta_1 = Cov(z,y) / Cov(z,x)
# beta_1_hat = sum((z_i - z_mean)*(y_i - y_mean))/ sum((z_i - z_mean)*(x_i - x_mean))
# beta_0_hat = y_mean - beta_1_hat * x_mean

# 15-1a Statistical Inference with the IV Estimator
# (3) E(u^2|z) = σ^2 = Var(u)

# Asymptotic variance of beta_1_hat is
# (σ^2) / (n * (σ_x)^2 * (p_x,z)^2)
# The population variance of x: (σ_x)^2
# The population correlation between x and z: (p_x,z)^2
# The sample size: n

# The IV residuals:
# u_i_hat = y_i - beta_0_hat - beta_1_hat * x_i
# (σ_hat)^2 = (1 / (n-2)) * sum((u_i_hat)^2)
# the estimated asymptotic variance (also the square of beta_1_hat)
# (σ_hat)^2 / (SST_x * (R_x,z)^2)


# Example 15.1 Estimating the Return to Education for Married Women
help(mroz)
table(mroz$inlf)
mroz_lf <- subset(mroz, inlf==1)
lwage_ols <- lm(lwage~educ, mroz_lf)
summary(lwage_ols)
educ <- lm(educ~fatheduc, mroz_lf)
summary(educ)
# install.packages("ivreg")
library(ivreg)
lwage_IV <- ivreg(lwage~ educ|fatheduc, data = mroz_lf)
summary(lwage_IV)
library(stargazer)
stargazer(lwage_ols, lwage_IV, type = "text")
help("ivreg")


educ1 <- lm(educ~educ+fatheduc, mroz_lf)
educ2 <- lm(educ~1, mroz_lf)
stargazer(educ1, educ2, type = "text")
anova(educ2, educ1)

# Example 15.2 Estimating the Return to Education for Men
help(wage2)
educ <- lm(educ~ sibs, wage2)
summary(educ)
lwage_IV <- ivreg(lwage~educ|sibs, data = wage2)
summary(lwage_IV)
lwage_ols <- lm(lwage~educ, wage2)
summary(lwage_ols)
stargazer(lwage_ols, lwage_IV, type = "text")

educ1 <- lm(educ~ sibs, wage2)
educ2 <- lm(educ~ 1, wage2)
anova(educ2, educ1)


# 15-1b Properties of IV with a Poor Instrumental Variable
# plim beta_1,IV_hat = beta_1 + Corr(z,u)/Corr(z,x) * σ_u/σ_x
# plim beta_1,OLS_hat = beta_1 + Corr(x,u) * σ_u/σ_x

# Example 15.3 Estimating the Effect of Smoking on Birth Weight
help(bwght)
lbwght_ols <- lm(lbwght~packs, bwght)
summary(lbwght_ols)
packs <- lm(packs~cigprice, bwght)
summary(packs)
lbwght_IV <- ivreg(lbwght~packs|cigprice, data = bwght)
stargazer(lbwght_ols, lbwght_IV, type = "text")

packs1 <- lm(packs~cigprice, bwght)
packs2 <- lm(packs~1, bwght)
anova(packs2, packs1)

cor(bwght$packs, bwght$cigprice)

# 15-1c Computing R-Squared after IV Estimation
# SSR for IV can actually be larger than SST,
# which makes R^2 for IV usually can not be explained 
# R^2 = 1 - SSR/SST

# 15-2 IV Estimation of the Multiple Regression Model

# The structural equation: 
# y_1 = beta_0 + beta_1 * y_2 + beta_2 * z_1 + u_1
# Find IV z_2 for y_2, Assuming: 
# E(u_1) = 0, Cov(z_1, u_1) = 0, Cov(z_2, u_1) = 0

# sum(y_i1 - beta_0_hat - beta_1_hat * y_i2 - beta_2_hat * z_i1) = 0
# sum(z_i1 * (y_i1 - beta_0_hat - beta_1_hat * y_i2 - beta_2_hat * z_i1)） = 0
# sum(z_i2 * (y_i1 - beta_0_hat - beta_1_hat * y_i2 - beta_2_hat * z_i1)） = 0

# Write the endogenous explanatory variable as a linear function of the exogenous variables and an error term
# y_2 = pai_0 + pai_1 * z_1 + pai_2 * z_2 + v_2, 
# E(v_2) = 0, Cov(z_1, v_2) = 0, Cov(z_2, v_2) = 0, 
# pai_2 ≠ 0, from Cov(z_2, y_2) ≠ 0

# y_1 = beta_0 + beta_1 * y_2 + beta_2 * z_1 +...+ beta_k * z_(k-1) + u_1
# E(u_1) = 0, Cov(z_j, u_1) = 0, j = 1,2,...,k

# The reduced form for y2 is:
# y_2 = pai_0 + pai_1 * z_1 +...+ pai_(k-1) * z_(k-1) + v_2, 
# pai_k ≠ 0

# Example 15.4 Using College Proximity as an IV for Education
help(card)
lwage_ols <- lm(lwage~educ+exper+expersq+black+smsa+south+smsa66
                +reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669, card)
summary(lwage_ols)

educ1 <- lm(educ~nearc4+exper+expersq+black+smsa+south+smsa66
           +reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669, card)
summary(educ1)
educ2 <- lm(educ~exper+expersq+black+smsa+south+smsa66
           +reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669, card)
anova(educ2, educ1)

lwage_IV <- ivreg(lwage~educ+exper+expersq+black+smsa+south+smsa66
                     +reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669
                  | nearc4+exper+expersq+black+smsa+south+smsa66
                  +reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669, data = card)
summary(lwage_IV)
library(stargazer)
stargazer(lwage_ols, lwage_IV, type = "text")

# 15-3 Two Stage Least Squares

# 15-3a A Single Explanatory Variable

# Two stage least squares(2SLS) estimator

# To find the best IV, choose the linear combination of exogenous variables that is most highly correlated with y_2
# y_2 = pai_0 + pai_1 * z_1 + pai_2 * z_2 + pai_3 * z_3 + v_2
# E(v_2) = 0, Cov(z_1, v_2) = 0, Cov(z_2, v_2) = 0, Cov(z_3, v_2) = 0
# The linear combination of the z_j: 
# y_2* = pai_0 + pai_1 * z_1 + pai_2 * z_2 + pai_3 * z_3
# pai_2 ≠ 0 or pai_3 ≠ 0 as H1; 
# H0: pai_2 = pai_3 = 0 

# Regress y_2 on z_1, z_2 and z_3 to obtain the fitted value: 
# y_2_hat = pai_0_hat + pai_1_hat * z_1 + pai_2_hat * z_2 + pai_3_hat * z_3 
# Taking y_2_hat as IV of y_2, 
# sum(y_i2_hat * (y_i1 - beta_0_hat - beta_1_hat * y_i2 - beta_2_hat * z_i1)) = 0

# y_2 = y_2* + v_2
# y_1 = beta_0 + beta_1 * y_2 + beta_2 * z_1 + u_1
# y_1 = beta_0 + beta_1 * y_2* + beta_2 * z_1 + u_1 + beta_1 * v_2
# E(u_1 + beta_1 * v_2) = 0, Cor(y_2* ,z) = 0

# Example 15.5 Return to Education for Working Women
help(mroz)
mroz_fe <- subset(mroz, inlf==1)
educ_1 <- lm(educ~exper+expersq+motheduc+fatheduc, mroz_fe)
summary(educ_1)
educ_2 <- lm(educ~exper+expersq, mroz_fe)
summary(educ_2)
anova(educ_2, educ_1)
lwage_2SLS <- ivreg(lwage~educ+exper+expersq 
                    | exper+expersq+motheduc+fatheduc,
                    data = mroz)
summary(lwage_2SLS)
stargazer(lwage_2SLS, type = "text")

# 15-3b Multicollinearity and 2SLS

# Multicollinearity can be more serious with 2SLS. 
# Approximated (asymptotic) variance of the 2SLS estimator of beta_1 is: 
# σ^2 / (SST_2_hat * (1-(R_2_hat)^2))

# 15-3c Detecting Weak Instruments
# 15-3d Multiple Endogenous Explanatory Variables
# 15-3e Testing Multiple Hypotheses after 2SLS Estimation

# 15-4 IV Solutions to Errors-in-Variables Problems

# x_1 = x_1* + e_1
# y = beta_0 + beta_1 * x_1* + beta_2 * x_2 + u
# y = beta_0 + beta_1 * x_1 + beta_2 * x_2 + (u - beta_1 * e_1)

# Example 15.6 Using Two Test Scores as Indicators of Ability
help(wage2)
lwage_ols <- lm(lwage~educ+exper+tenure+married+south+urban+black+IQ, wage2)
summary(lwage_ols)
IQ <- lm(IQ~KWW+educ+exper+tenure+married+south+urban+black, wage2)
summary(IQ)
lwage_2SLS <- ivreg(lwage~educ+exper+tenure+married+south+urban+black+IQ
                    | KWW+educ+exper+tenure+married+south+urban+black, 
                    data = wage2)
summary(lwage_2SLS)
summary(lwage_2SLS)$coef["educ",]
stargazer(lwage_ols, lwage_2SLS, type = "text")

# 15-5 Testing for Endogeneity and Testing Overidentifying Restrictions

# 15-5a Testing for Endogeneity

# 2SLS can be less efficient than OLS when explanatory variables are exogenous.

# Example 15.7



# 15-5b Testing Overidentification Restrictions




