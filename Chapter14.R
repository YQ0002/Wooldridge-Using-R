# Chapter 14 
rm(list=ls())
library(wooldridge)

# 14-1 Fixed Effects Estimation
# y_it = beta_1 * x_it + a_i + u_it, t = 1,2,...,T
# y_i_mean = beta_1 * x_it_mean + a_i + u_i_mean
# y_it - y_i_mean = beta_1 * (x_it - x_it_mean) + u_it + u_i_mean, t = 1,2,...,T
# Set y_it_dots = y_it - y_i_mean is the time-demeaned data, so do x and u
# y_it_dots = beta_1 * x_it_dots + u_it_dots

# Assumptions for Fixed Effects

# Assumption FE.1
# For each i the model is 
# y_it = beta_1 * x_it1 + ... + beta_k * x_itk + a_i + u_it, t = 1,2,...,T
# where the beta_j are the prameters to estimate and a_i is the unobserved effect
# Assumption FE.2: 
# random sample from the cross section
# Assumption FE.3
# Explanatory variables change over time, no perfect linear relationship
# Assumption FE.4
# E(u_it|X_i, a_i) = 0
# Assumption FE.5
# Var(u_it|X_i, a_i) = Var(u_it) = (σ_u)^2, t = 1,2,...,T
# Assumption FE.6
# Cov(u_it,u_is|X_i,a_i) = 0
# Assumption FE.7
# Conditional on X_i and a_i, the u_it are independent
# and indentically distributed as Normal(0,(σ_u)^2)

# Degree of freedom: 
# df = NT - N - k = N(T-1) - k
# NT: total observations 
# k: independent variables

# Example 14.1
help(jtrain)
table(jtrain$year)
# install.packages("plm")
library(plm)
lscrap1 <- plm(lscrap ~ d88+d89+grant+grant_1,
            data = jtrain,
            index= c("fcode", "year"),
            model = "within",
            effect = "individual")
summary(lscrap1)
help(plm)
summary(lscrap1)$coef[4,1]
# Δlscrap/Δgrant_1 = -0.422
# Δscrap/Δgrant = exp(-0.422)
exp(-0.422) - 1 # -0.344266
# Balanced Panel: n = 54, T = 3, N = 162
54 * (3-1) - 4 # df = 104
lscrap2 <- plm(lscrap ~ d88 + d89 + grant,
               data = jtrain,
               index = c("fcode", "year"),
               model = "within",
               effect = "individual")
library(stargazer)
stargazer(lscrap1, lscrap2, type = "text")
help("stargazer")

stargazer(lscrap1, lscrap2, type = "html", out = "D://output.html")


# Example 14.2
help(wagepan)
table(wagepan$year)
lwage1 <- plm(lwage~married+union+d81+d82+d83+d84+d85+d86+d87+
               d81*educ+d82*educ+d83*educ+d84*educ+d85*educ+d86*educ+d87*educ, 
             data = wagepan, 
             index = c("nr", "year"),
             model = "within",
             effect = "individual")
summary(lwage1)
# H0: interaction terms have no effects
lwage2 <- plm(lwage~married+union+d81+d82+d83+d84+d85+d86+d87,
              data = wagepan, 
              index = c("nr", "year"),
              model = "within",
              effect = "individual")
summary(lwage2)
pFtest(lwage1, lwage2)
# p = 0.2787, fail to reject H0 at the 5% level

lwage3 <- plm(lwage~married+union+year,
              data = wagepan, 
              index = c("nr", "year"),
              model = "within",
              effect = "twoway")
stargazer(lwage2, lwage3, type = "text")
help(plm)


# 14-1a The Dummy Variable Regression
# a_i_hat = y_i_mean - beta_1_hat * x_i1_mean - ... - beta_k_hat * x_ik_mean, i = 1,2,...,N

# 14-1b Fixed Effects(FE) or First Differencing(FD)
# When T = 2, FE = FD
# When T ≥ 3, u_it are serially uncorrelated(FE.6), FE is better
# When T is large but N is not(eg. N = 20 and T = 30), FE must be carefully applied

# 14-1c Fixed Effects with Unbalanced Panels
# Example 14.3
help(jtrain)
lscrap3 <- plm(lscrap ~ d88+d89+grant+grant_1+lsales+lemploy,
               data = jtrain,
               index= c("fcode", "year"),
               model = "within",
               effect = "individual")
stargazer(lscrap3, type = "text")
stargazer(lscrap1, lscrap3, type = "text")
pFtest(lscrap3, lscrap1)

# 14-2 Random Effects Models
# y_it = beta_1 * x_it + a_i + u_it, t = 1,2,...,T
# Cov(x_itj, a_i) = 0, t = 1,2,...,T; j = 1,2,...,k
# Say: v_it  = a_i + u_it
# y_it = beta_0 + beta_1 * x_it1 + ... + beta_k * x_itk + v_it
# Under the random effects assumptions,
# Corr(v_it, v_is) = (σ_a)^2 / ((σ_a)^2 + (σ_u)^2), t ≠ s
# (σ_a)^2 = Var(a_i), (σ_u)^2 = Var(u_it)

# GLS transformation
# Define θ = 1 - [(σ_u)^2 / ((σ_u)^2+T*(σ_a)^2)]^(1/2)
# y_it - θ*y_i_mean = beta_0*(1-θ) + beta_1*(x_it1 - θ*x_i1_mean) + ... + beta_k*(x_itk - θ*x_ik_mean) + (v_it - θ*v_i_mean)

# Relate RE with Pooled OLS and FE
# When θ = 0, Pooled OLS is obtained
# When θ = 1, FE is obtained
# If θ_hat is close to 0, RE estimates will be close to Pooled OLS

# Rewrite the quasi-demeaned error as: 
# v_it - θ*v_i_mean = (1-θ)*a_i + u_it + θ*u_i_mean

# Assumptions for Random Effects

# Assumption RE.1 = Assumption FE.1
# Assumption RE.2 = Assumption FE.2
# Assumption RE.3
# No perfect linear relationships among the explanatory variables
# Assumption RE.4
# E(u_it|X_i, a_i) = 0, also E(a_i|X_i) = beta_0
# Assumption RE.5
# Var(u_it|X_i, a_i) = Var(u_it) = (σ_u)^2, also Var(a_i|X_i) = (σ_a)^2, t = 1,2,...,T
# Assumption RE.6 = Assumption FE.6


# Example 14.4
help(wagepan)
lwage_ols <- lm(lwage~educ+black+hisp+exper+expersq+married+union+d81+d82+d83+d84+d85+d86+d87, wagepan)
summary(lwage_ols)
lwage_re1 <- plm(lwage~educ+black+hisp+exper+expersq+married+union+d81+d82+d83+d84+d85+d86+d87, 
              data = wagepan,
              index = c("nr", "year"),
              model = "random")
summary(lwage_re1)
lwage_fe2 <- plm(lwage~exper+expersq+married+union+d81+d82+d83+d84+d85+d86+d87, 
              data = wagepan,
              index = c("nr", "year"),
              model = "within",
              effect = "individual")
summary(lwage_re2)
stargazer(lwage_ols, lwage_re1, lwage_fe2, type = "text")

summary(lwage_fe2)$coef
(-0.132146418 )/(2*-0.005185498)


# 14-2a Random Effects or Pooled OLS
# H: (σ_a)^2 = 0
# If no unobserved effect, use OLS
# Both RE and POLS are inconsistent if a_i is correlated with explanatory variables

# 14-2b Random Effects or Fixed Effects
# If the key Explanatory variable is constant over time, FE should not be used
# If the key variable is set experimentally, RE would be appropriate
# It's usually inappropriate to determine using FE or RE only based on a_i
# Hausman test, H0: Cov(x_itj, a_i) = 0, RE is consistent
help(wagepan)
lwage_fe <- plm(lwage~educ+black+hisp+exper+expersq+married+union+d81+d82+d83+d84+d85+d86+d87, 
                 data = wagepan,
                 index = c("nr", "year"),
                 model = "within",
                 effect = "individual")
lwage_re <- plm(lwage~educ+black+hisp+exper+expersq+married+union+d81+d82+d83+d84+d85+d86+d87, 
                 data = wagepan,
                 index = c("nr", "year"),
                 model = "random")

stargazer(lwage_fe, lwage_re, type = "text")
phtest(lwage_fe, lwage_re) # p-value = 0.000448, H0 rejected
help(phtest)

# 14-3 The Correlated Random Effects Approach

# {x_it: t=1,2,...,T}
# x_i_mean = sum(x_it)/T
# a_i = alpha + gama * x_i_mean + r_i
# Cov(x_i_mean|r_i) = 0
# y_it = beta * x_it + alpha + gama * x_i_mean + r_i + u_it
# y_it = alpha + beta * x_it + gama * x_i_mean + r_i + u_it
# beta_CRE_hat = beta_FE_hat

# CRE provides a simple, formal way of choosing from FE and RE
# Time variable: x_it1, x_it2,..., x_itk
# Time averages: x_it1_mean,..., x_itk_mean
# Time constant variables: z_i1,...,z_im
# Time dummies: d2_t,...,dT_t
# y_it = alpha_1 + alpha_2*d2_t + ... + alpha_T * dT_t + beta_1 * x_it1 + ... + beta_k * x_itk
#        + gama_1 * x_i1_mean + ... + gama_k * x_ik_mean + sigma_1 * z_i1 + ... + sigma_m * z_im + r_it + u_it
# t = 1,2,...,T

# For RE/POLS estimates, obtain
# beta_(CRE,j)_hat = beta_(FE,j)_hat, j = 1,2,...,k
# alpha_(CRE,t)_hat = alpha_(FE,t)_hat, t = 1,2,...,T
# H0: gama_1 = gama_2 = ... = gama_k = 0

# CRE allowing lags, squares and interactions among other variables

# 14-4 General Policy Analysis with Panel Data
# heterogeneous trend model(or random trend model)
# y_it = eta_1 + alpha_2*d2_t + ... + alpha_T * dT_t + beta * w_it1 + x_it * psi + a_i + g_i*t + u_it
# t = 1,2,...,T
# delta_y_it = alpha_2*delta_d2_t + ... + alpha_T*delta_dT_t + beta*delta_w_it1 + delta_x_it*psi + g_i + delta_u_it
# t = 1,2,...,T
# g_i*t - g_i*(t-1) = g_i

# 14-5 Applying Panel Data Methods to Other Data Structures


# Problems 
# 1

# GMM









