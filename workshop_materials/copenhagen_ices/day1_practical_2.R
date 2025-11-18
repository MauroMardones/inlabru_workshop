## ----child="practicals/LM_ex_priors.qmd"--------------------------------------


library(dplyr)
library(INLA)
library(ggplot2)
library(patchwork)
library(inlabru)     


beta = c(2,0.5)
sd_error = 0.1

n = 100
x = rnorm(n)
y = beta[1] + beta[2] * x + rnorm(n, sd = sd_error)

df = data.frame(y = y, x = x)  


# Model 1
## -----------------------------------------------------------------------------
# Model components
cmp =  ~ -1 + beta_0(1) + 
  beta_1(x, model = "linear")
# Linear predictor
formula = y ~ Intercept + beta_1
# Observational model likelihood
lik =  bru_obs(formula = y ~.,
            family = "gaussian",
            data = df)
# Fit the Model
fit.lm = bru(cmp, lik)

##

xpred = seq(range(x)[1], range(x)[2], length.out = 100)
pred_data = expand.grid(x = xpred)
pred1 = predict(fit.lm, pred_data, formula = ~ beta_0 + beta_1) 


pred1 <- pred1$x %>%
  ggplot(aes(x=x,y=mean))+
  geom_line()+
  geom_ribbon(aes(x,
                  ymin = q0.025, 
                  ymax= q0.975), 
              alpha = 0.5) + 
  geom_point(data=df,aes(x=x,y=y))+
  theme_bw()

## -----------------------------------------------------------------------------


# Model 2
cmp1 =  ~-1 +  beta_0(1, prec.linear = 0.01) + 
  beta_1(x, model = "linear")
# Linear predictor
formula = y ~ Intercept + beta_1
# Observational model likelihood
lik =  bru_obs(formula = y ~.,
               family = "gaussian",
               data = df)
# Fit the Model
fit.lm1 = bru(cmp1, lik)



xpred = seq(range(x)[1], range(x)[2], length.out = 100)
pred_data = expand.grid(x = xpred)
predcmp2 = predict(fit.lm1, pred_data, formula = ~ beta_0 + beta_1) 


pred2 <- predcmp2 %>%
  ggplot(aes(x=x,y=mean))+
  geom_line()+
  geom_ribbon(aes(x,
                  ymin = q0.025, 
                  ymax= q0.975), 
              alpha = 0.5) + 
  geom_point(data=df,aes(x=x,y=y))+
  theme_bw()
# Change the precision for the linear effects
# 
# The precision for linear effects is set in the component definition. 
# For example, if we want to increase the precision to 0.01 for  
# we define the relative components as:


# Model 3 
cmp2 =  ~ -1 + 
  beta_0(1, prec.linear = 0.01) + 
  beta_1(x, model = "linear", prec.linear = 0.01)

# First we define the logGamma (0.01,0.01) prior 

prec.tau <- list(prec = list(prior = "loggamma",   # prior name
                             param = c(0.01, 0.01))) # prior values

lik2 =  bru_obs(formula = y ~.,
                family = "gaussian",
                data = df,
                control.family = list(hyper = prec.tau))
 
lm.fit2 = bru(cmp2, lik2) 

xpred = seq(range(x)[1], range(x)[2], length.out = 100)
pred_data = expand.grid(x = xpred)
predcmp3 = predict(lm.fit2, pred_data, formula = ~ beta_0 + beta_1) 


pred3 <- predcmp3 %>%
  ggplot(aes(x=x,y=mean))+
  geom_line()+
  geom_ribbon(aes(x,
                  ymin = q0.025, 
                  ymax= q0.975), 
              alpha = 0.5) + 
  geom_point(data=df,aes(x=x,y=y))+
  theme_bw()

egg::ggarrange(pred1, pred2, pred3,
                          ncol = 3, nrow = 1)
#inla.priors.used(lm.fit2)

plot(lm.fit2, "beta_0") + 
  plot(lm.fit2, "beta_1") +
  plot(fit.lm, "beta_0") +
  plot(fit.lm, "beta_1") +
  plot(fit.lm1, "beta_0") +
  plot(fit.lm1, "beta_1") +
  plot_layout(ncol = 2) & 
  theme_bw()
## ----lm.fit()## -----------------------------------------------------------------------------
#Change the prior for the precision of the observation error t
# First we define the logGamma (0.01,0.01) prior 


## ----child="practicals/LMM_fish_example.qmd"----------------------------------


library(dplyr)
library(INLA)
library(ggplot2)
library(patchwork)
library(inlabru)  
library(here)



# ubica con "here"
list.files(here("workshop_materials","copenhagen_ices",
                "datasets"))

PygmyWFBC <- read.csv(here("workshop_materials","copenhagen_ices",
                           "datasets","PygmyWFBC.csv"))

ggplot(PygmyWFBC, aes(x = factor(net_no), 
                      y = wt,
                      fill = sex)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("F" = "lightblue",
                                     "M" = "lightgreen")) +
  labs(y="Weight (g)",
       x = "Net no.")+
  theme_bw()



## -----------------------------------------------------------------------------
PygmyWFBC$sex_M <- ifelse(PygmyWFBC$sex=="F",0,1)


## -----------------------------------------------------------------------------
cmp =  ~ sex_M + 
  beta_0(1 , prec.linear = 0.01)  + 
  beta_1(tl, model = "linear", prec.linear = 0.01) +  
  net_eff(net_no, model = "iid") 

# First we define the logGamma (0.01,0.01) prior 

prec.tau <- list(prec = list(prior = "loggamma",   # prior name
                             param = c(0.01, 0.01))) # prior values

lik =  bru_obs(formula = wt ~ .,
            family = "gaussian",
            data = PygmyWFBC,
            control.family = list(hyper = prec.tau)
            )

fit = bru(cmp, lik)

summary(fit)

plot(fit, "beta_0")+
  plot(fit, "beta_1") &
  theme_bw()


## -----------------------------------------------------------------------------
plot(fit,"net_eff")+
  theme_bw()



## -----------------------------------------------------------------------------

sampvars <-  predict(fit,PygmyWFBC, ~ {
   tau_e <- Precision_for_the_Gaussian_observations
   tau_u <- Precision_for_net_eff
   list(sigma_u = 1/tau_u,
        sigma_e = 1/tau_e)
   },
   n.samples = 1000
  )

names(sampvars) = c("Error variance","Between-net Variance")

sampvars


ICC <-  predict(fit,PygmyWFBC, ~ {
  tau_e <- Precision_for_the_Gaussian_observations
  tau_u <- Precision_for_net_eff
  sigma_u = 1/tau_u
  sigma_e = 1/tau_e
  list(ICC = sigma_u/ (sigma_u+sigma_e))
},
n.samples = 1000
)


var_e <- fit$marginals.hyperpar$`Precision for the Gaussian observations` %>%
  inla.tmarginal(function(x) 1/x,.) 

var_u <- fit$marginals.hyperpar$`Precision for net_eff` %>%
  inla.tmarginal(function(x) 1/x,.) 

post_var_summaries <- cbind( inla.zmarginal(var_e,silent = T),
                             inla.zmarginal(var_u,silent = T))
colnames(post_var_summaries) <- c("sigma_e","sigma_u")
post_var_summaries
