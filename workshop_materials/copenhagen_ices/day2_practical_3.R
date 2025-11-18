
## ----child="practicals/Ar1_ex.qmd"--------------------------------------------

# Practical 3
# Aim of this practical:
#   
#   Fit temporal models for discrete time points using inlabru
# Forecasting for future observations
# Set penalized complexity (PC) priors
# Fit temporal models to non-Gaussian data
# we are going to learn:
#   
#   How to fit autoregressive and random walk models for time series data
# How to set PC priors in inlabru
# Forecast future observations using the posterior predictive density

## -----------------------------------------------------------------------------

# AR(1) models in inlabru
# 
# In this exercise we will:
#   
#   Simulate a time series with autocorrelated errors.
# 
# Fit an AR(1) process with inlabru
# 
# Visualize model predictions.
# 
# Forecasting for future observations
# 
# Start by loading useful libraries:

library(tidyverse)
library(INLA)
library(ggplot2)
library(patchwork)
library(inlabru)     


## -----------------------------------------------------------------------------
set.seed(123)

phi = 0.8
tau_u = 10
marg.prec = tau_u * (1-phi^2) # ar1 in INLA is parametrized as marginal variance
u_t =  as.vector(arima.sim(list(order = c(1,0,0), ar = phi), 
                           n = 100,
                           sd=sqrt(1/tau_u)))
a = 1
tau_e = 5
epsilon_t = rnorm(100, sd = sqrt(1/tau_e))
y = a + u_t + epsilon_t


ts_dat <- data.frame(y =y , x= 1:100)




ggplot(ts_dat,aes(y=y,x=x))+
  geom_line()+
  geom_smooth(method='loess',span=0.1)+
  theme_bw()


# Fitting an AR(1) model with inlabru
# Model components
# 
# First, we define the model components, notice that 
# the latent field is defined by two components: 
#   the intercept  and the autoregressive random effects :
## -----------------------------------------------------------------------------
# Model components
cmp =  ~ -1 + alpha(1) + ut(x,model = "ar1")



## -----------------------------------------------------------------------------
# Model formula
formula = y ~ alpha + ut
# Observational model
lik =  bru_obs(formula = y ~.,
            family = "gaussian",
            data = ts_dat)



## -----------------------------------------------------------------------------
# fit the model
fit.ar1 = bru(cmp, lik)

# compare against the true values

data.frame(
  true = c(a,tau_e,marg.prec,phi),
  rbind(fit.ar1$summary.fixed[,c(1,3,5)],
        fit.ar1$summary.hyperpar[,c(1,3,5)])
        ) %>% round(2)


## -----------------------------------------------------------------------------
pred_ar1 = predict(fit.ar1, ts_dat, ~ alpha + ut)

p0 <- ggplot(pred_ar1,aes(y=mean,x=x))+
  geom_line()+
    geom_ribbon(aes(x = x, 
                    y = mean, 
                    ymin = q0.025, 
                    ymax = q0.975),
                alpha = 0.5,
                col="tomato",
                fill="tomato") +
  geom_point(aes(y=y,x=x))+
  theme_bw()


# Forecasting future observations
 # Now, we will extend the previous model to forecast future observations.
 # using the posterior predictive distribution.
## -----------------------------------------------------------------------------
ts.forecast <- rbind(ts_dat, 
  data.frame(y = rep(NA, 50), x = 101:150))



## -----------------------------------------------------------------------------
# Model components
 #  alpha() y ut() siguen igual
cmp =  ~ -1 + alpha(1) + ut(x,model = "ar1")

 # -----------------------------------------------------------------------------
 # Model formula
pred_lik =  bru_obs(formula = y ~.,
            family = "gaussian",
            data = ts.forecast)
# fit the model
fit.forecast = bru(cmp, pred_lik)




## -----------------------------------------------------------------------------
pred_forecast = predict(fit.forecast, ts.forecast, ~ alpha + ut)

p1= ggplot(pred_forecast,aes(y=mean,x=x))+
  geom_line()+
    geom_ribbon(aes(x = x, y = mean, 
                    ymin = q0.025, 
                    ymax = q0.975),
                alpha = 0.5,
                col="lightblue",
                fill="lightblue") +
  geom_point(data=ts_dat, 
             aes(y=y,x=x))+
  theme_bw()


# now with RW1

## -----------------------------------------------------------------------------
# Model components
#  alpha() y ut() siguen igual
cmprw =  ~ -1 + alpha(1) + ut(x,model = "rw1")

# -----------------------------------------------------------------------------
# Model formula
pred_lik =  bru_obs(formula = y ~.,
                    family = "gaussian",
                    data = ts.forecast)
# fit the model
fit.forecast = bru(cmprw, pred_lik)


## -----------------------------------------------------------------------------
## model predictions

## -----------------------------------------------------------------------------
pred_forecast = predict(fit.forecast, ts.forecast, ~ alpha + ut)

p2= ggplot(pred_forecast,aes(y=mean,x=x))+
  geom_line()+
  geom_ribbon(aes(x = x, y = mean, 
                  ymin = q0.025, 
                  ymax = q0.975),
              alpha = 0.5,
              col="red",
              fill="red") +
  geom_point(data=ts_dat, 
             aes(y=y,x=x))+
  theme_bw()

# Plot 
egg::ggarrange(p0 + ggtitle("AR1 model"),
                p1 + ggtitle("AR1 Forecast"),
                p2 + ggtitle("RW1 Forecast"),
                ncol=1)

## ----child="practicals/Ar1_Rw1_ex.qmd"----------------------------------------

## -----------------------------------------------------------------------------
library(tidyverse) 
library(INLA) 
library(ggplot2)
library(patchwork) 
library(inlabru)
library(DAAG)


## -----------------------------------------------------------------------------
data("greatLakes")

greatLakes.df = data.frame(as.matrix(greatLakes),
                           year = time(greatLakes)) %>%
  pivot_longer(cols = c("Erie","michHuron","Ontario","StClair"),
               names_to = "Lakes",
               values_to = "height" ) 



greatLakes.df %>%
  ggplot(aes(x = year, y = height, color = Lakes)) +
  geom_point() +
  facet_wrap(~ Lakes, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none")+
  scale_color_brewer(palette = "Set1")




## -----------------------------------------------------------------------------
greatLakes.df$t.idx <- greatLakes.df$year-1917

Erie.df = greatLakes.df %>% filter(Lakes == "Erie")


# Fitting an AR(1) model with inlabru

# Fit an AR(1) model to the Erie lake data using inlabru, 
#then plot the model fitted values showing 95% credible intervals.

# Model components
cmp =  ~ -1 + alpha(1) + ut(t.idx,model = "ar1")
# Model formula
formula = height ~ alpha + ut


# Observational model
lik =  bru_obs(formula = height   ~.,
               family = "gaussian",
               data = Erie.df )

# fit the model
fit.Erie_ar1 = bru(cmp, lik)

## -----------------------------------------------------------------------------
pred_forecast_lake = predict(fit.Erie_ar1, Erie.df, ~ alpha + ut)


p4= ggplot(pred_forecast_lake, aes(y=mean,x=year))+
  geom_line()+
  geom_ribbon(aes(x = year, y = mean, 
                  ymin = q0.025, 
                  ymax = q0.975),
              alpha = 0.5,
              col="lightblue",
              fill="lightblue") +
  theme_bw()+
  geom_point(aes(y=height, x=year))
p4


# Are there any issues with the fitted model, 
# and if so, how do you think we should address them?
#   
# Answer
# It is clear that the model overfits the data, 
# leading to poor predictive performance. 
# Thus, we need to introduce some prior information on the what 
# we expect the variation of the process to be.

## -----------------------------------------------------------------------------
pc_prior <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)),
                 rho = list(prior = "pc.cor0", param = c(0.5, 0.3))) 

prec.tau_e <- list(prec = list(prior = "loggamma",   # prior name
                             param = c(1, 1))) # prior values

# Model components
cmp =  ~ -1 + alpha(1) + ut(t.idx, model = "ar1",  hyper = pc_prior)
# Model formula
formula = height ~ alpha + ut


# Observational model
lik =  bru_obs(formula = height  ~.,
            family = "gaussian",
            data = Erie.df,
            control.family = list(hyper = prec.tau_e))

# fit the model
fit.Erie_ar2 = bru(cmp, lik)


# Model predictions

pred_forecast_lake_2 = predict(fit.Erie_ar2, Erie.df, ~ alpha + ut)


p5 <- ggplot(pred_forecast_lake_2, aes(y=mean,x=year))+
  geom_line()+
  geom_ribbon(aes(x = year, y = mean, 
                  ymin = q0.025, 
                  ymax = q0.975),
              alpha = 0.5,
              col="lightgreen",
              fill="lightgreen") +
  theme_bw()+
  geom_point(aes(y=height, x=year))
p5


# with diferente priors

## -----------------------------------------------------------------------------
pc_prior <- list(theta = list(prior = "pc.prec", param = c(1, 0.1)),
                 rho = list(prior = "pc.cor0", param = c(0.3, 0.1))) 

prec.tau_e <- list(prec = list(prior = "loggamma",   # prior name
                               param = c(2, 2))) # prior values

# Model components
cmp =  ~ -1 + alpha(1) + ut(t.idx, model = "ar1",  hyper = pc_prior)


# Model formula
formula = height ~ alpha + ut


# Observational model
lik =  bru_obs(formula = height  ~.,
               family = "gaussian",
               data = Erie.df,
               control.family = list(hyper = prec.tau_e))

# fit the model
fit.Erie_ar3 = bru(cmp, lik)


# Model predictions

pred_forecast_lake_3 = predict(fit.Erie_ar3, Erie.df, ~ alpha + ut)


p6 <- ggplot(pred_forecast_lake_3, aes(y=mean,x=year))+
  geom_line()+
  geom_ribbon(aes(x = year, y = mean, 
                  ymin = q0.025, 
                  ymax = q0.975),
              alpha = 0.5,
              col="tomato",
              fill="tomato") +
  theme_bw()+
  geom_point(aes(y=height, x=year))

# with diferente priors

## -----------------------------------------------------------------------------
pc_prior <- list(theta = list(prior = "pc.prec", param = c(0, 1)),
                 rho = list(prior = "pc.cor0", param = c(0.1, 3))) 

prec.tau_e <- list(prec = list(prior = "loggamma",   # prior name
                               param = c(2, 2))) # prior values

# Model components
cmp_rw =  ~ -1 + alpha(1) + 
  ut(t.idx ,
     constr=FALSE,
     model = "rw1",
     hyper=pc_prior,
     scale.model = TRUE)
# Model formula
formula = height ~ alpha + ut


# Observational model
lik =  bru_obs(formula = height  ~.,
               family = "gaussian",
               data = Erie.df,
               control.family = list(hyper = prec.tau_e))

# fit the model
fit.Erie_ar4 = bru(cmp, lik)


# Model predictions

pred_forecast_lake_4 = predict(fit.Erie_ar4, Erie.df, ~ alpha + ut)


p7 <- ggplot(pred_forecast_lake_4, aes(y=mean,x=year))+
  geom_line()+
  geom_ribbon(aes(x = year, y = mean, 
                  ymin = q0.025, 
                  ymax = q0.975),
              alpha = 0.5,
              col="grey",
              fill="grey") +
  theme_bw()+
  geom_point(aes(y=height, x=year))


## with RW

# with diferente priors

## -----------------------------------------------------------------------------

pc_prior <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)),
                 rho = list(prior = "pc.cor0", param = c(0.5, 0.3))) 

prec.tau_e <- list(prec = list(prior = "loggamma",   # prior name
                               param = c(1, 10))) # prior values

# Model components
cmp_rw =  ~ -1 + alpha(1) + 
  ut(t.idx ,
     constr=FALSE,
     model = "rw1",
     hyper=pc_prior,
     scale.model = TRUE)
# Model formula
formula = height ~ alpha + ut
# Observational model
lik =  bru_obs(formula = height  ~.,
               family = "gaussian",
               data = Erie.df,
               control.family = list(hyper = prec.tau_e))
# fit the model
fit.Erie_rw1 = bru(cmp_rw, lik)

# Model predictions

pred_forecast_lake_4 = predict(fit.Erie_ar4, Erie.df, ~ alpha + ut)


p7 <- ggplot(pred_forecast_lake_4, aes(y=mean,x=year))+
  geom_line()+
  geom_ribbon(aes(x = year, y = mean, 
                  ymin = q0.025, 
                  ymax = q0.975),
              alpha = 0.5,
              col="grey",
              fill="grey") +
  theme_bw()+
  geom_point(aes(y=height, x=year))

ggarrange(p4 + ggtitle("AR1 without PC priors"),
          p5 + ggtitle("AR1 with PC priors"),
          p6 + ggtitle("AR1 with different PC priors"),
          p7 + ggtitle("AR1 with different PC priors 2"),
          ncol=1)

# how comare models?
# we can use DIC, WAIC, CPO, etc.

# Group-level effects

# Now we will model the height water levels for all four lakes 
# by grouping the random effects. This will allow a within-lakes 
# correlation to be included. In the next example, we allow for 
# correlated effects using an ar1 model for the years and iid random effects on the lakes. 
# First we create a lakes id and set the priors for our model:

## -----------------------------------------------------------------------------




## -----------------------------------------------------------------------------
greatLakes.df$lake_id <- as.numeric(as.factor(greatLakes.df$Lakes))

pc_prior <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)),
                 rho = list(prior = "pc.cor0", param = c(0.5, 0.3))) 

prec.tau_e <- list(prec = list(prior = "loggamma",   # prior name
                             param = c(1, 10))) # prior values



## -----------------------------------------------------------------------------
# Model components
cmp =  ~ -1 + alpha(1) + ut(year,model = "ar1",
                            hyper = pc_prior,
                            group =lake_id,
                            control.group = 
                              list(model = "iid", 
                                   scale.model = TRUE))

## -----------------------------------------------------------------------------
# Model formula
formula = height ~ alpha + ut


# Observational model
lik =  bru_obs(formula = height  ~.,
            family = "gaussian",
            data = greatLakes.df,
            control.family = list(hyper = prec.tau_e))

# fit the model
fit.all_lakes_ar1 = bru(cmp, lik)

# Model predictions
pred_ar1.all = predict(fit.all_lakes_ar1, greatLakes.df, ~ alpha + ut)



## -----------------------------------------------------------------------------
ggplot(pred_ar1.all,
       aes(y=mean,x=year, color=Lakes))+
  geom_line()+
    geom_ribbon(aes(x = year, 
                    y = mean, 
                    ymin = q0.025, 
                    ymax = q0.975),
                alpha = 0.5,
                color="lightgrey",
                fill= "lightgrey") +
  geom_point(aes(y=height,x=year)) + 
  facet_wrap(~Lakes,scales = "free")+
  theme_bw()


# Temporal models for non-Gaussian data


data("Tokyo")


## -----------------------------------------------------------------------------

# define model component
cmp =  ~ -1 + beta0(1) + time_effect(time, model = "rw2", cyclic = TRUE)

# define model predictor
eta = y ~ beta0 + time_effect

# build the observation model
lik = bru_obs(formula = eta,
              family = "binomial",
              Ntrials = n,
              data = Tokyo)

# fit the model
fit = bru(cmp, lik)


## -----------------------------------------------------------------------------

pTokyo = predict(fit, Tokyo, ~ plogis(beta0 + time_effect))

ggplot(data=pTokyo , aes(x= time, y= y) ) +
  geom_point() + 
  ylab("") + xlab("") +
  # Custom the Y scales:
  scale_y_continuous(
    # Features of the first axis
    name = "",
    # Add a second axis and specify its features
    sec.axis = sec_axis( transform=~./2, name="Probability")
  )  + geom_line(aes(y=mean*2,x=time)) +
  geom_ribbon(aes( ymin = q0.025*2, 
                             ymax = q0.975*2), alpha = 0.5)
  



# -----------------------------------------------------------------------------
# Excersice of Lecture 3

cmp= ~ -1 + Intercept(1) + time(time, model ="rw2")
formula = y ~ Intercept + time
lik = bru_obs(formula = formula,
              data = Tokyo,
              Ntrials = n,
              family = "binomial")
fit = bru(cmp, lik)
preds1 = predict(object = fit, newdata = Tokyo, ~ time)
preds2 = predict(object = fit, newdata = Tokyo, ~ Intercept + time)
inv_logit = function(x) ((1 + exp(-x))^(-1))
preds3 = predict(object = fit, newdata = Tokyo, ~ inv_logit(Intercept + time))

inv_logit = function(x) ((1 + exp(-x))^(-1))
preds = predict(object = fit, newdata = Tokyo, 
                ~ data.frame(time_eff = time,
                             lin_pred = Intercept + time,
                             probs = inv_logit(Intercept + time)),
                n.samples = 1000
)
# preds is then a list                
round(preds$probs[1:3,],3)

preds$probs %>% ggplot() + geom_line(aes(time, mean)) +
  geom_ribbon(aes(time, ymin = q0.025, ymax = q0.975), alpha = 0.5)



samples = generate(object = fit, newdata = Tokyo, 
                   ~ data.frame(time_eff = time,
                                lin_pred = Intercept + time,
                                probs = inv_logit(Intercept + time)),
                   n.samples = 20
)
# samples is now a list of length 20 (n.samples) each element of the list looks like:

samples[[1]][1:3,]

data.frame(time = Tokyo$time, sapply(samples, function(x) x[,1])) %>%
  pivot_longer(-time) %>%
  ggplot() + geom_line(aes(time, value, group = name, color = factor(name))) +
  theme(legend.position = "none")


# execsice "mtcars"
df <- mtcars
# transformar mgp a factor
df$gear <- as.factor(df$gear)
m1 = lm(mpg ~ gear, data = df)
m1$coefficients

df$gear_id = as.numeric(df$gear) # inlabru needs values like 1,2,3 for random effects

# -----------------------------------------------------------------------------
#How do we do this in inlabru?
  
# Option 2: fixed effect are just random effects with fixed precision
cmp2 = ~ -1 +  gear_effect(gear_id, model = "iid", fixed = T,initial = -6)
# or: cmp2 = ~ Intercept(1) +  gear_effect(gear_id, model = "iid", fixed = T,initial = -6, constr = T)
lik2 = bru_obs(formula = mpg ~.,
               data = df)
fit2 = bru(cmp2, lik2)
fit2$summary.random$gear_effect$mean



