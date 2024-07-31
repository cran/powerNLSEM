## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("jpirmer/powerNLSEM", build_vignettes = TRUE)

## ----setup--------------------------------------------------------------------
library(powerNLSEM)

## -----------------------------------------------------------------------------
model <- "
# measurement models
X =~ 1*x1 + 0.8*x2 + 0.7*x3
Y =~ 1*y1 + 0.85*y2 + 0.78*y3
Z =~ 1*z1 + 0.9*z2 + 0.6*z3

# structural models
Y ~ 0.3*X + .2*Z +  .2*X:Z

# residual variances
Y~~.7975*Y
X~~1*X
Z~~1*Z

# covariances
X~~0.5*Z

# measurement error variances
x1~~.1*x1
x2~~.2*x2
x3~~.3*x3
z1~~.2*z1
z2~~.3*z2
z3~~.4*z3
y1~~.5*y1
y2~~.4*y2
y3~~.3*y3
"

## ----echo = F-----------------------------------------------------------------
cat("# structural models
Y ~ 0.3*X + .2*Z +  .2*X:Z

# residual variances
Y~~.7975*Y")

## ----warning=FALSE,error=FALSE, eval=TRUE, warning=FALSE----------------------
Result_Power <- powerNLSEM(model = model, 
                           POI = c("Y~X", "Y~Z", "Y~X:Z"), 
                           method = "UPI",
                           search_method = "adaptive", 
                           steps = 2, # for computational reasons, better >= 10
                           power_modeling_method = "probit",
                           R = 200, # for computational reasons, better >= 2000
                           power_aim = .8, 
                           alpha = .05, 
                           alpha_power_modeling = .05,
                           CORES = 1, 
                           seed = 2024)

## -----------------------------------------------------------------------------
names(Result_Power)

## -----------------------------------------------------------------------------
summary(Result_Power)

## -----------------------------------------------------------------------------
Result_Power$N

## -----------------------------------------------------------------------------
dim(Result_Power$est) # dimensions
head(Result_Power$est) # first 6 rows

## -----------------------------------------------------------------------------
head(Result_Power$se) # first 6 rows

## -----------------------------------------------------------------------------
head(Result_Power$fitOK)

## -----------------------------------------------------------------------------
Result_Power$convergenceRate

## -----------------------------------------------------------------------------
Result_Power$N_trials

## -----------------------------------------------------------------------------
Result_Power$power
Result_Power$beta
Result_Power$alpha

## -----------------------------------------------------------------------------
Result_Power$search_method
Result_Power$power_modeling_method
Result_Power$runtime
Result_Power$seed # general seed
head(Result_Power$args$seeds) # seeds within each simulation

## ----fig.width=7, fig.height=5, fig.align='center'----------------------------
plot(Result_Power)

## ----fig.width=7, fig.height=5, fig.align='center'----------------------------
plot(Result_Power, se = TRUE)

## ----fig.width=7, fig.height=5, fig.align='center'----------------------------
plot(Result_Power, se = TRUE, plot = "empirical")

## ----warning=FALSE------------------------------------------------------------
reanalyse.powerNLSEM(Result_Power, 
                     powerLevels = c(.5, .6, .7, .8, .9, .95))

## ----fig.width=7, fig.height=5, fig.align='center'----------------------------
plot(Result_Power, se = TRUE, 
     power_aim = c(.5, .6, .7, .8, .9, .95))

## ----fig.width=7, fig.height=5, fig.align='center', warning=FALSE-------------
reanalyse.powerNLSEM(Result_Power, 
                     powerLevels = c(.5, .6, .7, .8, .9),
     alpha_power_modeling = .001)

plot(Result_Power, se = TRUE, 
     power_aim = c(.5, .6, .7, .8, .9),
     alpha_power_modeling = .001)

## ----fig.width=7, fig.height=5, fig.align='center', warning=FALSE-------------
reanalyse.powerNLSEM(Result_Power, 
                     powerLevels = c(.5, .6, .7, .8, .9),
     alpha_power_modeling = 1)

plot(Result_Power, se = TRUE, 
     power_aim = c(.5, .6, .7, .8, .9),
     alpha_power_modeling = 1)

## -----------------------------------------------------------------------------
populationModel <- "
# measurement models
X =~ 1*x1 + 0.8*x2 + 0.7*x3
Y =~ 1*y1 + 0.85*y2 + 0.78*y3

# structural models
Y ~ 0.3*X 

# residual variances
Y~~.91*Y
X~~1*X

# measurement error variances
x1~~.1*x1
x2~~.2*x2
x3~~.3*x3
y1~~.5*y1
y2~~.4*y2
y3~~.3*y3
"

## ----warning=FALSE------------------------------------------------------------
Simple <- powerNLSEM(model = populationModel, POI = c("Y~X"), method = "UPI",
                     search_method = "adaptive", steps = 2, 
                     power_modeling_method = "probit",
                     R = 200, power_aim = .8, alpha = .05, 
                     seed = 2024, CORES = 1)

## -----------------------------------------------------------------------------
Simple$N

## ----warning=FALSE------------------------------------------------------------
VerifyRes <- powerNLSEM(model = populationModel, POI = c("Y~X"), method = "UPI",
                       search_method = "bruteforce", Ns = Simple$N,
                       R = 200, seed = 2024, CORES = 1)

## -----------------------------------------------------------------------------
summary(VerifyRes)$powersPerN

## ----echo=FALSE---------------------------------------------------------------
P <- summary(VerifyRes)$powersPerN[1,2]
Reps <- VerifyRes$args$R * VerifyRes$convergenceRate
P_LB <- P - qnorm(.975)*sqrt(P*(1-P)/Reps)
P_UB <- P + qnorm(.975)*sqrt(P*(1-P)/Reps)

