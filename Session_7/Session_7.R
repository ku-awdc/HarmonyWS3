params <-
list(presentation = TRUE)

#' ---
#' title: Session 7
#' subtitle: Incorporating imperfect sensitivity and specificity into more complex models
#' date: "2022-06-10"
#' author:
#'   - Matt Denwood
#' theme: metropolis
#' aspectratio: 169
#' colortheme: seahorse
#' header-includes: 
#'   - \input{../rsc/preamble}
#' params:
#'   presentation: TRUE
#' output:
#'   beamer_presentation:
#'       pandoc_args: ["-t", "beamer"]
#'       slide_level: 2
#'   html_document: default
#' ---
#' 
## ----rendering, eval=FALSE, include=FALSE-------------------------------------
## # To render this as PDF (beamer) slides run:
## rmarkdown::render('Session_7.Rmd', 'beamer_presentation', params=list(presentation=TRUE))
## # And for html:
## rmarkdown::render('Session_7.Rmd', 'html_document', params=list(presentation=FALSE))

#' 
## ----setup, include=FALSE-----------------------------------------------------
source("../rsc/setup.R", local = environment())

#' 
#' ## Recap
#' 
#' Models for diagnostic test evaluation require:
#' 
#'   - At least 2 tests
#'   - At least 2 populations, but preferably 3 or more
#'   - Quite a lot of data
#' 
#' . . .
#' 
#' Fitting the models is technically quite straightforward
#' 
#' The real difficulty lies in the interpretation
#' 
#'   - What exactly is the latent class?
#' 
#' 
#' # Incorporating coefficients:  prevalence
#' 
#' ## Modelling variation in infection probability
#' 
#' - Individuals may be at higher/lower risk of being infected due to known characteristics e.g.:
#' 
#'   * Age
#'   * Sex
#'   * History
#'   * Presence of co-infections
#'   * Whatever
#' 
#' . . .
#' 
#' - There are three ways to deal with this:
#' 
#'   1. Ignore it
#'   1. Group "populations" by these characteristics
#'   1. Embed a (preferably simple!) generalised linear model within your LCM
#' 
#' 
#' ## Logistic regression in JAGS
#' 
## ----echo=FALSE, comment=""---------------------------------------------------
lrmod <- "model{

  for(i in 1:N){
    Observation[i] ~ dbern(prob[i])
    logit(prob[i]) <- intercept + beta1[Category[i]] + beta2*Covariate[i]
  }

  intercept ~ dnorm(0, 0.01)
  beta1[1] <- 0
  for(c in 2:NC){
    beta1[c] ~ dnorm(0, 0.01)
  }
  beta2 ~ dnorm(0, 0.01)

  #data# N, Observation, NC, Category, Covariate
  #monitor# intercept, beta1, beta2
  #inits# intercept, beta1, beta2
}
"
cat(lrmod)
cat(lrmod, file="logistic_regression.txt")
cleanup <- c(cleanup, "logistic_regression.txt", "logistic_imperfect.txt", "logistic_2test.txt", "logistic_hw.txt")

#' 
#' - - -
#' 
#' 
## ----echo=FALSE, comment=""---------------------------------------------------
lrmod <- "model{

  for(i in 1:N){
    Observation[i] ~ dbern(obs_prob[i])
    obs_prob[i] <- prob[i]*se + (1-prob[i])*(1-sp)
    logit(prob[i]) <- intercept + beta1[Category[i]] + beta2*Covariate[i]
  }

  se ~ dbeta(148.43, 16.49)T(1-sp, )
  sp ~ dbeta(240.03, 12.63)

  intercept ~ dnorm(0, 0.01)
  beta1[1] <- 0
  for(c in 2:NC){
    beta1[c] ~ dnorm(0, 0.01)
  }
  beta2 ~ dnorm(0, 0.01)

  #data# N, Observation, NC, Category, Covariate
  #monitor# intercept, beta1, beta2, se, sp
  #inits# intercept, beta1, beta2, se, sp
}
"
cat(lrmod)
cat(lrmod, file="logistic_imperfect.txt")

#' 
#' - - -
#' 
## ----echo=FALSE, comment=""---------------------------------------------------
lrmod <- "model{

  for(i in 1:N){
    Observation[i] ~ dbern(obs_prob[i])
    obs_prob[i] <- prob[i]*se + (1-prob[i])*(1-sp)
    logit(prob[i]) <- intercept + beta1[Category[i]] + beta2*Covariate[i]
  }

  #data# se, sp

  intercept ~ dnorm(0, 0.01)
  beta1[1] <- 0
  for(c in 2:NC){
    beta1[c] ~ dnorm(0, 0.01)
  }
  beta2 ~ dnorm(0, 0.01)

  #data# N, Observation, NC, Category, Covariate
  #monitor# intercept, beta1, beta2
  #inits# intercept, beta1, beta2
}
"
cat(lrmod)
cat(lrmod, file="logistic_imperfect.txt")

#' 
#' - - -
#' 
#' 
## ----echo=FALSE, comment=""---------------------------------------------------
lrmod <- "model{

  for(i in 1:N){
    Observation[i] ~ dbern(obs_prob[i])
    obs_prob[i] <- prob[i]*se[Test[i]] + (1-prob[i])*(1-sp[Test[i]])
    logit(prob[i]) <- intercept + beta1[Category[i]] + beta2*Covariate[i]
  }

  #data# se, sp

  intercept ~ dnorm(0, 0.01)
  beta1[1] <- 0
  for(c in 2:NC){
    beta1[c] ~ dnorm(0, 0.01)
  }
  beta2 ~ dnorm(0, 0.01)

  #data# N, Observation, NC, Category, Covariate, Test
  #monitor# intercept, beta1, beta2
  #inits# intercept, beta1, beta2
}
"
cat(lrmod)
cat(lrmod, file="logistic_2test.txt")

#' 
#' - - -
#' 
#' 
## ----echo=FALSE, comment=""---------------------------------------------------
lrmod <- "model{

  for(i in 1:N){
    Observations[i,1:4] ~ dmulti(obs_probs[i,1:4], 1)
    
    obs_probs[i,1] <- (prob[i] * ((1-se[1])*(1-se[2]))) + ((1-prob[i]) * ((sp[1])*(sp[2])))
    obs_probs[i,2] <- (prob[i] * ((se[1])*(1-se[2]))) + ((1-prob[i]) * ((1-sp[1])*(sp[2])))
    obs_probs[i,3] <- (prob[i] * ((1-se[1])*(se[2]))) + ((1-prob[i]) * ((sp[1])*(1-sp[2])))
    obs_probs[i,4] <- (prob[i] * ((se[1])*(se[2]))) + ((1-prob[i]) * ((1-sp[1])*(1-sp[2])))
    
    logit(prob[i]) <- intercept + beta1[Category[i]] + beta2*Covariate[i]
  }

  #snip#

}
"
cat(lrmod)
cat(lrmod, file="logistic_hw.txt")

#' 
#' - - -
#' 
## ----echo=FALSE, comment=""---------------------------------------------------
lrmod <- "model{

  for(i in 1:G){
    Observations[i,1:4] ~ dmulti(obs_probs[i,1:4], Total[i])
    
    obs_probs[i,1] <- (prob[i] * ((1-se[1])*(1-se[2]))) + ((1-prob[i]) * ((sp[1])*(sp[2])))
    obs_probs[i,2] <- (prob[i] * ((se[1])*(1-se[2]))) + ((1-prob[i]) * ((1-sp[1])*(sp[2])))
    obs_probs[i,3] <- (prob[i] * ((1-se[1])*(se[2]))) + ((1-prob[i]) * ((sp[1])*(1-sp[2])))
    obs_probs[i,4] <- (prob[i] * ((se[1])*(se[2]))) + ((1-prob[i]) * ((1-sp[1])*(1-sp[2])))
    
    logit(prob[i]) <- intercept + beta1[Category[i]] + beta2*RoundedCovariate[i]
  }

  #snip#
  
}
"
cat(lrmod)
cat(lrmod, file="logistic_hw.txt")

#' 
#' 
#' ## Embedding a LR within a LCM
#' 
#' - Hierarchical modelling gives us the freedom to include "models within models" as needed for our specific application, but we usually have to write these ourselves!
#' 
#' - Remember that blocking at group level is much more efficient than looping through all individuals as fewer likelihood calculations are required
#' 
#' . . .
#' 
#' - Autocorrelation may be problematic - if so try to use different contrast schemes eg:
#' 
## ----eval=FALSE---------------------------------------------------------------
##   sex_effect ~ dnorm(0, 0.01)
##   beta1[1] <- -sex_effect/2
##   beta1[2] <- sex_effect/2

#' 
#' - - -
#' 
#' - Random effects are kind of like fixed effects:
#' 
## ----eval=FALSE---------------------------------------------------------------
##   #snip#
##     logit(prob[i]) <- intercept + beta1[Category[i]] + beta3[Group[i]]
##   #snip#
## 
##   for(r in 1:NR){
##     beta3[r] ~ dnorm(0, tau)
##   }
##   tau ~ dgamma(0.01, 0.01)
## 
##   #inits# tau
##   #monitor# tau, beta3

#' 
#' 
#' ## Generating code for a LR
#' 
#' You can use template.jags as inspiration:
#' 
## ----echo=FALSE---------------------------------------------------------------
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
data <- data.frame(weight, group)
cleanup <- c(cleanup, "linear_model.txt")

#' 
#' 
## -----------------------------------------------------------------------------
template.jags(weight ~ group, family="gaussian", data=data, file="linear_model.txt")
results <- run.jags("linear_model.txt")

#' 
#' - - -
#' 
## ----echo=FALSE, comment=""---------------------------------------------------
cat(readLines("linear_model.txt"), sep="\n")

#' 
#' - - -
#' 
#' Supported features:
#' 
#'   - Gaussian, binomial, Poisson, negative binomial, ZIB, ZIP, ZINB
#'   - Random intercepts
#'   - Automatic centering of continuous variables
#' 
#' We can also add (currently manually):
#' 
#'   - Random slopes
#'   - Spline terms
#'   - Interval censoring
#' 
#' 
#' ## Grouping populations
#' 
#' - This is much easier than writing a GLM within our LCM...
#' 
#' - We can also use the template_huiwalter function!
#'   
#'   * See Otero-Abad 2017 for a simple example
#' 
#' - If you have a lot of populations you could use a simple random effect:
#' 
## ----eval=FALSE---------------------------------------------------------------
##   # prev[p] ~ dbeta(1, 1)
##   logit(prev[p]) <- intercept + raneff[i]
##   raneff[i] ~ dnorm(0, tau)

#' 
#' . . .
#' 
#' - Be careful that Se/Sp is still consistent across populations!
#' 
#' 
#' ## Do nothing?
#' 
#' What is the goal of your analysis?
#' 
#'   * Estimating risk factors for disease?
#'   * Estimating true prevalence?
#'   * Estimating Se/Sp?
#' 
#' . . .
#' 
#' Inclusion of risk factors for disease is NOT necessary to estimate Se/Sp!
#' 
#' If you are interested in risk factors for disease (rather than the se/sp directly) then I would probably use a simpler model with fixed se/sp (+/- multiple imputation)
#' 
#' 
#' # Covariates for Sensitivity / Specificity
#' 
#' ## What if diagnostic tests are not consistent across populations?
#' 
#' This time we can't just ignore it!
#' 
#' Solutions:
#' 
#' - Remove that population (and clearly state this in the paper..!)
#' 
#' - Allow the relevant parameter to vary between populations
#' 
#' - Use a (very simple) GLM on the relevant parameter(s)
#' 
#' . . .
#' 
#' But now we are no longer technically within the Hui-Walter framework...
#' 
#' ## Varying Se/Sp between populations
#' 
## ---- eval=FALSE--------------------------------------------------------------
## 		Tally[1:4,p] ~ dmulti(prob[1:4,p], N[p])
## 		
## 		prob[1,p] <- prev[p] * ((1-se[1,SeGp[p]])*(1-se[2,SeGp[p]]))  +  (1-prev[p]) * (sp[1])*(sp[2])
## 	
## 		prob[2,p] <- prev[p] * (se[1,SeGp[p]]*(1-se[2,SeGp[p]]))  +  (1-prev[p]) * (1-sp[1])*sp[2]
## 		
## 		prob[3,p] <- prev[p] * ((1-se[1,SeGp[p]])*(se[2,SeGp[p]]))  +  (1-prev[p]) * (sp[1])*(1-sp[2])
## 	
## 		prob[4,p] <- prev[p] * (se[1,SeGp[p]]*se[2,SeGp[p]])  +  (1-prev[p]) * (1-sp[1])*(1-sp[2])
## 	
## 		prev[p] ~ dbeta(1,1)

#' 
#' . . .
#' 
#' See also:  Stærk-Østergaard 2022
#' 
#' 
#' ## Embedded GLM for Se/Sp
#' 
## ---- eval=FALSE--------------------------------------------------------------
## 	for(p in 1:Populations){
## 		Tally[1:4,p] ~ dmulti(prob[1:4,p], N[p])
## 		
## 		# Probability of observing test -/-
## 		prob[1,p] <- prev[p] * ((1-se[1,p])*(1-se[2,p]))  +  (1-prev[p]) * (sp[1])*(sp[2])
## 	
## 		#snip#
## 		
## 		logit(se[1,p]) <- se_intercept[1] + se1_beta[Se1Category[p]]
## 		logit(se[2,p]) <- se_intercept[2] + se2_beta[Se2Category[p]]
## 
## 	}
## 
##   # NB: tweak contrasts for convergence!
##   se_beta1[1] <- -se_eff/2
##   se_beta1[2] <- se_eff/2
##   se_eff ~ dnorm(0, 0.01)

#' 
#' . . .
#' 
#' See also:  Martinez 2008 (although they wrote the samplers themselves!)
#' 
#' 
#' ## Embedded GLM for Infection AND Se/Sp
#' 
## ---- eval=FALSE--------------------------------------------------------------
## 	for(p in 1:Populations){
## 		Tally[1:4,p] ~ dmulti(prob[1:4,p], N[p])
## 		
## 		# Probability of observing test -/-
## 		prob[1,p] <- prob[p] * ((1-se[1,p])*(1-se[2,p]))  +  (1-prob[p]) * (sp[1])*(sp[2])
## 	
## 		#snip#
## 		
## 		logit(se[1,p]) <- se_intercept[1] + se1_beta[Se1Category[p]]
## 		logit(se[2,p]) <- se_intercept[2] + se2_beta[Se2Category[p]]
## 
## 		logit(prob[i]) <- intercept + beta1[Category[i]]
## 
## 	}
## 
##   #snip#
##   se_eff ~ dnorm(0, 0.01)
##   pr_eff ~ dnorm(0, 0.01)

#' 
#' . . .
#' 
#' But what happens if "Category" is confounded with "Se1Category" ??
#' 
#' 
#' ## General points
#' 
#' - Inconsistent se/sp may happen in e.g. laboratory vs field settings, blood vs milk samples, or due to biological effects such as age-aquired immunity
#' 
#' - Theoretically it is possible to incorporate this into the model, but...
#' 
#'   * It is not frequently done so the models are less well developed/understood
#' 
#'   * Great care is needed, and any additions should be based on known biological processes
#'   
#'   * If all populations have their own se/sp then the model collapses!
#' 
#' . . .
#' 
#' - Be VERY careful when prevalence and se/sp have the same covariate
#' 
#'   * It may work if balancing populations by these covariates, and only including them as se/sp covariates?
#' 
#' 
#' # Practical session 7
#' 
#' ## Points to consider {.fragile}
#' 
#' 1. What is the optimal number of populations?
#' 
#' 2. What happens to identifiability when you deviate from the standard Hui-Walter model?
#' 
#' 
#' `r exercise_start()`
#' 
#' ## Exercise 1
#' 
#' We can use simulated data to explore the effect of including different covariates for prevalence.  Let's say that we have the following covariates:  region, herd, breed.
#' 
## -----------------------------------------------------------------------------
set.seed(2022-06-07)

library("tidyverse")

N <- 1500
R <- 3
H <- 30
B <- 2
se <- c(0.8, 0.9)
sp <- c(0.99, 0.98)

tibble(Animal = 1:N, Breed = sample(1:B, N, TRUE), Herd = sample(1:H, N, TRUE)) |>
  left_join(
    tibble(Herd = 1:H, Region = sample(1:R, H, TRUE), herd_type = sample(c("B1","B2","mixed"), H, TRUE), herd_effect = rnorm(H, 0, 0.5)),
    by="Herd"
  ) |>
  left_join(
    tibble(Region = 1:R, region_effect = c(-1, 0, 1)),
    by = "Region"
  ) |>
  mutate(Breed = case_when(
    herd_type == "B1" ~ 1L,
    herd_type == "B2" ~ 2L,
    herd_type == "mixed" ~ Breed
  )) |>
  mutate(breed_effect = c(-0.15, 0.15)[Breed]) |>
  mutate(prob = plogis(-0.5 + region_effect + herd_effect + breed_effect)) |>
  mutate(status = rbinom(N, 1, prob)) |>
  mutate(Test1 = rbinom(N, 1, status*se[1] + (1-status)*(1-sp[1]))) |>
  mutate(Test2 = rbinom(N, 1, status*se[2] + (1-status)*(1-sp[2]))) |>
  select(Region, Herd, Breed, Test1, Test2) |>
  arrange(Region, Herd) ->
  dataset

str(dataset)
dataset |>
  count(Region, Test1, Test2)

#' 
#' Try to analyse these datasets using the following model variants:
#' 
#' 1.  Group by region (ignore herd and breed)
#' 
#' 1.  Group by region and herd (ignore breed)
#' 
#' 1.  Group by region and use a random effect of herd (ignore breed)
#' 
#' 1.  (Optional) as above, but include a fixed effect of breed
#' 
#' How do the models differ in terms of (a) complexity (i.e. the pain-in-the-arse factor) and (b) inference?
#' 
#' 
#' ## Solution 1
#' 
#' TODO
#' 
#' 
#' ## Exercise 2
#' 
#' We can now simulate the same data as above, but introducing a third test that has a sensitivity that is affected by breed.
#' 
## -----------------------------------------------------------------------------
set.seed(2022-06-07)

library("tidyverse")

N <- 1500
R <- 3
H <- 30
B <- 2
se <- matrix(c(0.8, 0.8, 0.9, 0.9, 0.75, 0.95), ncol=3)
sp <- c(0.99, 0.98, 0.97)

tibble(Animal = 1:N, Breed = sample(1:B, N, TRUE), Herd = sample(1:H, N, TRUE)) |>
  left_join(
    tibble(Herd = 1:H, Region = sample(1:R, H, TRUE), herd_type = sample(c("B1","B2","mixed"), H, TRUE), herd_effect = rnorm(H, 0, 0.5)),
    by="Herd"
  ) |>
  left_join(
    tibble(Region = 1:R, region_effect = c(-1, 0, 1)),
    by = "Region"
  ) |>
  mutate(Breed = case_when(
    herd_type == "B1" ~ 1L,
    herd_type == "B2" ~ 2L,
    herd_type == "mixed" ~ Breed
  )) |>
  mutate(breed_effect = c(-0.15, 0.15)[Breed]) |>
  mutate(prob = plogis(-0.5 + region_effect + herd_effect + breed_effect)) |>
  mutate(status = rbinom(N, 1, prob)) |>
  mutate(Test1 = rbinom(N, 1, status*se[Breed,1] + (1-status)*(1-sp[1]))) |>
  mutate(Test2 = rbinom(N, 1, status*se[Breed,2] + (1-status)*(1-sp[2]))) |>
  mutate(Test3 = rbinom(N, 1, status*se[Breed,3] + (1-status)*(1-sp[2]))) |>
  select(Region, Herd, Breed, Test1, Test2, Test3) |>
  arrange(Region, Herd) ->
  dataset

str(dataset)
dataset |>
  count(Region, Test1, Test2, Test3)

#' 
#' Try to fit the following standard Hui-Walter models for Test1 and Test3 results (ignore Test2 for now):
#' 
#' 1. Group by region
#' 
#' 2. Group by region and breed
#' 
#' Which estimates change between models (1) and (2), and why?
#' 
#' Now add a breed effect to the sensitivity, and re-run these two models.  What are the differences between models, and why?
#' 
#' 
#' ## Solution 2
#' 
#' TODO
#' 
#' 
#' ## Optional Exercise A
#' 
#' Simulate some data representing observed test outcomes along with one categorical predictor (with two levels) and one continuous predictor, with a single imperfect test.  Use the following R code:
#' 
## -----------------------------------------------------------------------------
set.seed(2021-07-01)
N <<- 1000

sim_intercept <- -0.5

NC <<- 2
category_probs <- rep(1, NC)/NC
sim_beta1 <- c(0, 0.6)
stopifnot(length(sim_beta1)==NC)

covariate_mean <- 0
covariate_sd <- 0.5
sim_beta2 <- 0.3

lr_data <- tibble(
  Animal = 1:N, 
  Category = sample.int(NC, N, replace=TRUE, prob=category_probs),
  Covariate = rnorm(N, covariate_mean, covariate_sd),
  probability = plogis(sim_intercept + sim_beta1[Category] + sim_beta2*Covariate),
  Status = rbinom(N, 1, probability)
)

#' 
#' We can see the true relationship between predictors and probability of the outcome:
#' 
## -----------------------------------------------------------------------------
ggplot(lr_data) +
  aes(x = Covariate, y = probability, col = factor(Category)) +
  geom_line()

#' 
#' Add a new column `Observation` to this data, representing an imperfect diagnostic test based on the true `Status` but with sensitivity of 50% and specificity of 99%.  Either try to write this R code yourself or borrow it from an earlier session.
#' 
#' Now use the following simple logistic regression model to analyse the data:
#' 
## ----echo=FALSE, comment=''---------------------------------------------------
cat(readLines("logistic_regression.txt"), sep="\n")

#' 
#' You will need to make sure that runjags can find initial values in your working environment, for example:
#' 
## -----------------------------------------------------------------------------
intercept <- list(chain1=-4, chain2=4)
beta1 <- list(chain1=c(NA, -4), chain2=c(NA, 4))
beta2 <- list(chain1=4, chain2=-4)

#' 
#' You will also need to pass the data frame `lr_data` to `run.jags` using the `data` argument.
#' 
#' How do the parameter estimates compare to the true values?  You can see the true values using:
#' 
## -----------------------------------------------------------------------------
sim_intercept
sim_beta1
sim_beta2

#' 
#' 
#' ### Solution A
#' 
#' We can modify the data to include outcome as follows:
#' 
## -----------------------------------------------------------------------------
se <<- 0.5
sp <<- 0.99

lr_data <- lr_data %>%
  mutate(Observation = rbinom(n(), 1, Status*se + (1-Status)*(1-sp)))

#' 
#' Then we need to set the initial values:
#' 
## -----------------------------------------------------------------------------
intercept <- list(chain1=-4, chain2=4)
beta1 <- list(chain1=c(NA, rep(-4, NC-1)), chain2=c(NA, rep(4, NC-1)))
beta2 <- list(chain1=4, chain2=-4)

#' 
#' Then we can run the model:
#' 
## -----------------------------------------------------------------------------
results_lr <- run.jags("logistic_regression.txt", n.chains=2, data=lr_data)
# Remember to check convergence and effective sample size!
# plot(results_lr)
results_lr

#' 
#' The median estimates for beta1 and beta1 are not too far away from the simulation parameter values, and at the very least they are contained within the 95% CI.  However, the intercept parameter is under-estimated compared to the simulation value of -0.5.  This is due to the difference between the apparant prevalence and trur prevalence caused by the poor sensitivity of the test.
#' 
#' The other thing to note is that this model takes a lot longer to run than a Hui-Walter model - this is due to looping over individuals with individual covariates.  If we only had categorical predictors then we would be much better off collapsing the observed combinations of categorical predictors together, so that our outcome was Binomial and not just Bernoulli (in this case we would be looping over 2 categorical predictor levels, and not 1000 observations).  However, there is no way of doing this with continuous covariates unless you are willing to categorise them into a number of discrete bins.
#' 
#' 
#' ## Optional Exercise B
#' 
#' Now analyse the same data using the following imperfect test model:
#' 
## ----echo=FALSE, comment=''---------------------------------------------------
cat(readLines("logistic_imperfect.txt"), sep="\n")

#' 
#' For illustration purposes we are assuming se and sp are fixed to the same values as we used to simulate the data.  In reality you would probably use mean/median estimates from a published source, and possibly include some kind of uncertainty either by putting Beta priors (obtained using `PriorGen::findbeta()`) on these parameters, or by doing a sensitivity analysis by varying the se and sp parameters.
#' 
#' Fit the model to the data.  What has changed relative to the analysis from exercise 1?
#' 
#' 
#' ### Solution B
#' 
#' This is very similar to the solution for exercise A, just with a different model:
#' 
## -----------------------------------------------------------------------------
results_imp <- run.jags("logistic_imperfect.txt", n.chains=2, data=lr_data)
# Remember to check convergence and effective sample size!
# plot(results_imp)
results_imp

#' 
#' This model is slower than the logsitic regression model on the same data because the model is more complicated.  But has it made any difference to the estimates?  There are small differences in the coefficients (beta1 and beta2), but nothing substantial.  The intercept parameter has been affected, as it now reflects the true prevalence rather than the observed prevalence.  But we don't usually care about the intercept anyway.
#' 
#' So in this case we don't really gain anything by using an imperfect test model.  We might as well just say that the imperfect diagnostic test characteristics are one part of the variability that is captured by the Binomial distribution response, and that the intercept reflects the average observed prevalence and not the average true prevalence.  The only exception to this is where one or more of the covariates has an extremely strong affect on the true prevalence, in which case we may under-estimate the magnitude of this effect due to the inter-play between sensitivity, specificity and prevalence.
#' 
#' 
#' ## Optional Exercise C
#' 
#' Now let's make this more complicated.  Simulate two different tests, where the first test is used for animals that have a value of 1 for the categorical predictor, and the second test is used for animals that have a value of 2 for the categorical predictor (these could be animal groups or farms, for example).  The first test has the same sensitivity and specificity as before, but the second test has higher sensitivity but lower specifity (both are 95% for this test).  Write the R code yourself if you want to, otherwise see the hint below (just above the solution).
#' 
#' Now analyse the same data using the original logistic regression model, as well as the following multiple-imperfect-tests model:
#' 
## ----echo=FALSE, comment=''---------------------------------------------------
cat(readLines("logistic_2test.txt"), sep="\n")

#' 
#' Fit the model to the data.  What has changed relative to the analysis from exercise 2?
#' 
#' #### Hint
#' 
#' You could use this R code for simulating data:
#' 
## -----------------------------------------------------------------------------
se <- c(0.5, 0.99)
sp <- c(0.95, 0.95)

lr_2test <- lr_data %>%
  mutate(Obs1 = rbinom(n(), 1, Status*se[1] + (1-Status)*(1-sp[1]))) %>%
  mutate(Obs2 = rbinom(n(), 1, Status*se[2] + (1-Status)*(1-sp[2]))) %>%
  mutate(Test = Category) %>%
  mutate(Observation = case_when(
    Test == 1 ~ Obs1,
    Test == 2 ~ Obs2
  ))

#' 
#' 
#' ### Solution C
#' 
#' The first part of this is the same as the solution for exercise A, just with the new observation:
#' 
## -----------------------------------------------------------------------------
results_lr_2t <- run.jags("logistic_regression.txt", n.chains=2, data=lr_2test)
# Remember to check convergence and effective sample size!
# plot(results_lr_2t)
results_lr_2t

#' 
#' You can see that we get quite different estimates for beta1 compared to before!  This is because the diagnostic test is confounded with the categorical predictor.  This confounding has caused us to over-estimate the effect of the categorical predictor.
#' 
#' However, we can fit the model that allows the test to differ:
#' 
## -----------------------------------------------------------------------------
results_imp_2t <- run.jags("logistic_2test.txt", n.chains=2, data=lr_2test)
# Remember to check convergence and effective sample size!
# plot(results_imp_2t)
results_imp_2t

#' 
#' In this case we control for the confounding between the test type and categorical predictor, and end up recovering more sensible estimates for beta1 (as well as the intercept and beta2).
#' 
#' Note that none of these models allow us to estimate sensitivity or specificity:  there simply is not enough information in the data.  We are therefore forced to fix the values of sensitivity and specificity within the model and assume that these are correct!  The only alternative is to fit a simple fixed effect of the test type in a standard GLM, in which case you can estimate the association between the test type and the observed prevalence.  However, where test type is completely confounded with another predictor variable (as in this case), then we are unable to separate those two effects without incorporating prior knowledge for the diagnostic test performance.
#' 
#' 
#' 
#' `r exercise_end()`
#' 
#' 
#' ## Summary {.fragile}
#' 
#' - Adding populations (or equivalently, covariates on prevalence) adds parameters but may add information
#'   
#'   * But it is not always worthwile!
#' 
#' - Using covariates on sensitivity and specificity is tricky...
#' 
#' - Some further reading:  Martinez et al 2008, Stærk-Østergaard et al 2022
#' 
#' 
## ----include=FALSE------------------------------------------------------------
unlink(cleanup)

