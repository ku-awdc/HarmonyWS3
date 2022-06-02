## # To render this as html (without having to use Matt's Makefile):
## rmarkdown::render('Session_Preparation.Rmd', 'html_document')

set.seed(2021-06-18)

packages <- c("tidyverse", "PriorGen", "rjags", "runjags", 
                   "coda", "TeachingDemos")
if(length(find.package(packages)) != length(packages)){
  install.packages(packages)
}

stopifnot(getRversion() >= "4.1.0")
stopifnot(require('tidyverse'))
stopifnot(require('PriorGen'))
stopifnot(require('rjags'))
stopifnot(require('runjags'))
stopifnot(require('coda'))
stopifnot(require('TeachingDemos'))
packageVersion("runjags") >= numeric_version("2.2.1-7")
stopifnot(testjags()$JAGS.available)
stopifnot(numeric_version(testjags()$JAGS.version) >= "4.3.0")
stopifnot(testjags()$rjags.found)
stopifnot(numeric_version(testjags()$rjags.version) >= "4-13")

tosses <- 10
probability <- 0.5
heads <- 5
likelihood_1 <- choose(tosses, heads) * probability^heads *
                (1-probability)^(tosses-heads)
likelihood_1

likelihood_2 <- dbinom(heads, tosses, probability)
likelihood_2

likelihood_fun <- function(prevalence) dbinom(7, 10, prevalence)

likelihood_fun(0.8)
likelihood_fun(0.5)

optimise(likelihood_fun, interval=c(0, 1), maximum=TRUE)

model <- glm(cbind(7,10-7) ~  1, family=binomial)
plogis(coef(model))

parameters <- seq(0, 1, length.out=101)
likelihoods <- numeric(length(parameters))
for(i in 1:length(parameters)){
	likelihoods[i] <- likelihood_fun(parameters[i])
}
plot(parameters, likelihoods, type='l')
abline(h=0.267, lty='dashed', col='red')
abline(v=0.7, lty='dashed', col='red')

prior_fun <- function(prevalence) dbeta(prevalence, 2, 2)

results <- data.frame(parameter=seq(0, 1, length.out=101), 
                      likelihood=NA, prior=NA, posterior=NA)
for(i in 1:nrow(results)){
	results$likelihood[i] <- likelihood_fun(results$parameter[i])
	results$prior[i] <- prior_fun(results$parameter[i])
	results$posterior[i] <- results$likelihood[i] * results$prior[i]
}
par(mfrow=c(3,1))
with(results, plot(parameter, likelihood, type='l'))
with(results, plot(parameter, prior, type='l'))
with(results, plot(parameter, posterior, type='l'))

a <- 2+7
b <- 2+3
mo <- (a-1)/(a+b-2)
mn <- (a)/(a+b)
md <- qbeta(0.5,a,b)
ci <- TeachingDemos::hpd(qbeta, shape1=a, shape2=b)
cat('Mode:', mo, '\n')
cat('Mean:', mn, '\n')
cat('Median:', md, '\n')
cat('95% CI:', ci[1], '-', ci[2], '\n')

with(results, plot(parameter, posterior, type='l'))
abline(v=mo, lty='dashed', col='red')
abline(v=mn, lty='dotted', col='red')
abline(v=md, lty='dotdash', col='red')
abline(v=ci[1], lty='solid', col='blue')
abline(v=ci[2], lty='solid', col='blue')
abline(h=likelihood_fun(ci[2])*prior_fun(ci[2]), lty='dashed', col='blue')

# The coda package has many utilities to work with MCMC objects:
library('coda')

metropolis <- function(burnin = 0, sample = 10000, sigma = 0.05, 
              initial_value = 0.05, plot=TRUE){
  stopifnot(initial_value > 0, initial_value < 1)
  stopifnot(sigma > 0)
  burnin <- as.integer(burnin)
  sample <- as.integer(sample)
  stopifnot(burnin >= 0)
  stopifnot(sample > 0)

  # Redefine these to work on the log scale:
  llikelihood_fun <- function(prevalence)
    dbinom(7, 10, prevalence, log=TRUE)
  lprior_fun <- function(prevalence) 
    dbeta(prevalence, 2, 2, log=TRUE)

  parameters <- numeric(burnin+sample)
  parameters[1] <- initial_value
  current <- initial_value
  post <- llikelihood_fun(current) + lprior_fun(current)
  for(i in 2:(burnin+sample)){
    proposal <- rnorm(1, current, sigma)
    if(proposal > 0 && proposal < 1){
      newpost <- llikelihood_fun(proposal) + lprior_fun(proposal)
      accept <- newpost > post || rbinom(1, 1, exp(newpost-post))
      if(accept){
        current <- proposal
        post <- newpost
      }
    }
    parameters[i] <- current
  }
  
  if(plot && burnin > 0){
    plot(1:burnin, parameters[1:burnin], type='l', col='red', 
         xlim=c(1,burnin+sample), ylim=c(0,1), 
         main='Parameter values (red:burnin, blue:sample)', 
         ylab='prevalence', xlab='Iteration')
    lines((burnin+1):(burnin+sample), parameters[-(1:burnin)], col='blue')
  }else if(plot){
    plot(1:sample, parameters, type='l', col='blue', 
         xlim=c(1,burnin+sample), ylim=c(0,1), 
         main='Parameter values (red:burnin, blue:sample)', 
         ylab='prevalence', xlab='Iteration')
  }

  parameters <- window(coda::as.mcmc(parameters), start=burnin+1)
  varnames(parameters) <- 'prevalence'
  
  return(parameters)
}

set.seed(2021-06-18)

samples <- metropolis(burnin = 0, sample = 1000, initial_value=0.05)

par(mfrow=c(2,1))
with(results, plot(parameter, posterior, type='l', 
                   xlim=c(0,1), main='True posterior', ylab=NA, xlab=NA))
plot(density(samples), xlim=c(0,1), main='Sampled posterior', 
     ylab=NA, xlab=NA)

# Mean of samples:
mean(samples)
# Median of samples:
median(samples)
# 95% CI of samples:
HPDinterval(samples)

cat('True posterior mean:', mn, '\n')
cat('True posterior median:', md, '\n')
cat('True posterior 95% CI:', ci[1], '-', ci[2], '\n')

set.seed(2021-06-18)

samples <- metropolis(burnin = 100, sample = 1000, initial_value=0.05)

set.seed(2021-06-18)

samples <- metropolis(burnin = 100, sample = 1000, initial_value=0.99)

# Effective sample size:
effectiveSize(samples)

set.seed(2021-06-18)

samples <- metropolis(burnin = 1000, sample = 20000, initial_value=0.05)
# Effective sample size:
effectiveSize(samples)

par(mfrow=c(2,1))
with(results, plot(parameter, posterior, type='l', 
      xlim=c(0,1), main='True posterior', ylab=NA, xlab=NA))
plot(density(samples), xlim=c(0,1), main='Sampled posterior', 
      ylab=NA, xlab=NA)
cat("Mean of samples: ", mean(samples), '\n')
cat('True posterior mean:', mn, '\n')

cat("Median of samples: ", median(samples), '\n')
cat('True posterior median:', md, '\n')

sci <- HPDinterval(samples)
cat("95% CI of samples: ", sci[1,1], '-', sci[1,2], '\n')
cat('True posterior 95% CI:', ci[1], '-', ci[2], '\n')

set.seed(2021-06-18)
samples <- metropolis(burnin = 1000, sample = 20000,
                      initial_value=0.99, plot=FALSE)
# Mean of samples:
mean(samples)

set.seed(2021-06-18)
samples <- metropolis(burnin = 1000, sample = 20000,
                      initial_value=0.99, plot=FALSE)
# Mean of samples:
mean(samples)

set.seed(2021-06-19)
samples <- metropolis(burnin = 1000, sample = 20000,
                      initial_value=0.99, plot=FALSE)
# Mean of samples:
mean(samples)

## samples <- metropolis(burnin = 1000, sample = 10000)
## # Effective sample size:
## effectiveSize(samples)
## # Mean of samples:
## mean(samples)
## # Median of samples:
## median(samples)
## # 95% CI of samples:
## HPDinterval(samples)

cat('True posterior mean:', mn, '\n')
cat('True posterior median:', md, '\n')
cat('True posterior 95% CI:', ci[1], '-', ci[2], '\n')

## # Large sigma:
## samples <- metropolis(sigma=10)
## # Autocorrelation:
## autocorr(samples, lags=1)
## # Effective sample size:
## effectiveSize(samples)
## 
## # Moderately large sigma:
## samples <- metropolis(sigma=1)
## # Autocorrelation:
## autocorr(samples, lags=1)
## # Effective sample size:
## effectiveSize(samples)
## 
## # Moderately small sigma:
## samples <- metropolis(sigma=0.1)
## # Autocorrelation:
## autocorr(samples, lags=1)
## # Effective sample size:
## effectiveSize(samples)
## 
## # Small sigma:
## samples <- metropolis(sigma=0.01)
## # Autocorrelation:
## autocorr(samples, lags=1)
## # Effective sample size:
## effectiveSize(samples)

## # Around 0.3 is optimal, but between 0.2-0.4 give >2000
## seq(0.1,1,by=0.1) |> {\(s) plot(s,sapply(s, function(x) effectiveSize(metropolis(sigma=x))),xlab="sigma",ylab="effectiveSize")}(); abline(h=2000)
