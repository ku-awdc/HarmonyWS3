## JAGS modules

# There are basically two types of things to add to JAGS:  distributions and functions
# See the code under basicmodule/src and basicmodule/R for an example of adding a simple "dlom" distribution and "adder" function

# Then install the package:
install.packages("Session_8/JAGS_modules/basicmodule/", repos=NULL, type="source")

# Then you can use it as follows:
library("basicmodule")
library("runjags")

mm <- "
model{

  for(i in 1:N){
    # Our customised distribution:
    dd[i] ~ dlom(alpha, sigma)
  }
  alpha ~ dgamma(0.1, 0.1)
  sigma ~ dgamma(0.1, 0.1)

  # Our customised function:
  ss <- adder(1, 2)

  #monitor# ss, alpha, sigma
  #data# N, dd

}
"

N <- 20
dd <- rgamma(N, 1, 1)

results <- run.jags(mm)

results

# Other types of node (e.g. vector/array arguments) can be defined using one of the other classes given in the headers folder
# Note that it is also possible to interface the code with R using Rcpp
