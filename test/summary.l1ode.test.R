directory <- '~/working/program/l1ode/r-package/R/'

source( paste(directory,'generation.l1ode.R',sep='') )
source( paste(directory,'summary.l1ode.R',sep='') )

# Parameter
set.seed(0)
dimension <- as.integer(10)
time_point <- seq ( 0 , 1 , length.out=101 )

# Generate model
object <-
  generation.l1ode (
    dimension
    , time_point
#    , scaling = TRUE
    , sanitycheck = TRUE
  )

# Summary
property <- summary.l1ode(object$truth$data[,-1])
print(property)
