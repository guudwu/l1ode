directory <- '~/working/program/l1ode/r-package/R/'

source( paste(directory,'generation.l1ode.R',sep='') )

# Parameter
set.seed(0)
dimension <- as.integer(9)
time_point <- seq ( 0 , 1 , length.out=101 ) [-1]

orthogonal_transformation <-
  lapply (
    1 : ((dimension-1)/2) , function(index)
    {
      return ( as.integer ( c ( 2*index , 2*index+1 ) ) )
    } )

# Generate model
object <-
  generation.l1ode (
    dimension
    , time_point
#    , scaling = TRUE
    , orthogonal_transformation = orthogonal_transformation
    , sanitycheck = TRUE
  )

# Check curve with "ode" function from "deSolve" package.
linODE <- function ( time , state , pars )
{
  res <- pars[[1]] %*% state
  if ( ! is.null ( pars[[2]] ) )
    res <- res + pars[[2]]
  return ( list(res) )
}

library('deSolve')
add0 <- time_point[1]!=0
if ( add0 )
{
  time_point <- c ( 0 , time_point )
}
ode_res <- deSolve::ode (
  object$truth$initial
  , time_point
  , linODE
  , list ( object$truth$linear , object$truth$constant )
)
if ( add0 )
{
  ode_res <- ode_res[-1,]
}

difference <- ode_res[,-1] - object$truth$data[,-1]
rss <- norm ( difference , 'F' )
cat('rss: ')
cat(rss)
cat('\n')
#print(difference)

temp <- abs(object$truth$linear[which(object$truth$linear!=0)])
cat('Largest entry: ')
cat(max(abs(temp)))
cat('\n')
cat('Smallest entry: ')
cat(min(abs(temp)))
cat('\n')
