# Plot a l1ode-object.

plot.l1ode <- function (
  l1ode
  , num_col = as.integer(3)
  , sanitycheck = FALSE
)

# INPUT:
# l1ode: A l1ode-object.
#   It should contain element "truth$data".
# num_col: Number of columns in the figure.
# sanitycheck: Whether to perform a sanity check on input arguments.

{

# Sanity check#{{{

if ( sanitycheck )
{
# l1ode#{{{
  if ( !is.list(l1ode) )
  {
    stop('Argument "l1ode" should be a l1ode-object.')
  }

  if (
    is.null(l1ode$truth$data)
    || !is.matrix(l1ode$truth$data)
    || !is.numeric(l1ode$truth$data)
  )
  {
    stop('Argument "l1ode" should contain element "truth$data", '
      ,'which must be a numerical matrix.')
  }
#}}}

# num_col#{{{
  if (
    !is.integer(num_col)
    || length(num_col)!=1
  )
  {
    stop('Argument "num_col" should be an integer scalar.')
  }
#}}}
}
#}}}

dimension <- (ncol(l1ode$truth$data)-1)
num_row <- ceiling ( dimension / num_col )

par ( mfrow = c ( num_row , num_col ) )

lapply ( 1 : dimension , function(index)
{
  plot (
    l1ode$truth$data[,1]
    , l1ode$truth$data[,index+1]
    , type = 'l'
    , xlab = ''
    , ylab = ''
  )
} )

return()

}
