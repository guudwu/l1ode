# Check property of generated ODE system.

# Currently following properties are checked:
# Pair-wise correlation of curves.
# Variance Inflation Factor.
# Condition number of data matrix.

summary.l1ode <- function
(
  data
  , correlation = TRUE
  , vif = TRUE
  , kappa = TRUE
  , sanitycheck = FALSE
)

# INPUT:
# data: A data matrix with no time-points.
#   Each column stands for one curve.
# correlation: Whether to check pair-wise correlation.
# vif: Whether to check VIF.
# kappa: Whether to check condition number.
# sanitycheck: Whether to perform a sanity check on input arguments.

# OUTPUT:
# correlation: Pairwise correlation matrix.
# max_correlation: Maximal pairwise correlation.
# vif: Variance Infaltion Factor.
# kappa: Condition number of data matrix.

{

ret <- list()

# Sanity check#{{{
if ( sanitycheck )
{
# data#{{{
  if (
    !is.matrix(l1ode$truth$curve)
    || !is.numeric(l1ode$truth$curve)
  )
  {
    stop('Argument "data" must be a numerical matrix.')
  }
#}}}

# correlation#{{{
  if (
    !is.logical(correlation)
    || length(correlation)!=1
  )
  {
    stop('Argument "correlation" should be a logical scalar.')
  }
#}}}

# vif#{{{
  if (
    !is.logical(vif)
    || length(vif)!=1
  )
  {
    stop('Argument "vif" should be a logical scalar.')
  }
#}}}

# kappa#{{{
  if (
    !is.logical(kappa)
    || length(kappa)!=1
  )
  {
    stop('Argument "kappa" should be a logical scalar.')
  }
#}}}
}
#}}}

# Correlation#{{{
if ( correlation )
{
  ret$correlation <-
    cor ( data )
  ret$max_correlation <-
    max ( abs (
      ret$correlation
      - diag(nrow(ret$correlation))
    ) )
}
#}}}

# VIF#{{{
if ( vif )
{
  require('HH')
  temp <- lapply ( 1 : ncol(data) , function(index)
  {
    return ( data[,index] )
  } )
  ret$vif <-
    as.vector (
      HH::vif ( data.frame(temp) )
    )
}
#}}}

# Condition number#{{{
if ( kappa )
{
  ret$kappa = kappa ( data )
}
#}}}

return(ret)

}
