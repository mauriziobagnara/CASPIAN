## traffic kernel. includes effect of the amount of traffic between nodes
# currently represented as a saturating exponential.

# a: shape parameter
# T: numeric, amount of traffic (e.g. Num. cars/trucks)

f_traff <- function(T,a){ return(1-exp(-a*T))}
