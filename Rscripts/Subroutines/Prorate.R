# For more information on the pro-rating see SI of Lenzen 2012 
# (Link: https://doi.org/10.1021/es300171x)

Prorate <- function(C,x_m)
{
  Ones <- rep( 1,length(x_m) )
  
  x_n <- as.vector(C %*% diag(x_m) %*% Ones)
  
  x_n <- diag(1/x_n)
  
  x_n[x_n == Inf] <- 0
  
  M <- x_n %*% C %*% diag(x_m)
  
  return(M)
}