# Function for aggregating the tables

Agg <- function(x,aggkey,dim)
{
  if(dim == 1) x <- t(x)
  
  colnames(x) <- aggkey
  
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  
  if(dim == 1) x <- t(x)
  
  return(x)
}