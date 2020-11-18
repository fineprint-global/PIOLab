  

Build_IOT <- function(SUT,mode)
{
  S <- SUT$S
  U <- SUT$U
  v <- SUT$v
  y <- SUT$y
  w <- SUT$w
  
  # Code <- Load_IOCodes() # Load IO codes
  
  index <- list( "ind" = Code$Z[Code$Z$EntityCode == 1,],
                 "pro" = Code$Z[Code$Z$EntityCode == 2,]
                )
  
  
  
  D <- t( t(S) / colSums(S) )  # Commodity proportions i.e. market share matrix (ixp)
  # D[is.na(D)] <- 0
  
  C <- S/rowSums(S)  # Commodity composition of industry output (ixp)
  # C[is.na(C)] <- 0
  
  x <- rowSums(S)  # Gross industry output
  q <- colSums(S)  # Gross product output
  
  B <- t(t(U)/x)  # Product by industry input coefficients
  
  print( paste( "Number of negative elements in B:",length( B[is.na(B)] ) ) )
  print( paste( "Number of elements with infinite in B:",length( B[B == Inf] ) ) )
  
  if(mode == "pxp")
  {
    A <- B%*%D  # Commodity-by-Commodity technology matrix
    
    Z <- A %*% diag(q)
    
    L <- solve(diag(nrow(A))-A)  # Derive inverse
    
    # Transform primary inputs from a industry to product classification
    v <- v%*%D
    
    # transform wastes from industry to product classification
    w <- w%*%D
    
    # Create extension of the model by merging primary inputs (v) with  wastes discarded back to the environment (in y)
    e <- matrix(c(t(v),t(w)),nrow = ncol(v),ncol = (nrow(w)+nrow(v)))
    
    print( paste( "Original gross production of products:", round( sum(q)/10^6, digits = 0),"Mt" ) )
    print( paste( "Gross production of products as calculated with L:", round( sum(L%*%y)/10^6, digits = 0),"Mt" ) )
    
    # Estimate new gross production of products (without wastes)
    q <- rowSums(L%*%y)
    
  }
  
  if(mode == "ixi")
  {
    A <- D %*% B  # ixi technology matrix (industry technology assumption)
    
    Z <- A %*% diag(x)  # ixi transaction matrix
    
    L <- solve(diag(nrow(A))-A)  # Derive inverse
    
    # Create extension by merging primary inputs (v) with  wastes (in y)
    e <- matrix(c(t(v),t(w)),nrow = ncol(v),ncol = (nrow(w)+nrow(v)))
    
    y <- D %*% y
    
    print( paste( "Original gross production of industries:", round( sum(x)/10^6, digits = 0),"Mt" ) )
    print( paste( "Gross production of industries calculated with L:", round( sum(L%*%y)/10^6, digits = 0),"Mt" ) )
    
    # Estimate new gross production (without wastes)
    x <- rowSums( L%*%y )
  }
  
  print( paste( "Minimum value in L:", min(L) ) )
  
  model <- list("Z" = Z, "L" = L, "y" = y, "x" = x, "q" = q, "e" = e)
  
  remove(B,D,S,U,v,w)
  
  return(model)
}
