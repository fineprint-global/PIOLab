
# Function to calculate footprint with original model:
Calc_FP <- function(stressor,pop_switch)
{
  L <- IOT$L
  y <- IOT$y
  e <- IOT$e
  x <- rowSums(L %*% y)  # Estimate new gross production
  intens <- e[, base$input$Code[base$input$Name == stressor] ]/x
  
  MP <- L * intens  # Material multipliers
  FP <- MP %*% y  # Material fooptrints
  
  FP <- Agg(FP, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
  FP <- colSums(FP)
  
  if(pop_switch == 1) FP <- FP / pop  # Calculate original per cap footprints
  
  return(FP)  
}
