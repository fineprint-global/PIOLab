
Analysis_Circularity <- function()
{
  
  L <- IOT$L
  y <- IOT$y
  e <- IOT$e
  Z <- IOT$Z
  
  x <- rowSums(L %*% y)  # Estimate new gross production
  
  stress <- list("prim" = c("Pig iron","Sponge iron"),
                 "sec" = c("Steel scrap","Forming scrap"))
  
  result <- data.frame("prim" = NA,
                       "sec" = NA,
                       base$region,
                       stringsAsFactors = FALSE)  
  
  # Read row indics for stressor and aggregate into regions
  for(i in 1:2)
  {
    index <- Code$Z %>% filter(EntityCode == 2, SectorName %in% stress[[i]]) %>% pull(SectorIndex)
    
    result[,i] <- colSums( Agg(x = Z[index,], aggkey = rep(base$region$Abbrev, each = num$pro), dim = 2) )
    
  }
  
  result <- result %>% select(prim,sec,Region) %>% group_by(Region) %>% 
    summarise(prim = sum(prim), sec = sum(sec))
  
  result["circ"] <- round( result$sec / (result$prim + result$sec), digits = 2)
  
  intens <-  e / x   # calculate direct intensities
  
  
}
