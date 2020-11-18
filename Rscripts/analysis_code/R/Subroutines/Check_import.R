################################################################################
# Reads SUT of specific country

Check_import <- function(from,to)
{
  # Set trade block to be extracted:
  
  index <- list( "ind" = filter(Code$Z,RegionName == to,EntityCode == 1) %>% pull(SectorIndex),
                 "pro" = filter(Code$Z,RegionName == from,EntityCode == 2) %>% pull(SectorIndex),
                 "fin" = 1:6 + 6 * ( base$region$Code[base$region$Name == to] - 1 ) )
  
  u <- round(SUT$U[ index$pro, index$ind ],digit = 0)
  
  colnames(u) <- base$industry$Name
  rownames(u) <- base$product$Name
  
  y <- round(SUT$y[ index$pro, index$fin ],digit = 0)
  
  output <- list( "U" = u,
                  "Y" = y)
  
  return(output)
}


