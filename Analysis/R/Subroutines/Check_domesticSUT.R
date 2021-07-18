################################################################################
# Reads SUT of specific country

Check_DomesticSUT <- function(reg)
{
  # Set region to be extracted:
  
  index <- list( "ind" = filter( Code$Z, RegionName == reg,EntityCode == 1) %>% pull(SectorIndex),
                 "pro" = filter( Code$Z, RegionName == reg,EntityCode == 2) %>% pull(SectorIndex),
                 "fin" = 1:6 + 6 * ( base$region$Code[base$region$Name == reg] - 1 ) )
  
  s <- round( SUT$S[ index$ind, index$pro ], digit = 0 )
  u <- round( SUT$U[ index$pro, index$ind ], digit = 0 )
  v <- round( SUT$v[ , index$ind ], digit = 0 )
  w <- round( SUT$w[ , index$ind ], digit = 0 )
  y <- round( SUT$y[ index$pro ,  index$fin ], digit = 0 )
  
  im_intermediate <- round(  colSums( SUT$U[ -index$pro, index$ind ] ) , digits = 0 )
  im_final <- colSums( round( SUT$y[ -index$pro ,  index$fin ], digit = 0 ) )
  ex_intermediate <- round(  rowSums( SUT$U[ index$pro, -index$ind ] ) , digits = 0 )
  ex_final <- rowSums( round( SUT$y[ index$pro ,  -index$fin ], digit = 0 ) )
  
  rownames(s) <- colnames(u) <- base$industry$Name
  colnames(s) <- rownames(u) <- base$product$Name
  
  rownames(v) <- base$input$Name
  
  output <- list("Supply" = s,
                 "Use" = u,
                 "Y" = y,
                 "In" = v,
                 "Out" = w,
                 "IM_intermediate" = im_intermediate,
                 "IM_final" = im_final,
                 "EX_intermediate" = ex_intermediate,
                 "EX_final" = ex_final)
  
  return(output)
}


