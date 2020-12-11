################################################################################
# This script executes several functions for checking the quality of the tables
# and stores them in an output folder

Diagnostics <- function()
{
  
  rocket <- Plot_Rocket()           # Create rocket plot (realized vs reported values)
  
  balance <- Check_Balance()        # Compute industry balances
  
  boundary <- Plot_BoundaryFlows()  # Comparison of total (boundary) inputs and outputs 
  
  
  scatters <- ggarrange(plotlist = list(rocket, balance),
                        ncol = 2,
                        nrow = 1)
  
  l <- vector(mode = "list", length = 3)
  l[[1]] <- scatters
  l[[3]] <- boundary
  
  ggarrange(plotlist = l,
            ncol = 1,
            nrow = 3,
            heights = c(4,0.3,2) )
  
  
  # Write plot to file:
  ggsave(path = path$output,
         filename = paste0(job$year,"_Diagnostics.png"),
         width = 11, height = 9)
  
  dat <- list("s" = list(), "u" = list() ) # Empty lists for storing domestic SUTs
  
  for( i in base$region$Code )
  {
    tables <- Check_DomesticSUT(base$region$Name[i])   # Read domestic tables
    
    tables$Out <- t( tables$Out ) 
    colnames(tables$Out) <- c( "SEM-outputs (liquid/solid residuals)", "SEM-outputs (gaseous residuals)")
    
    dat$s[[i]] <- cbind( tables$Supply, tables$Out )
    
    u <- matrix( NA, nrow = (num$pro+1+num$va), ncol = (num$ind+2) )
    
    u[ 1:num$pro, 1:num$ind ] <- tables$Use 
    
    u[ num$pro+1 , 1:num$ind ] <- tables$IM_intermediate
    
    
    u[ num$pro+1 , num$ind+1 ] <- sum( tables$IM_final )
    
    u[ 1:num$pro, num$ind+1 ] <- rowSums(tables$Y)
    
    u[ 1:num$pro, num$ind+2 ] <- tables$EX_intermediate + tables$EX_final
    
    u[ (num$pro+2):nrow(u), 1:num$ind ] <- tables$In
    
    # u[ num$pro+1 , 7:10 ] <- 0
    # u[4:6, ncol(u) ] <- 0
    
    rownames(u) <- c( rownames(tables$Use), "Imports", paste0( "Boundary-inputs (",rownames(tables$In),")" )  )
    colnames(u) <- c( colnames(tables$Use), "Final use", "Exports" )
    
    dat$u[[i]] <- u
  }
  
  
  export <- function(x,n)   # Wrapper to write tables in to excel workbooks
  {
    write.xlsx(x, 
               paste0(path$output,"/SI/",job$year,"_",n,"_Tables.xlsx"), 
               sheetName = as.list(base$region$Name),
               colNames = TRUE,
               rowNames = TRUE,
               zoom = 50)
  }
  
  export( dat$s, "Supply")
  export( dat$u, "Use" )
  
}

