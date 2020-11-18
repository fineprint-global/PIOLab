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
         filename = "Diagnostics.png",
         width = 11, height = 9)
  
  dat <- list("s" = list(), "u" = list() ) # Empty lists for storing domestic SUTs
  
  for( i in base$region$Code )
  {
    tables <- Check_DomesticSUT(base$region$Name[i])   # Read domestic tables
    
    dat$s[[i]] <- tables$Supply
    
    dat$u[[i]] <- tables$Use
  }
  
  
  export <- function(x,n)   # Wrapper to wrte tables in to excel workbooks
  {
    write.xlsx(x, 
               paste0(path$output,"/PSUT/Domestic_",n,"Tables.xlsx"), 
               sheetName = as.list(base$region$Name),
               colNames = TRUE,
               rowNames = TRUE,
               zoom = 50)
  }
  
  export( dat$s, "Supply")
  
  export( dat$u, "Use" )
  
  
  
}

