################################################################################
#
# This script is generalized decision tree for estimating unknown doemstic use 
# based on known domestic production and trade flows
#
# Input variable:
#
# item = string of the product to be allocated (e.g. flat rolled products)
# users = vector with string of the industries that use item (e.g. flat rolled products in fabrication)
# share = shares how to distribute item between users
# data = bilateral trade flows
# pro = sum of domestic production 
# use = sum of domestic use of domestic production 
AllocateSupply2Use <- function(SUT,item,share,users,data,pro,use,i)
{
  if(i %in% data$From)
  {
    #####################################
    # Fork 1: Export values are available
    
    # Check if domestic production is available
    if(pro > 0)
    {
      ################################################
      # Fork 2: Domestic production value is available
      
      # Filter and sum exports
      export <- sum(filter(data ,From == i))
      
      if(pro > export)
      {
        ################################################
        # Fork 3: Domestic production larger than exports
        # Write difference between domestic production and exports
        value <- pro - export
      }else
      { 
        ################################################
        # Fork 3: Domestic production smaller than exports
        # Try to solve by using import and use values instead  
      
        if(i %in% data$To)
        {
          #################################################
          # Fork 4: Imports values are available
          # Filter and sum imports
          import <- sum(filter(data ,To == i))
        
          if(use > import)
          {
            ################################################
            # Fork 5: Domestic use larger than imports
            # Write difference  
            value <- use - import
          }else
          {
            ################################################
            # Fork 5: Domestic use is smaller than imports
            # In this case: assume 50% of domestic production goes into domestic use
            value <- pro * 0.5
          }
        }else
        {
          #################################################
          # Fork 4: Imports not available
          # Write domestic production into doemstic use
          value <- pro
        }
      }
    }else
    {
      #######################################################
      # Fork 2: When no domstic production value is available
      # Write domestic use of domestic production is zero
      value <- 0
    }
  }else
  {
    #############################################
    # Fork 1: When no export values are available
    # Write domestic production value
    value <- pro 
  }
  print(paste0(i," ",item," ",value))
  # Write value according to end-use shares into use table
  SUT[item,users] <- value*share
  return(SUT)
}

