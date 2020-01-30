################################################################################
#
# IEDataProcessing_PIOLab_StandardErrors
#
# This feed sources all SUT files and calculates standard errors using a simple regression 
# Information is stored in a S8 AISHA format. 
#

IEDataProcessing_PIOLab_BuildS8forTy <- function(year,path)
{
  print(paste0("IEDataProcessing_PIOLab_BuildS8forTy initiated."))
  
  # Path to folder where processed SUTs are stored
  path_set <- paste0(path$IE_Processed,"/SUT/")
  
  # Set base products and industries
  n_reg <- nrow(base$region)
  n_pro <- nrow(base$product)
  n_ind <- nrow(base$industry)
  
  ##############################################################################
  # Create empty S8 sheet
  
  # Number of 8-tuples in domestic supply tables
  n_sup <- n_reg * n_pro * n_ind
  
  # Number of 8-tuples in use tables (domestic + trade) 
  n_use <- n_reg^2 * n_pro * n_ind
  
  # Number of 8-tuples in final demand block (domestic + trade)
  n_fd <- n_reg^2 * (n_pro + n_ind) * 3
  
  S8_sup <- data.frame("index" = 1:n_sup,"x1" = 1,"x2" = 1,"x3" = rep(1:n_reg,each = n_pro*n_ind),
                              "x4" = 1, "x5" = 1:n_ind, "x6" = rep(1:n_reg,each = n_pro*n_ind), 
                              "x7" = 2, "x8" = rep(1:n_pro,each = n_ind),"t" = NA,"SD" = NA)
  
  S8_use <- data.frame("index" = 1:n_use,"x1" = 1,"x2" = 1,"x3" = rep(1:n_reg,each = n_pro*n_ind),
                              "x4" = 2,"x5" = 1:n_pro, "x6" = rep(1:n_reg,each = n_reg*n_pro*n_ind), 
                              "x7" = 1, "x8" = rep(1:n_ind,each = n_pro),"t" = NA,"SD" = NA)
  
  S8_fd <- data.frame("index" = 1:n_fd,"x1" = 1, "x2" = 1, "x3" = rep(1:n_reg,each = ((n_pro+n_ind)*3)),
                          "x4" = c(rep(1,n_ind),rep(2,n_pro)), "x5" = c(1:n_ind,1:n_pro), "x6" = rep(1:n_reg,each = n_reg*(n_pro+n_ind)*3), 
                          "x7" = 3, "x8" = rep(1:3,each = (n_pro+n_ind)),"t" = NA,"SD" = NA)
  
  # Note HP: Try to remove the message that follows here. Doesn't mean anything.
  for(r in 1:n_reg)
  {
    print(r)
    # Load and write domestic supply tables
    SUP <- read.table(file = paste0(path_set,year,"_DomesticSupply_Region",r,".csv"),sep = ",")
    SUP <- melt(SUP)
    S8_sup$t[S8_sup$x3 == r] <- SUP$value            
    
    # Load and write domestic use tables
    USE <- read.table(file = paste0(path_set,year,"_DomesticUse_Region",r,".csv"),sep = ",")
    USE <- melt(USE)
    S8_use$t[S8_use$x3 == r & S8_use$x6 == r] <- USE$value
    
    # Load and write domestic final demand tables
    FD <- read.table(file = paste0(path_set,year,"_BoundaryOutput_Region",r,".csv"),sep = ",")
    FD <- melt(FD)
    S8_fd$t[S8_fd$x3 == r & S8_fd$x6 == r] <- FD$value
    
    # Write trade blocks
    for(s in (1:n_reg)[-r])
    {
      # Trade intermediates (Use tables)
      USE <- read.table(file = paste0(path_set,year,"_IntermediateTrade_",s,"_",r,".csv"),sep = ",")
      USE <- melt(USE)
      S8_use$t[S8_use$x3 == s & S8_use$x6 == r] <- USE$value
      
      # Traded final consumption products
      FD <- read.table(file = paste0(path_set,year,"_FinalTrade_",s,"_",r,".csv"),sep = ",")
      FD <- melt(FD)
      S8_fd$t[S8_fd$x3 == s & S8_fd$x6 == r] <- FD$value
    }
  }
  
  # Combine frames and select columns                        
  S8 <- bind_rows(S8_use,S8_sup,S8_fd)
  S8 <- S8[,2:10]
  
  path_set <- paste0(path$IE_Processed,"/BuildS8forTy")
  
  # Check whether output folder for processed data exists, if yes delete them
  if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
  dir.create(path_set)
  
  filename <-  paste0(path_set,"/",gsub("-","",Sys.Date()),
                      "_PIOLab_AllCountries_000_BuildS8forTy_000_S8FileFor",year,".csv")
  
  write.table(S8,file = filename,row.names = FALSE,col.names = FALSE,sep = ",")
  
  print(paste0("IEDataProcessing_PIOLab_BuildS8forTy finished"))
  
}




