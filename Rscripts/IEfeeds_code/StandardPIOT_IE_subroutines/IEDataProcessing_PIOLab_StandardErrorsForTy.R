################################################################################
#
# IEDataProcessing_PIOLab_StandardErrors
#
# This feed sources all SUT files and calculates standard errors using a simple regression 
# Information is stored in a S8 AISHA format. 
#

IEDataProcessing_PIOLab_StandardErrorsForTy <- function(year,path)
{
  print(paste0("IEDataProcessing_PIOLab_StandardErrorsForTy initiated."))
  
  # Path to folder where processed SUTs are stored
  path_set <- paste0(path$IE_Processed,"/SUT/")
  
  # Set base products and industries
  base_products <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 7)
  base_industries <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 8)
  n_pro <- nrow(base_products)
  n_ind <- nrow(base_industries)
  
  ##############################################################################
  # Create empty S8 sheet
  
  # Number of 8-tuples in domestic supply tables
  n_sup <- n_reg * n_pro * n_ind
  
  # Number of 8-tuples in use tables (domestic + trade) 
  n_use <- n_reg^2 * n_pro * n_ind
  
  # Number of 8-tuples in final demand block (domestic + trade)
  n_fd <- n_reg^2 * n_pro * 2
  
  S8_sup <- data.frame("index" = 1:n_sup,"x1" = 1,"x2" = 1,"x3" = rep(1:n_reg,each = n_pro*n_ind),
                              "x4" = 1, "x5" = 1:n_ind, "x6" = rep(1:n_reg,each = n_pro*n_ind), 
                              "x7" = 2, "x8" = rep(1:n_pro,each = n_ind),"t" = NA,"SD" = NA)
  
  S8_use <- data.frame("index" = 1:n_use,"x1" = 1,"x2" = 1,"x3" = rep(1:n_reg,each = n_pro*n_ind),
                              "x4" = 2,"x5" = 1:n_pro, "x6" = rep(1:n_reg,each = n_reg*n_pro*n_ind), 
                              "x7" = 1, "x8" = rep(1:n_ind,each = n_pro),"t" = NA,"SD" = NA)
  
  S8_fd <- data.frame("index" = 1:n_fd,"x1" = 1, "x2" = 1, "x3" = rep(1:n_reg,each = n_pro*2),
                          "x4" = 2, "x5" = 1:n_pro, "x6" = rep(1:n_reg,each = n_reg*n_pro*2), 
                          "x7" = 3, "x8" = rep(1:2,each = n_pro),"t" = NA,"SD" = NA)
  
  # Note HP: Try to remove the message that follows here. Doesnn't mean anything.
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
  
  path_set <- paste0(path$IE_Processed,"/StandardErrorsForTy")
  
  # Check whether output folder for processed data exists, if yes delete them
  if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
  dir.create(path_set)
  
  filename <-  paste0(path_set,"/",gsub("-","",Sys.Date()),
                      "_PIOLab_AllCountries_000_StandardErrorsForTy_000_S8FileFor",year,".csv")
  
  write.table(S8,file = filename,row.names = FALSE,col.names = FALSE,sep = ",")
  
  print(paste0("IEDataProcessing_PIOLab_StandardErrorsForTy finished"))
  
}




