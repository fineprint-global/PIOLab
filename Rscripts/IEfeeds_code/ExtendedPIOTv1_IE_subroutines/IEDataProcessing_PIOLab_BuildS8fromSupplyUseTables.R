################################################################################
#
# This feed sources all SUT files and creates files in the format of S8 that AISHA can understand
# See annex in AISHA manual for more information on that data type.
#
# Author: Hanspeter Wieland (hanspeter.wieland@wu.ac.at)
#

IEDataProcessing_PIOLab_BuildS8fromSupplyUseTables <- function(year,path)
{
  print(paste0("IEDataProcessing_PIOLab_BuildS8fromSupplyUseTables initiated."))
  
  # Wrapper for writing files to folder
  write_file <- function(object,name)
  {
    filename <-  paste0(path_target,"/",gsub("-","",Sys.Date()),
                        "_PIOLab_AllCountriesS8File_",name,year,".csv")
    
    write.table(object,file = filename,row.names = FALSE,col.names = FALSE,sep = ",")
  }
  
  # Path to folder where processed SUTs are stored
  path_source <- paste0(path$IE_Processed,"/SUT/")
  path_target <- paste0(path$IE_Processed,"/S8files")
  
  # Check whether output folder for processed data exists, if yes delete them
  if(dir.exists(path_target)) unlink(path_target,recursive = TRUE) 
  dir.create(path_target)
  
  # Set base products and industries
  n_reg <- nrow(base$region)
  n_pro <- nrow(base$product)
  n_ind <- nrow(base$industry)
  
  ##############################################################################
  # Create empty S8 sheet
  
  # Number of sheet and year
  n_she <- 1
  n_yea <- 1
  # Number of 8-tuples in domestic supply tables
  n_sup <- n_reg * n_pro * n_ind
  # Number of 8-tuples in use tables (domestic + trade) 
  n_use <- n_reg^2 * n_pro * n_ind
  # Number of final demand categories
  fin <- 3
  # Number of 8-tuples in final demand block (domestic + trade)
  n_fd <- n_reg^2 * (n_pro + n_ind) * fin
  # Number of primary inputs
  pi <- 5
  # Number of 8-tuples in primary input block
  n_va <- n_reg * n_ind * pi
  
  S8_sup <- data.frame("index" = 1:n_sup,"x1" = n_yea,"x2" = n_she,"x3" = rep(1:n_reg,each = n_pro*n_ind),
                       "x4" = 1, "x5" = 1:n_ind, "x6" = rep(1:n_reg,each = n_pro*n_ind), 
                       "x7" = 2, "x8" = rep(1:n_pro,each = n_ind),"t" = NA)
  
  S8_use <- data.frame("index" = 1:n_use,"x1" = n_yea,"x2" = n_she,"x3" = rep(1:n_reg,each = n_pro*n_ind),
                       "x4" = 2,"x5" = 1:n_pro, "x6" = rep(1:n_reg,each = n_reg*n_pro*n_ind), 
                       "x7" = 1, "x8" = rep(1:n_ind,each = n_pro),"t" = NA)
  
  S8_fd <- data.frame("index" = 1:n_fd,"x1" = n_yea, "x2" = n_she, "x3" = rep(1:n_reg,each = ((n_pro+n_ind)*fin)),
                      "x4" = c(rep(1,n_ind),rep(2,n_pro)), "x5" = c(1:n_ind,1:n_pro), "x6" = rep(1:n_reg,each = n_reg*(n_pro+n_ind)*fin), 
                      "x7" = 3, "x8" = rep(1:fin,each = (n_pro+n_ind)),"t" = NA)
  
  S8_va <- data.frame("index" = 1:n_va,"x1" = n_yea, "x2" = n_she, "x3" = rep(1:n_reg,each = (n_ind*pi)),
                      "x4" = 3, "x5" = rep(1:pi,n_ind), "x6" = rep(1:n_reg,each = (n_ind*pi)), 
                      "x7" = 1, "x8" = rep(1:n_ind,each = pi),"t" = NA)
  
  
  # Note HP: Try to remove the message that follows here. Doesn't mean anything.
  for(r in 1:n_reg)
  {
    # Load and write domestic supply tables
    SUP <- read.table(file = paste0(path_source,year,"_DomesticSupply_Region",r,".csv"),sep = ",")
    # Supress messages because the warning "No id variables; using all as measure variables" has no consequences
    # Try to implement the data.table package in the future. At the moment this is not possible because not applicable 
    # on the R version on the Uni Sydney Linux server
    SUP <- suppressMessages({melt(SUP)})
    S8_sup$t[S8_sup$x3 == r] <- SUP$value            
    
    # Load and write domestic use tables
    USE <- read.table(file = paste0(path_source,year,"_DomesticUse_Region",r,".csv"),sep = ",")
    USE <- suppressMessages({melt(USE)})
    S8_use$t[S8_use$x3 == r & S8_use$x6 == r] <- USE$value
    
    # Load and write domestic final demand tables
    FD <- read.table(file = paste0(path_source,year,"_BoundaryOutput_Region",r,".csv"),sep = ",")
    FD <- suppressMessages({melt(FD)})
    S8_fd$t[S8_fd$x3 == r & S8_fd$x6 == r] <- FD$value
    
    # Load and write primary inputs
    PI <- read.table(file = paste0(path_source,year,"_BoundaryInput_Region",r,".csv"),sep = ",")
    PI <- suppressMessages({melt(PI)})
    S8_va$t[S8_va$x3 == r & S8_va$x6 == r] <- PI$value
    
    
    # Write trade blocks
    for(s in (1:n_reg)[-r])
    {
      # Trade intermediates (Use tables)
      USE <- read.table(file = paste0(path_source,year,"_IntermediateTrade_",s,"_",r,".csv"),sep = ",")
      USE <- suppressMessages({melt(USE)})
      S8_use$t[S8_use$x3 == s & S8_use$x6 == r] <- USE$value
      
      # Traded final consumption products
      FD <- read.table(file = paste0(path_source,year,"_FinalTrade_",s,"_",r,".csv"),sep = ",")
      FD <- suppressMessages({melt(FD)})
      S8_fd$t[S8_fd$x3 == s & S8_fd$x6 == r] <- FD$value
    }
  }
  
  # Remove index columns                        
  S8_sup <- S8_sup[,2:10]
  S8_use <- S8_use[,2:10]
  S8_fd <- S8_fd[,2:10]
  S8_va <- S8_va[,2:10]
  
  # Arrange IO elements differently to write standard errors more specificly
  zero <- rbind(S8_sup[S8_sup$t == 0,],
                S8_fd[S8_fd$t == 0,],
                S8_va[S8_va$t == 0,])
  # Note that the use table still includes zeros because the use side will be treated with more caution
  # meaning SE are higher
  
  sup <- S8_sup[S8_sup$t != 0,]
  use <- S8_use
  fd <- S8_fd[S8_fd$t != 0,]
  # Separate waste from final demand
  wa <- fd[fd$x8 != 1,]
  fd <- fd[fd$x8 == 1,]
  # Separate primary inputs
  va <- S8_va[S8_va$t != 0,]
  ore <- va[va$x5 == 1,]
  eol <- va[va$x5 == 2,]
  res <- va[va$x5 %in% 3:5,]
  
  # test if number of rows add up
  nrow(sup)+nrow(use)+nrow(fd)+nrow(ore)+nrow(eol)+nrow(zero)+nrow(res)+nrow(wa)
  nrow(S8_fd)+nrow(S8_sup)+nrow(S8_use)+nrow(S8_va)
  # test if sums add up
  sum(sup$t)+sum(use$t)+sum(fd$t)+sum(ore$t)+sum(eol$t)+sum(res$t)+sum(wa$t)
  sum(S8_fd$t)+sum(S8_sup$t)+sum(S8_use$t)+sum(S8_va$t)
  
  # Write files to folder
  write_file(sup,"Supply")
  write_file(use,"Use")
  write_file(fd,"FinalDemand")
  write_file(ore,"Extraction")
  write_file(eol,"EolScrap")
  write_file(res,"OtherInput")
  write_file(wa,"Waste")
  write_file(zero,"Zero")
  
  print(paste0("IEDataProcessing_PIOLab_BuildS8fromSupplyUseTables finished"))
  
}




