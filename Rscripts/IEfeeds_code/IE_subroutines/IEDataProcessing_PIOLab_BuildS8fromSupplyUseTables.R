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
    if(dir.exists(path_target))
    {
      filename <-  paste0(path_target,gsub("-","",Sys.Date()),
                          "_PIOLab_AllCountriesS8File_",name,year,".csv")
      
      write.table(object,file = filename,row.names = FALSE,col.names = FALSE,sep = ",")  
    }else
    {
      # For debugging, i.e. when running the script on a local pc and not on the server,
      # The files are stored in the root folder
      filename <-  paste0(path_source,gsub("-","",Sys.Date()),
                          "_PIOLab_AllCountriesS8File_",name,year,".csv")
      
      write.table(object,file = filename,row.names = FALSE,col.names = FALSE,sep = ",")
    }
    
  }
  
  # Path to folder where processed SUTs are stored
  path_source <- paste0(path$IE_Processed,"/SUT/")
  path_target <- paste0(path$mother,"Data/IE/")
  
  # Load SUT templates, which are needed for grouping the 8-tupels and setting correct standard errors
  
  SUT_temp <- list( "Supply" = as.matrix( read.xlsx(path$IE_classification, sheet = 5, rowNames = TRUE) ),
                    "Use" = as.matrix( read.xlsx(path$IE_classification, sheet = 6, rowNames = TRUE) ) )
  
  
  SUT_temp$Use <- SUT_temp$Use[1:num$flow,]  # Remove the primary input block
  
  # Set col and row names to numeric
  colnames(SUT_temp$Supply) <- rownames(SUT_temp$Use) <- 1:num$flow
  rownames(SUT_temp$Supply) <- colnames(SUT_temp$Use) <- 1:num$process
  
  # Transform from wide to long
  SUT_temp$Use <- melt(SUT_temp$Use)
  SUT_temp$Supply <- melt(SUT_temp$Supply)
  
  colnames(SUT_temp$Use) <- colnames(SUT_temp$Supply) <- c("row","col","val")  
  
  # Set key for ease of identification
  SUT_temp$Use["key"] <- paste0(SUT_temp$Use$row,"-",SUT_temp$Use$col)
  SUT_temp$Supply["key"] <- paste0(SUT_temp$Supply$row,"-",SUT_temp$Supply$col)
  
  # fiter only non zero entries
  SUT_temp$Supply <- SUT_temp$Supply[SUT_temp$Supply$val != 0,] 
  SUT_temp$Use <- SUT_temp$Use[SUT_temp$Use$val != 0,] 
  
  
  ##############################################################################
  # Create empty S8 sheet
  
  # Number of sheet and year
  n_she <- 1
  n_yea <- 1
  
  n_sup <- num$region * num$flow * num$process    # Number of 8-tuples in domestic supply tables
  n_use <- num$region^2 * num$flow * num$process  # Number of 8-tuples in use tables (domestic + trade) 
  fin <- num$demand                               # Number of final demand categories
  proflo <- num$process + num$flow                # Total number of unique sectors
  n_fd <- num$region^2 * proflo * fin             # Number of 8-tuples in final demand block (domestic + trade)
  pi <- num$input                                 # Number of primary inputs
  n_va <- num$region * num$process * pi           # Number of 8-tuples in primary input block
  
  S8_sup <- data.frame("index" = 1:n_sup,
                       "x1" = n_yea,
                       "x2" = n_she,
                       "x3" = rep(1:num$region,each = num$flow*num$process),
                       "x4" = 1, 
                       "x5" = 1:num$process, 
                       "x6" = rep(1:num$region,each = num$flow*num$process), 
                       "x7" = 2, 
                       "x8" = rep(1:num$flow,each = num$process),
                       "t" = NA )
  
  S8_use <- data.frame("index" = 1:n_use,
                       "x1" = n_yea,
                       "x2" = n_she,
                       "x3" = rep(1:num$region,each = num$flow*num$process),
                       "x4" = 2,
                       "x5" = 1:num$flow, 
                       "x6" = rep(1:num$region,each = num$region*num$flow*num$process), 
                       "x7" = 1, 
                       "x8" = rep(1:num$process,each = num$flow),
                       "t" = NA )
  
  S8_fd <- data.frame("index" = 1:n_fd,
                      "x1" = n_yea, 
                      "x2" = n_she, 
                      "x3" = rep(1:num$region,each = proflo *fin),
                      "x4" = c( rep(1,num$process),rep(2,num$flow) ) , 
                      "x5" = c(1:num$process,1:num$flow), 
                      "x6" = rep(1:num$region,each = num$region*proflo*fin), 
                      "x7" = 3, 
                      "x8" = rep(1:fin,each = proflo),
                      "t" = NA )
  
  S8_va <- data.frame("index" = 1:n_va,"x1" = n_yea, "x2" = n_she, "x3" = rep(1:num$region,each = (num$process*pi)),
                      "x4" = 3, "x5" = rep(1:pi,num$process), "x6" = rep(1:num$region,each = (num$process*pi)), 
                      "x7" = 1, "x8" = rep(1:num$process,each = pi),"t" = NA)
  
  
  # Note by HP: Try to remove the message that follows here. Doesn't mean anything.
  
  for(r in 1:num$region)
  {
    # Load and write domestic supply tables
    SUP <- read.table(file = paste0(path_source,year,"_Supply_Region",r,".csv"),sep = ",")
    # Supress messages because the warning "No id variables; using all as measure variables" has no consequences
    # Try to implement the data.table package in the future. At the moment this is not possible because not applicable 
    # on the R version on the Uni Sydney Linux server
    SUP <- suppressMessages({melt(SUP)})
    S8_sup$t[S8_sup$x3 == r] <- SUP$value            
    
    # Load and write domestic use tables
    USE <- read.table(file = paste0(path_source,year,"_Use_Region",r,".csv"),sep = ",")
    USE <- suppressMessages({melt(USE)})
    S8_use$t[S8_use$x3 == r & S8_use$x6 == r] <- USE$value
    
    # Load and write domestic final demand tables
    FD <- read.table(file = paste0(path_source,year,"_FinalOutput_Region",r,".csv"),sep = ",")
    FD <- suppressMessages({melt(FD)})
    S8_fd$t[S8_fd$x3 == r & S8_fd$x6 == r] <- FD$value
    
    # Load and write primary inputs
    PI <- read.table(file = paste0(path_source,year,"_PrimaryInput_Region",r,".csv"),sep = ",")
    PI <- suppressMessages({melt(PI)})
    S8_va$t[S8_va$x3 == r & S8_va$x6 == r] <- PI$value
    
    
    # Write trade blocks
    for(s in (1:num$region)[-r])
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
  
  S8 <- bind_rows(S8_sup,S8_use,S8_fd,S8_va)  # All-in-one for consistency analyses
  
  S8 <- S8[S8$t != 0,]  # Remove alle zero elements
  
  
  ### Arrange supply table ###
  
  S8_sup["key"] <- paste0(S8_sup$x5,"-",S8_sup$x8)   # Set key
  
  # Separate into possible and not possible entries + remove key
  sup_value <- S8_sup[S8_sup$key %in% SUT_temp$Supply$key,1:9]
  sup_zero <- S8_sup[!S8_sup$key %in% SUT_temp$Supply$key,1:9]
  
  ### Arrange use table ###
  
  S8_use["key"] <- paste0(S8_use$x5,"-",S8_use$x8)   # Set key
  
  # Separate into possible and not possible entries + remove key
  use_value <- S8_use[S8_use$key %in% SUT_temp$Use$key,1:9]
  use_zero <- S8_use[!S8_use$key %in% SUT_temp$Use$key,1:9]
  
  ### Arrange other tables ###
  
  fd <- S8_fd[S8_fd$t != 0,]
  # Separate waste from final demand
  wa <- fd[fd$x8 %in% base$demand$Code[base$demand$Type == "Waste"],]  
  fd <- fd[!fd$x8 %in% base$demand$Code[base$demand$Type == "Waste"],]
  # Separate primary inputs
  va <- S8_va[S8_va$t != 0,]
  ore <- va[va$x5 == base$input$Code[base$input$Name == "Crude Ore"],]
  eol <- va[va$x5 == base$input$Code[base$input$Name == "End-of-Life Scrap"],]
  res <- va[va$x5 %in% base$input$Code[base$input$Name %in% c("Flux","Coke","Air")],]
  
  zero <- rbind(sup_zero,
                use_zero,
                S8_fd[S8_fd$t == 0,],
                S8_va[S8_va$t == 0,])
  
  # test if number of rows add up
  nrow(sup_value)+nrow(use_value)+nrow(fd)+nrow(ore)+nrow(eol)+nrow(zero)+nrow(res)+nrow(wa)
  nrow(S8_fd)+nrow(S8_sup)+nrow(S8_use)+nrow(S8_va)
  # test if sums add up
  sum(sup_value$t)+sum(use_value$t)+sum(fd$t)+sum(ore$t)+sum(eol$t)+sum(res$t)+sum(wa$t)
  sum(S8_fd$t)+sum(S8_sup$t)+sum(S8_use$t)+sum(S8_va$t)
  
  # Write files to folder
  write_file(sup_value,"Supply")
  write_file(use_value,"Use")
  write_file(fd,"FinalOutput")
  write_file(ore,"Extraction")
  write_file(eol,"EolScrap")
  write_file(res,"OtherInput")
  write_file(wa,"Waste")
  write_file(zero,"Zero")
  write_file(S8,"AllInOne")
  
}




