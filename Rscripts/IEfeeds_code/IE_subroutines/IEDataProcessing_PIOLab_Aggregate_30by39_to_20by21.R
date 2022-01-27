# This function aggregates the IE for 30 Processes and 30 flows to 20 and 22 respectively.

IEDataProcessing_PIOLab_Aggregate_30by39_to_20by21 <- function(year,path)
{
  
  print("IEDataProcessing_PIOLab_Aggregate_30by39_to_20by21 initiated.")
  
  # Load Number2File:
  source(paste0(path$Subroutines,"/Numbers2File.R"))
  
  # Read aggregators
  Agg_pro <- read.xlsx(str_c(path$Concordance,"/Sector Aggregators/Aggregator_30by39_to_20by21.xlsx"),sheet = 1)
  Agg_flo <- read.xlsx(str_c(path$Concordance,"/Sector Aggregators/Aggregator_30by39_to_20by21.xlsx"),sheet = 2)
  
  # Clean aggregators
  Agg_pro <- as.matrix( Agg_pro[1:30, 2:21])
  Agg_flo <- as.matrix( Agg_flo[1:39, 2:22])
  
  ## Aggregate tables
  
  for(i in base$region$Code)
  {
    ## Supply table
    tmp <- as.matrix( read.csv(str_c(path$IE_Processed,"/SUT/",year,"_Supply_Region",i,".csv"), header = FALSE) )
    tmp_new <- t(Agg_pro) %*% tmp %*% Agg_flo
    Numbers2File(tmp_new, paste0(path$IE_Processed,"/SUT/",year,"_Supply_Region",i,".csv") )
    
    ## Final output
    tmp <- as.matrix( read.csv( str_c(path$IE_Processed,"/SUT/",year,"_FinalOutput_Region",i,".csv"), header = FALSE) )
    
    # divide in industry output and flow output and aggregate
    tmp_pro <- t(Agg_pro) %*% tmp[1:nrow(Agg_pro),]
    tmp_flo <- t(Agg_flo) %*% tmp[(nrow(Agg_pro)+1):nrow(tmp),]
    
    # Re-combine and write to file
    tmp_new <- as.matrix( rbind(tmp_pro, tmp_flo) )
    Numbers2File(tmp_new, str_c(path$IE_Processed,"/SUT/",year,"_FinalOutput_Region",i,".csv") )  
    
    ## Primary input
    tmp <- as.matrix( read.csv(str_c(path$IE_Processed,"/SUT/",year,"_PrimaryInput_Region",i,".csv"), header = FALSE) )
    tmp_new <- tmp %*% Agg_pro
    Numbers2File(tmp_new, str_c(path$IE_Processed,"/SUT/",year,"_PrimaryInput_Region",i,".csv") )
    
    ## Domestic Use table
    tmp <- as.matrix( read.csv(str_c(path$IE_Processed,"/SUT/",year,"_Use_Region",i,".csv"), header = FALSE) )
    tmp_new <- t(Agg_flo) %*% tmp %*% Agg_pro
    
    # Remove own use from rolling processes
    tmp_new[8,8] <- 0
    
    # Write to file
    Numbers2File(tmp_new, paste0(path$IE_Processed,"/SUT/",year,"_Use_Region",i,".csv") )
    
    ## Trade blocks
    for(s in base$region$Code[-i])
    {
      ## Trade intermediates (Use tables)
      tmp <- as.matrix( read.table(file = paste0(path$IE_Processed,"/SUT/",year,"_IntermediateTrade_",s,"_",i,".csv"),sep = ",") )
      tmp_new <- t(Agg_flo) %*% tmp %*% Agg_pro
      
      # Remove own use from rolling processes
      tmp_new[8,8] <- 0
      
      # Write to file
      Numbers2File(tmp_new, paste0(path$IE_Processed,"/SUT/",year,"_IntermediateTrade_",s,"_",i,".csv") )
      
      ## Traded final consumption products
      tmp <- as.matrix( read.table(file = paste0(path$IE_Processed,"/SUT/",year,"_FinalTrade_",s,"_",i,".csv"),sep = ",") )
      
      # divide in industry output and flow output and aggregate
      tmp_pro <- t(Agg_pro) %*% tmp[1:nrow(Agg_pro),]
      tmp_flo <- t(Agg_flo) %*% tmp[(nrow(Agg_pro)+1):nrow(tmp),]
      
      # Re-combine and write to file
      tmp_new <- as.matrix( rbind(tmp_pro, tmp_flo) )
      Numbers2File(tmp_new, paste0(path$IE_Processed,"/SUT/",year,"_FinalTrade_",s,"_",i,".csv") )
    }
  }
}