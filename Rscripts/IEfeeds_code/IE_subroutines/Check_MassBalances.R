# This script calculates balances of the initial estimate (for industries and products),
# and stores it in a xlsx file report
path$IE_Processed

Check_MassBalances <- function()
{
  path_set <- paste0(path$IE_Processed,"/SUT")
  
  for(j in 1:num$region)
  {
    if(j == 1)
    {
      SUP <- read.table(paste0(path_set,"/",year,"_Supply_Region",j,".csv"),
                        row.names = NULL,sep = ',')/1000000
      USE <- read.table(paste0(path_set,"/",year,"_Use_Region",j,".csv"),
                        row.names = NULL,sep = ',')/1000000
      FD <- read.table(paste0(path_set,"/",year,"_FinalOutput_Region",j,".csv"),
                       row.names = NULL,sep = ',')/1000000
      IN <- read.table(paste0(path_set,"/",year,"_PrimaryInput_Region",j,".csv"),
                       row.names = NULL,sep = ',')/1000000
    }else
    {
      SUP <- SUP + read.table(paste0(path_set,"/",year,"_Supply_Region",j,".csv"),
                              row.names = NULL,sep = ',')/1000000
      USE <- USE + read.table(paste0(path_set,"/",year,"_Use_Region",j,".csv"),
                              row.names = NULL,sep = ',')/1000000
      FD <- FD + read.table(paste0(path_set,"/",year,"_FinalOutput_Region",j,".csv"),
                            row.names = NULL,sep = ',')/1000000
      IN <- IN + read.table(paste0(path_set,"/",year,"_PrimaryInput_Region",j,".csv"),
                            row.names = NULL,sep = ',')/1000000
    }
    
    for(i in (1:num$region)[-j])
    {
      if(i == 2 & j == 1)
      {
        USE_im <- read.table(paste0(path_set,"/",year,"_IntermediateTrade_",i,"_",j,".csv"),
                             row.names = NULL,sep = ',')/1000000
        FD_im <- read.table(paste0(path_set,"/",year,"_FinalTrade_",i,"_",j,".csv"),
                            row.names = NULL,sep = ',')/1000000
      }
      USE_im <- USE_im + read.table(paste0(path_set,"/",year,"_IntermediateTrade_",i,"_",j,".csv"),
                                    row.names = NULL,sep = ',')/1000000
      
      FD_im <- FD_im + read.table(paste0(path_set,"/",year,"_FinalTrade_",i,"_",j,".csv"),
                                  row.names = NULL,sep = ',')/1000000
    }
  }
  
  
  Sum_indu <- data.frame("product" = base$process$Name,
                         "SUP[Mt]" = rowSums(SUP)+rowSums(FD[base$process$Code,]),
                         "USE[Mt]" = (colSums(USE)+colSums(IN)+colSums(USE_im)),
                         "USE_dom[Mt]" = (colSums(USE)+colSums(IN)),
                         "USE_im[Mt]" = colSums(USE_im))    
  
  
  Sum_prod <- data.frame("product" = base$flow$Name,
                         "Supply[Mt]" = colSums(SUP),
                         "Use[Mt]" = (rowSums(FD[ (nrow(base$process) + 1) : nrow(FD), ])+ rowSums(USE) + rowSums(USE_im) ) )
  
  
  # function for compiling mass balances of industries and products
  Balances <- function(reg)
  {
    r <- base$region %>% filter(Name == reg) %>% pull(Code)
    
    SUP <- read.table(paste0(path_set,"/",year,"_Supply_Region",r,".csv"),
                      row.names = NULL,sep = ',')/10^6
    
    USE <- read.table(paste0(path_set,"/",year,"_Use_Region",r,".csv"),
                      row.names = NULL,sep = ',')/10^6
    
    FD <- read.table(paste0(path_set,"/",year,"_FinalOutput_Region",r,".csv"),
                     row.names = NULL,sep = ',')/10^6
    
    IN <- read.table(paste0(path_set,"/",year,"_PrimaryInput_Region",r,".csv"),
                     row.names = NULL,sep = ',')/10^6
    
    # Create emtpty data frames for trade flows
    
    USE_im <- USE
    USE_im[1:nrow(USE),1:ncol(USE)] <- 0
    
    FD_im <- FD
    FD_im[1:nrow(FD),1:ncol(FD)] <- 0
    
    USE_ex <- USE_im
    FD_ex <- FD_im
    
    
    for(i in (1:num$region)[-r])
    {
      
      USE_im <- USE_im + read.table(paste0(path_set,"/",year,"_IntermediateTrade_",i,"_",r,".csv"),
                                    row.names = NULL,sep = ',')/10^6
      
      USE_ex <- USE_ex + read.table(paste0(path_set,"/",year,"_IntermediateTrade_",r,"_",i,".csv"),
                                    row.names = NULL,sep = ',')/10^6
      
      FD_im <- FD_im + read.table(paste0(path_set,"/",year,"_FinalTrade_",i,"_",r,".csv"),
                                  row.names = NULL,sep = ',')/10^6
      
      FD_ex <- FD_ex + read.table(paste0(path_set,"/",year,"_FinalTrade_",r,"_",i,".csv"),
                                  row.names = NULL,sep = ',')/10^6
    }
    
    Sum_ind <- data.frame("process" = base$process$Name,
                          "SUP_use" = rowSums(SUP),
                          "SUP_waste" = rowSums(FD[base$process$Code,]),
                          "USE_dom" = (colSums(USE)+colSums(IN)),
                          "USE_im" = colSums(USE_im),
                          "SUP" = rowSums(SUP)+rowSums(FD[base$process$Code,]),
                          "USE" = (colSums(USE)+colSums(IN)+colSums(USE_im)) )    
    
    Sum_ind["Balance_abs"] <- Sum_ind$USE - Sum_ind$SUP
    
    Sum_ind["Balance_rel"] <- 100 * Sum_ind$Balance_abs / Sum_ind$SUP
    
    Sum_ind[is.na(Sum_ind)] <- 0
    
    Sum_ind[,-1] <- round( Sum_ind[,-1], digits = 3 )
    
    FD <- FD + FD_ex
    
    Sum_pro <- data.frame("flow" = base$flow$Name,
                          "FD" = rowSums(FD[ (nrow(base$process) + 1) : nrow(FD), ]),
                          "USE_dom" = rowSums(USE),
                          "USE_ex" = rowSums(USE_ex),
                          "SUP" = colSums(SUP),
                          "USE" = ( rowSums(FD[ (nrow(base$process) + 1) : nrow(FD), ]) + rowSums(USE) + rowSums(USE_ex) ) )
    
    Sum_pro["Balance_abs"] <- Sum_pro$USE - Sum_pro$SUP
    
    Sum_pro["Balance_rel"] <- 100 * Sum_pro$Balance_abs / Sum_pro$SUP
    
    Sum_pro[is.na(Sum_pro)] <- 0
    
    Sum_pro[,-1] <- round( Sum_pro[,-1], digits = 3 )
    
    Sum <- list("ind" = Sum_ind,
                "pro" = Sum_pro)
    
    return(Sum)
    
  }
  
  # objects for region results storage
  industries <- list()
  products <- list()
  
  # Object for hitlist across regions
  Top <- list("ind" = data.frame("index" = 1:(num$process * num$region),
                                 "region" = rep(base$region$Name, each = num$process),
                                 "process" = rep(base$process$Name, num$region),
                                 "Balance_abs" = NA,
                                 "Balance_rel" = NA ),
              "pro" = data.frame("index" = 1:(num$flow * num$region),
                                 "region" = rep(base$region$Name, each = num$flow),
                                 "flow" = rep(base$flow$Name, num$region),
                                 "Balance_abs" = NA,
                                 "Balance_rel" = NA ) )
  
  # Running loop over regions
  for(reg in base$region$Name)
  {
    out <- Balances(reg)  # Calculate balances 
    
    # Store in objects for further export to file
    industries[[reg]] <- out$ind
    products[[reg]] <- out$pro
    
    Top$ind$Balance_abs[Top$ind$region == reg] <- out$ind$Balance_abs
    Top$ind$Balance_rel[Top$ind$region == reg] <- out$ind$Balance_rel
    
    Top$pro$Balance_abs[Top$pro$region == reg] <- out$pro$Balance_abs
    Top$pro$Balance_rel[Top$pro$region == reg] <- out$pro$Balance_rel
  }
  
  # Write region results to two files
  
  write.xlsx( industries,
              paste0( path$IE_Processed,"/IEReport_Balances_Industry.xlsx" ),
              sheetName = base$region$Name )
  
  write.xlsx( products,
              paste0( path$IE_Processed,"/IEReport_Balances_Product.xlsx" ),
              sheetName = base$region$Name )
  
  # Sort the top lists across regions in descending order
  Top$ind <- Top$ind[order( -abs(Top$ind$Balance_abs) ) ,]
  Top$pro <- Top$pro[order( -abs(Top$pro$Balance_abs) ) ,]
  
  out <- list(Top$ind, Top$pro, Sum_indu, Sum_prod)   # Store in one list object for export
  
  # Write to file
  write.xlsx( out,
              paste0( path$IE_Processed,"/IEReport_Balances_Overview.xlsx" ),
              sheetName = c("Violations_industry", "Violations_product", "Global_industry", "Global_product") )
  
}