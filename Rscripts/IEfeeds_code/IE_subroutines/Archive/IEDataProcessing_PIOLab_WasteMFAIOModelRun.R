#########################################
#                                       #
#   This code runs the Waste-IO model   #
#       and saves the data in the       #
#         appropriate folders           #
#                                       #
#########################################

IEDataProcessing_PIOLab_WasteMFAIOModelRunV2 <- function(year,path)
{
  
  print("IEDataProcessing_PIOLab_WasteMFAIOModelRunV2 initiated.")
  
  
  # Load codes and indices of model:
  
  IO.codes <- read.csv( paste0(path$IE_Processed,"/EXIOWasteMFAIO/IO_codes.csv"))
  
  IO.codes["Key"] <- paste0(IO.codes$base,"-",IO.codes$sector)
  
  Y.codes <- read.csv( paste0(path$IE_Processed,"/EXIOWasteMFAIO/Y_codes.csv"))
  
  Y.codes["Key"] <- paste0(Y.codes$base,"-",Y.codes$sector)
  
  
  # Load model variables:
  
  Q <- as.matrix( read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Q.csv"),
                           header = FALSE) )
  
  L <- as.matrix( read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_L.csv"),
                           header = FALSE) )
  
  Y <- as.matrix( read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Y.csv"),
                           header = FALSE) )
  
  x <- rowSums( L %*% Y )  # Calculate gross output
  
  # Calculate multipliers
  
  E <- colSums(Q)/x
  
  E[is.na(E)] <- 0
  
  MP <- L*E
  
  # 1. Estimate flows from fabrication to final product
  
  FP <- MP %*% diag( rowSums(Y) )
  
  colnames(FP) <- rownames(FP) <- IO.codes$Key
  
  FP <- melt(FP)  # Transfrom from wide to long format (pivot-like list) 
  
  colnames(FP) <- c("From","To","Quantity")
  
  FP[,c("From","To")] <- apply(FP[,c("From","To")], c(2), as.character)
  
  
  # Differentiate sector and region codes (from and to):
  
  FP <- left_join( x = FP, y = select(IO.codes,base,sector,Key), 
                   by = c("From" = "Key"), 
                   copy = FALSE, 
                   suffix = c(".from",".to") ) 
  
  FP <- left_join( x = FP, y = select(IO.codes,base,sector,Key), 
                   by = c("To" = "Key"), 
                   copy = FALSE, 
                   suffix = c(".from",".to") ) 
  
  Fabrication2Final <- select(FP,-From,-To) # Remove key (not needed anymore)
  
  
  # 2. Estimate steel in final demand
  
  
  FP <- Y # Create object to store data
  
  # Set col and row names:
  
  colnames(FP) <- paste0(Y.codes$base,"-",Y.codes$demand)
  
  rownames(FP) <- paste0(IO.codes$base,"-",IO.codes$sector)
  
  FP[1:nrow(Y),1:ncol(Y)] <- 0  # Set values to zero
  
  
  # Calculate flows:
  
  for( i in 1:ncol(Y) ) FP[,i] <- rowSums( t(MP)*Y[,i] )
  
  
  FP <- melt(FP)  # Transfrom from wide to long format (pivot-like list) 
  
  colnames(FP) <- c("From","To","Quantity")
  
  FP[,c("From","To")] <- apply(FP[,c("From","To")], c(2), as.character)
  
  
  # Differentiate sector and region codes (from and to):
  
  FP <- left_join( x = FP, y = select(IO.codes,base,sector,Key), 
                   by = c("From" = "Key"), 
                   copy = FALSE, 
                   suffix = c(".from",".to") ) 
  
  FP <- left_join( x = FP, y = select(IO.codes,base,sector,Key), 
                   by = c("To" = "Key"), 
                   copy = FALSE, 
                   suffix = c(".from",".to") ) 
  
  Fabrication2Final <- select(FP,-From,-To) # Remove key (not needed anymore)
  
  
  
  
  colSums(SteelInFinalDemand)/1000000
  
  
  sum(SteelInFinalDemand)
  
  SteelInFinalDemand <- tibble::rownames_to_column(SteelInFinalDemand, "Key")
  SteelInFinalDemand <- melt(SteelInFinalDemand,id.vars = "Key")
  colnames(SteelInFinalDemand) <- c("Key","To.Region","Quantity")
  # Warning messages are turned off for the following join
  options(warn = -1)
  SteelInFinalDemand <- left_join(SteelInFinalDemand,IO.codes,c("Key"),copy = FALSE) %>%
    select(base,To.Region,commodity,Quantity)
  options(warn = 0)
  colnames(SteelInFinalDemand)[c(1,3)] <- c("From.Region","Commodity")
  SteelInFinalDemand <- left_join(SteelInFinalDemand,base$product,c("Commodity" = "Name"),copy = FALSE) %>%
    select(From.Region,To.Region,Code,Quantity)
  colnames(SteelInFinalDemand)[3] <- "Product"
  
  # 4. Save results to folder 
  write.csv(FabricationToFinal,file = paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_FabricationToFinalDemand.csv"),row.names = FALSE)
  write.csv(SteelInFinalDemand,file = paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_SteelInFinalDemand.csv"),row.names = FALSE)
  
  print("DataProcessing_PIOLab_WasteMFAIOModelRun finished.")
}