#########################################
#                                       #
#   This code runs the Waste-IO model   #
#       and saves the data in the       #
#         appropriate folders           #
#                                       #
#########################################

IEDataProcessing_PIOLab_WasteMFAIOModelRun <- function(year,path)
{
  
  print("IEDataProcessing_PIOLab_WasteMFAIOModelRun initiated.")
  
  
  # Load codes and indices of model:
  
  IO.codes <- read.csv( paste0(path$IE_Processed,"/EXIOWasteMFAIO/IO_codes.csv"))
  
  IO.codes["Key"] <- paste0(IO.codes$base,"-",IO.codes$sector)
  
  Y.codes <- read.csv( paste0(path$IE_Processed,"/EXIOWasteMFAIO/Y_codes.csv"))
  
  Y.codes["Key"] <- paste0(Y.codes$base,"-",Y.codes$demand)
  
  
  # Load model variables:
  
  Q <- as.matrix( read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Q.csv"),
                           header = FALSE) )
  
  L <- as.matrix( read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_L.csv"),
                           header = FALSE) )
  
  Y <- as.matrix( read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Y.csv"),
                           header = FALSE) )
  
  x <- rowSums( L %*% Y )  # Calculate gross output
  
  
  
  E <- colSums(Q)/x  # Calculate direct intensity
  
  E[is.na(E)] <- 0
  
  MP <- L*E  # Calculate material mulitplier
  
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
  
  colnames(FP) <- Y.codes$Key
  
  rownames(FP) <- IO.codes$Key
  
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
  
  FP <- left_join( x = FP, y = select(Y.codes,base,demand,Key), 
                   by = c("To" = "Key"), 
                   copy = FALSE, 
                   suffix = c(".from",".to") ) 
  
  FinalDemand <- select(FP,-From,-To) # Remove key (not needed anymore)
  
  # 4. Save results to folder 
  
  write.csv(Fabrication2Final,
            file = paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Fabrication2FinalDemand.csv"),
            row.names = FALSE)
  
  write.csv(FinalDemand,
            file = paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_FinalDemand.csv"),
            row.names = FALSE)
  
  
}