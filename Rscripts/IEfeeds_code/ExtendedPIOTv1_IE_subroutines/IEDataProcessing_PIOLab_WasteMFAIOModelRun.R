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
  
  # Load Waste IO Model variables
  load(paste0(path$IE_Processed,"/EXIOWasteMFAIO/IO.codes.RData"))    
  IO.codes <- select(IO.codes,base,commodity,Key)
  load(paste0(path$IE_Processed,"/EXIOWasteMFAIO/Y.codes.RData"))
  
  x <- read.table(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_x.csv"),col.names = FALSE,sep = ",")
  x <- x[,1] 
  L <- read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_L.csv"),header = FALSE)
  colnames(L) <- IO.codes$Key
  rownames(L) <- IO.codes$Key
  Y <- read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Y.csv"),header = FALSE)
  rownames(Y) <- IO.codes$Key 
  colnames(Y) <- Y.codes$base
  load(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Q.RData"))
  
  # 1. Calculate multipliers
  E <- colSums(Q)/x
  E[is.na(E)] <- 0
  MP <- L*E
  
  # 2. Estimate flows from fabrication to final product
  FP <- t(t(MP)*rowSums(Y))
  FP <- melt(FP)
  colnames(FP) <- c("From","To","Quantity")
  
  # Look up base product classification codes
  # Warning messages are turned off for the following join section
  options(warn = -1)
  FP <- left_join(FP,IO.codes,c("From" = "Key"),copy = FALSE)
  FP <- left_join(FP,base$product,c("commodity" = "Name"),copy = FALSE) %>%
    select(base,Code,To,Quantity)
  colnames(FP)[1:2] <- c("From.Region","From.Product")
  FP <- left_join(FP,IO.codes,c("To" = "Key"),copy = FALSE)
  FP <- left_join(FP,base$product,c("commodity" = "Name"),copy = FALSE) %>%
    select(From.Region,From.Product,base,Code,Quantity)
  colnames(FP)[3:4] <- c("To.Region","To.Product")
  options(warn = 0)
  
  FabricationToFinal <- FP
  
  # 3. Estimate steel in final demand
  # Create empty array to store data
  SteelInFinalDemand <- Y
  SteelInFinalDemand[1:nrow(Y),1:ncol(Y)] <- 0
  for(i in 1:ncol(Y)) SteelInFinalDemand[,i] <- rowSums(t(MP)*Y[,i])
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