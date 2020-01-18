###########################################
#                                         #
#   This function fills the gap of the    #
#   of the WorldSteels (WSA) production   #
#   numbers using the IEA energy data.    #
#   Moreover, BACI trade data is aligned  #
#   with IRP ore extraction data          #
#                                         #  
###########################################

DataProcessing_AligningData <- function(year,path)
{
  print("DataProcessing_AligningData initiated.")
  ##############################################################################
  # 1. Loading data
  # Loading processed WSA production numbers
  Pig <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_PigIron.csv"))
  Flat <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_FlatRolledProducts.csv"))
  colnames(Flat)[2] <- "Flat"
  Long <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_LongRolledProducts.csv"))
  colnames(Long)[2] <- "Long"
  BOF_steel <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_SteelOxygenBlownConverters.csv"))
  EAF_steel <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_SteelElectricFurnaces.csv"))
  colnames(EAF_steel)[2] <- "EAF"
  
  # Loading processed IEA energy data
  BF_gas <- read.csv(paste0(path$Processed,"/IEA/IEA_",year,"_BlastFurnaceGas.csv"))
  colnames(BF_gas)[2] <- "Energy"
  BOF_gas <- read.csv(paste0(path$Processed,"/IEA/IEA_",year,"_BlastOxygenFurnaceGas.csv"))
  colnames(BOF_gas)[2] <- "Energy"
  Energy_con <- read.csv(paste0(path$Processed,"/IEA/IEA_",year,"_SteelIndustryEnergyConsumption.csv"))
  
  # Loading BACI trade data on iron ores and IRP extraction accounts
  IronOre_Trade <- read.csv(file = paste0(path$Processed,"/BACI/BACI_",year,"_IronOre.csv"))
  IronOre_Extraction <- read.csv(file = paste0(path$Processed,"/IRP/IRP_",year,".csv"))
  
  ##############################################################################
  # 2. Filling gaps in WSA production accounts by checking IEA energy data
  # 2.1 WSA pig iron production vs. production of (IEA) blast furnace gas
  
  # Following the WSA publication "Energy use in the steel industry" (p98) between 
  # 4.314 and 7.17 GJ of blast furnace gas is produced per ton of pig iron.
  # In the following we fill the gaps assuming an average intensity of 5.7 GJ/t
  
  # Merge the two data sets and filter for missing production values
  Pig_vs_BF <- full_join(Pig,BF_gas,c("base"),copy = FALSE) %>% 
    mutate(Intensity = Energy/Quantity)
  Pig_vs_BF_gap <-  Pig_vs_BF %>% filter(Quantity == 0 & Energy != 0)
  
  # Fill in values and save new pig iron production data
  if(length(Pig_vs_BF_gap) > 0) {
    Pig_vs_BF$Quantity[Pig_vs_BF$base %in% Pig_vs_BF_gap$base] <- Pig_vs_BF_gap$Energy / 5.7 
    Pig <- Pig_vs_BF %>% select(base,Quantity) 
    write.csv(Pig, file = paste0(path$Processed,"/WSA/WSA_",year,"_PigIron.csv"),row.names = FALSE)}
  
  remove(Pig_vs_BF,Pig_vs_BF_gap,Pig,BF_gas)
  
  # 2.2 BOF steel production vs. BOF gas production
  
  # Again according to the WSA report on energy, the blown oxygen furnace steelmaking
  # process produces gases of approx. 0.234 GJ per crude steel (p101). Please note that the 
  # WSA case studies (p104) showed values betwene 0.4 and 0.8 GJ per ton
  
  Steel_vs_BOF <- full_join(BOF_steel,BOF_gas,c("base"),copy = FALSE) %>% 
    mutate(Intensity = Energy/Quantity)
  Steel_vs_BOF_gap <- Steel_vs_BOF %>% filter(Quantity == 0 & Energy != 0)
  
  # Fill in values and save new BOF steel production data
  if(nrow(Steel_vs_BOF_gap) > 0) {
    Steel_vs_BOF$Quantity[Steel_vs_BOF$base %in% Steel_vs_BOF_gap$base] <- Steel_vs_BOF_gap$Energy / 0.234 
    BOF_steel <- Steel_vs_BOF %>% select(base,Quantity) 
    write.csv(BOF_steel, file = paste0(path$Processed,"/WSA/WSA_",year,"_SteelOxygenBlownConverters.csv"),row.names = FALSE)}
  
  remove(Steel_vs_BOF,Steel_vs_BOF_gap,BOF_gas)
  colnames(BOF_steel)[2] <- "BOF"
  
  # 2.3 Filling gaps for rolled steel products
  Steel_vs_energy <- full_join(Energy_con,Flat,c("base"),copy = FALSE)
  Steel_vs_energy <- full_join(Steel_vs_energy,Long,c("base"),copy = FALSE) 
  Steel_vs_energy <- full_join(Steel_vs_energy,BOF_steel,c("base"),copy = FALSE)
  Steel_vs_energy <- full_join(Steel_vs_energy,EAF_steel,c("base"),copy = FALSE)
  Steel_vs_energy[is.na(Steel_vs_energy)] <- 0
  Steel_vs_energy <- mutate(Steel_vs_energy,Rolled = Flat+Long,Intensity = Value/Rolled)
  Steel_vs_energy$Intensity[Steel_vs_energy$Intensity == Inf] <- 0
  
  # The reference plant, of the WSA report, has an average energy intensity for
  # long rolled products of approx. 20 GJ per ton and 9.5 GJ per ton for scrap based long products 
  # WSA referenced flat rolling plate mill has an energy intensity of 24 GJ per ton of product (p120-122)
  # Please note that these numbers refer to the total energy requirement including 
  # upstream energy use for steel and iron making. Direct energy use in rolling mills is much smaller at around 2-7 GJ/t
  
  # 2.3.1 Filter for countries with inconsistent data
  # No flat rolling reported but BOF steel is produced 
  Gap <- Steel_vs_energy %>% filter(Flat == 0 & BOF != 0)
  if(nrow(Gap) > 0) Steel_vs_energy$Flat[Steel_vs_energy$base %in% Gap$base] <- Gap$BOF
  
  # No long rolling reported but EAF steel is porduced
  Gap <- Steel_vs_energy %>% filter(Long == 0 & EAF != 0)
  if(nrow(Gap) > 0) Steel_vs_energy$Long[Steel_vs_energy$base %in% Gap$base] <- Gap$EAF
  
  # BOF steel reported but flat rolled products + energy intensity is below the threshold
  Gap <- Steel_vs_energy %>% filter(BOF == 0 & Flat != 0 & Intensity < 20)
  if(nrow(Gap) > 0) Steel_vs_energy$BOF[Steel_vs_energy$base %in% Gap$base] <- Gap$Flat
  
  # No flat and long rolling reported but energy use data available
  Gap <- Steel_vs_energy %>% filter(Flat == 0 & Long == 0 & Value != 0)
  if(nrow(Gap) > 0) {
    # Assuming 18 GJ/ton
    filling <- Gap$Value/18
    Steel_vs_energy[Steel_vs_energy$base %in% Gap$base,c("Flat","Long","BOF","EAF")] <- filling/2 }
  
  ##############################################################################
  # 3. Delete BACI trade flows of iron ores where IRP reports no extraction
  colnames(IronOre_Extraction)[2] <- "Extraction"
  IronOre_Trade_agg <- IronOre_Trade %>% select(From,quantity) %>% group_by(From) %>% 
    summarise(quantity = sum(quantity)) %>% ungroup(From)
  colnames(IronOre_Trade_agg) <- c("base","Trade")
  IronOre_Check <- full_join(IronOre_Extraction,IronOre_Trade_agg,c("base"),copy = FALSE) %>%
    mutate(Share = Trade/Extraction)
  # Read base classificatipon region code
  delete <- IronOre_Check %>% filter(is.na(Extraction)) %>% select(base)
  # Remove trade data for regions with no extraction
  IronOre_Trade <- IronOre_Trade[!IronOre_Trade$From %in% delete$base,]
  
  ##############################################################################
  # 4. Writing manipulated data to folder
  Flat <- select(Steel_vs_energy,base,Flat) 
  Long <- select(Steel_vs_energy,base,Long)
  BOF <- select(Steel_vs_energy,base,BOF)
  EAF <- select(Steel_vs_energy,base,EAF)
  colnames(Flat)[2] <- colnames(Long)[2] <- colnames(BOF)[2] <- colnames(EAF)[2] <- "Quantity"
  
  write.csv(Flat,file = paste0(path$Processed,"/WSA/WSA_",year,"_FlatRolledProducts.csv"),row.names = FALSE)
  write.csv(Long,file = paste0(path$Processed,"/WSA/WSA_",year,"_LongRolledProducts.csv"),row.names = FALSE)
  write.csv(BOF,file = paste0(path$Processed,"/WSA/WSA_",year,"_SteelOxygenBlownConverters.csv"),row.names = FALSE)
  write.csv(EAF,file = paste0(path$Processed,"/WSA/WSA_",year,"_SteelElectricFurnaces.csv"),row.names = FALSE)
  write.csv(IronOre_Trade,file = paste0(path$Processed,"/BACI/BACI_",year,"_IronOre.csv"),row.names = FALSE)
}