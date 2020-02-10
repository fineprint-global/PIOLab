#####################################
#                                   #
#   This code writes the data       #
#   into the domestic SUTs that     # 
#   can be used as an input for     # 
#               AISHA               #
#                                   #  
#####################################


IEDataProcessing_PIOLab_BuildingDomesticTables <- function(year,path)
{
  print("IEDataProcessing_PIOLab_BuildingDomesticTables initiated.")
  # Load region aggregator
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(IEdatafeed_name)
  # Load function to compile end use map
  source(paste0(path$Subroutines,"/makeEndUseMap.R"))
  # Load slag rate of blast furnace and coefficient per ton of pig iron output
  WSA_yield <- read.csv(paste0(path$IE_Processed,"/WSA/SteelIndustryYields.csv")) %>%
    select(Process,Average)
  # Load extension of the WasteMFAIO model and IO codes
  load(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Q.RData"))
  load(paste0(path$IE_Processed,"/EXIOWasteMFAIO/IO.codes.RData"))
  # Load trade data
  BACI <- read.csv(paste0(path$IE_Processed,"/BACI/BACI_",year,".csv"))
  # Load allocation function
  source(paste0(path$Subroutines,"/AllocateSupply2Use.R"))
  
  # Define general variables
  n_pro <- nrow(base$product)
  n_ind <- nrow(base$industry)
  n_reg <- nrow(base$region)
  n_va <- 2
  n_fd <- 2
  
  for(i in 1:n_reg)
  {
    print(i)
    # Create empty SUT
    SUT <- data.frame(matrix(0,(n_pro+n_ind+n_va),(n_pro+n_ind+n_fd)))
    colnames(SUT) <- c(base$industry$Name,base$product$Name,
                       "FinalConsumption","Environment")
    rownames(SUT) <- c(base$industry$Name,base$product$Name,
                       "InputsFromNature","EOLScrap")
    SUT <- as.matrix(SUT)
    ############################################################################
    # 1. Writing boundary inputs of iron ore extraction and end-of-life scrap to table
    
    # 1.1 EoL scrap 
    data <- read.csv(paste0(path$IE_Processed,"/EOL/EOL_",year,".csv"))
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["EOLScrap","Scrap preparation"] <- value }
    
    # 1.2 IRP iron ore extraction i.e. inputs from nature
    data <- read.csv(paste0(path$IE_Processed,"/IRP/IRP_",year,".csv"))
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["InputsFromNature","Mining"] <- value }
    
    ############################################################################
    # 2. Writing production values into supply table
    
    # 2.1 Waste (gangue) output of mining
    # Read iron ore grades and assume beneficiation to 60% of ore grade
    data <- read.csv(paste0(path$IE_Processed,"/Grades/IronOreGrades.csv"))  
    
    # if no ore grades are available for a region, assume 0.6
    if(i %in% data$base) 
    {concen <- data$Concentration[data$base == i]}else  
    {concen <- 0.6}
    
    if(SUT["InputsFromNature","Mining"] > 0) {
    iron <- SUT["InputsFromNature","Mining"]*concen
    # iron ore weight with 60% grade, when concentration is higher than 0.6, leave it like it is
    if(concen <= 0.6) {value_new <- iron/0.6} else
    {value_new <- iron/concen}
    # Calculate waste flow 
    waste <- value-value_new
    # Allocate output of iron ore (now 60% grade) to mining
    SUT["Mining","Iron ore"] <- value_new
    # Allocate waste rock to the supply of waste by mining
    SUT["Mining","Waste"] <- waste }
    
    # 2.2 Write pig iron production and use yields to estimate waste/slag flows
    data <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_PigIron.csv"))
    
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["Blast furnace","Pig iron"] <- value
      # Take yield from WSA
      yield <- WSA_yield$Average[WSA_yield$Process == 'BF slag rate']/1000
      # Estimate slag output
      slag <- value * yield
      # Estimate other outputs (top gas and dust)
      other <- value *((1/0.65)-1)
      waste <- slag + other
      SUT["Blast furnace","Waste"] <- waste }
    
    # 2.3 Sponge iron
    data <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_SpongeIron.csv"))
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["Direct reduction","Sponge iron"] <- value
      # Allocate slag output assuming losses of 5% of sponge iron output
      #yield <- 0.05
      #waste <- value * yield
      # Alternatively allocate losses including gangue by assuming waste of 600 kg per ton of useful output
      waste <- value * 0.6
      SUT["Direct reduction","Waste"] <- waste }
      
    # 2.4 Write steel production of BOF & OHF
    data_1 <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_SteelOxygenBlownConverters.csv"))
    data_2 <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_SteelOpenHearthFurnaces.csv"))
    data <- rbind(data_1,data_2) %>% group_by(base) %>% summarise(Quantity = sum(Quantity))
    remove(data_1,data_2)
    
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["Oxygen blown & open hearth furnace","Liquid steel"] <- value
      # Calculate waste
      yield <- WSA_yield$Average[WSA_yield$Process == 'BOF yield']
      waste <- (value/yield)-value
      SUT["Oxygen blown & open hearth furnace","Waste"] <- waste }
    
    # 2.5 Write steel production of EAF
    data <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_SteelElectricFurnaces.csv"))
    
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["Electric arc furnace","Liquid steel"] <- value
      # Calculate waste
      yield <- WSA_yield$Average[WSA_yield$Process == 'EAF yield']
      waste <- (value/yield)-value
      SUT["Electric arc furnace","Waste"] <- waste }
    
    # 2.6 Write flat rolling production and forming scrap
    data <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_FlatRolledProducts.csv"))
    
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["Flat rolling","Flat rolled products"] <- value
      # Calculate waste
      yield <- WSA_yield$Average[WSA_yield$Process == 'Slab caster yield']
      waste <- (value/yield)-value
      SUT["Flat rolling","Forming & fabrication scrap"] <- waste }
    
    # 2.7 Write long rolling production and forming scrap
    data <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_LongRolledProducts.csv"))
    
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["Long rolling","Long rolled products"] <- value
      # Calculate waste
      yield <- WSA_yield$Average[WSA_yield$Process == 'Bloom caster yield']
      waste <- (value/yield)-value
      SUT["Long rolling","Forming & fabrication scrap"] <- waste }
    
    # 2.8 Write production of ingots and subsequently fill the gap of the casting
    # process with slabs (because all feeds into flat rolling)
    data <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_Ingots.csv"))
    
    if(i %in% data$base) {
      value <- data$Quantity[data$base == i]
      SUT["Casting of ingots & slabs","Ingots"] <- value
      value <- SUT["Flat rolling","Flat rolled products"] - SUT["Casting of ingots & slabs","Ingots"]
      SUT["Casting of ingots & slabs","Slabs"] <- value
      yield <- WSA_yield$Average[WSA_yield$Process == 'Slab caster yield']
      value <- SUT["Flat rolling","Flat rolled products"]
      waste <- (value/yield)-value
      SUT["Casting of ingots & slabs","Forming & fabrication scrap"] <- waste }
    
    # 2.9 For the production value of billets and blooms use the value of long rolling
      value <- sum(SUT["Long rolling",c("Long rolled products","Forming & fabrication scrap")])
      SUT["Casting of billets & blooms","Billets & blooms"] <- value
      # Estimate scrap flows using yield data
      yield <- WSA_yield$Average[WSA_yield$Process == 'Bloom caster yield']
      waste <- (value/yield)-value
      SUT["Casting of billets & blooms","Forming & fabrication scrap"] <- waste 
      
    # 2.10 Fabrication i.e. manufacturing output
    data <- read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_FabricationToFinalDemand.csv")) %>%
      filter(From.Region == i) %>% select(From.Product,Quantity) %>% group_by(From.Product) %>%
      summarise(Quantity = sum(Quantity))
    # Set up index to write values directly into cells
    index <- cbind(base$industry$Name[11:20],base$product$Name[13:22])
    SUT[index] <- data$Quantity
      
    # 2.11 Fabrication scarp
    data <- read.csv(paste0(path$IE_Processed,"/FabricationScrap/FabricationScrap_",year,".csv")) %>%
      filter(base == i)
    data <- data.frame("commodity" = as.character(data$commodity),
                       "Quantity" = data$Quantity,
                       stringsAsFactors = FALSE)
    index <- as.data.frame(index,stringsAsFactors = FALSE)
    colnames(index) <- c("Industry","Product")
    data <- left_join(index,data,c("Product" = "commodity"),copy = FALSE)
    
    SUT[data$Industry,"Forming & fabrication scrap"] <- data$Quantity

    # 2.12 Estimate production of scrap steel by summing over "forming and fabrication scrap"
    # and EoL scrap supply
    
    value <- sum(SUT[,"Forming & fabrication scrap"]) + SUT["EOLScrap","Scrap preparation"]
    SUT["Scrap preparation","Scrap steel"] <- value  
    
    ############################################################################
    # 3. Filling use table
    # IMPORTANT: The allocation follows the logic to source data on exports from the
    # BACI data set and subtract this value from the domestic production of the upstream process 
    # to get an estimate for the domestic inputs of each process. Where this results in negative values
    # for domestic use, we assume that inputs are 50% from foreign source
    
    # 3.1 Steel in final demand
    data <- read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_SteelInFinalDemand.csv")) %>%
      filter(From.Region == i & To.Region == i)
    index <- base$product$Name[base$product$Code %in% data$Product]
    SUT[index,"FinalConsumption"] <- data$Quantity
    
    # 3.2 waste discarded back to the environment
    SUT["Waste","Environment"] <- sum(SUT[,"Waste"])
    
    # 3.3.1 Flat Rolled products used in fabrication
    item <- "Flat rolled products"
    # Load map to allocate to end use (fabrication)
    map <- makeEndUseMap(i,"Flat")
    map <- map[order(map$index),]
    share <- map$Share
    users <- base$industry$Code[11:20][map$index]
    # Load trade data 
    data <- filter(BACI,Product == base$product$Code[base$product$Name == item]) %>% select(From,To,quantity)
    # Read domestic production
    pro <- SUT["Flat rolling","Flat rolled products"]
    # Read domestic use
    use <- sum(Q["Flat",IO.codes$index[IO.codes$base == i]])
    
    # Execute function for estimation script to write values
    SUT <- AllocateSupply2Use(SUT,item,share,users,data,pro,use,i)
    
    # 3.3.2 Long Rolled products used in fabrication
    item <- "Long rolled products"
    # Load map to allocate to end use (fabrication)
    map <- makeEndUseMap(i,"Long")
    map <- map[order(map$index),]
    share <- map$Share
    users <- base$industry$Code[11:20][map$index]
    # Load trade data 
    data <- filter(BACI,Product == base$product$Code[base$product$Name == item]) %>% select(From,To,quantity)
    # Read domestic production
    pro <- SUT["Long rolling",item]
    # Read domestic use
    use <- sum(Q["Long",IO.codes$index[IO.codes$base == i]])
    
    # Execute function for estimation script to write values
    SUT <- AllocateSupply2Use(SUT,item,share,users,data,pro,use,i)
    
    # 3.4 Use of forming and fabrication scrap by scrap preparation
    SUT["Forming & fabrication scrap","Scrap preparation"] <- sum(SUT[,"Forming & fabrication scrap"])
    
    # 3.5 Use of billets and blooms by long rolling
    # Load number for exports of billets and blooms 
    item <- "Billets & blooms"
    users <- "Long rolling"
    share <- 1
    data <- filter(BACI,Product == base$product$Code[base$product$Name == item]) %>% select(From,To,quantity)
    pro <- SUT["Casting of billets & blooms",item]
    use <- sum(SUT[users,])
    
    # Execute function for estimation script to write values
    SUT <- AllocateSupply2Use(SUT,item,share,users,data,pro,use,i)
    
    # 3.6 Use of ingots and slabs by flat rolling
    # 3.6.1 Ingots
    export <- filter(BACI,Product == base$product$Code[base$product$Name == "Ingots"]) %>% 
      select(From,To,quantity) %>% filter(From == i) 
    value <- SUT["Casting of ingots & slabs","Ingots"] - sum(export$quantity)
    # In case value is negative, assume that 5% of inputs is coming from domestic ingots
    if(value > 0) {SUT["Ingots","Flat rolling"] <- value} else
    {SUT["Ingots","Flat rolling"] <- sum(SUT["Flat rolling",]) * 0.05}
    
    # 3.6.2 Slabs
    export <-  filter(BACI,Product == base$product$Code[base$product$Name == "Slabs"]) %>% 
      select(From,To,quantity) %>% filter(From == i) 
    # Domestic production of slabs minus exports = domestic use
    value <- SUT["Casting of ingots & slabs","Slabs"] - sum(export$quantity)
    if(value > 0) {SUT["Slabs","Flat rolling"] <- value} else
    {SUT["Slabs","Flat rolling"] <-  sum(SUT["Flat rolling",]) * 0.45}
    
    # 3.7 Casting of billets and blooms and casting of ingots and slabs
    # The only input to casting is liquid steel which is not a traded commodity, 
    # The inputs to casting are therefor estimated by using the production/output value
    SUT["Liquid steel","Casting of billets & blooms"] <- sum(SUT["Casting of billets & blooms",])
    SUT["Liquid steel","Casting of ingots & slabs"] <- sum(SUT["Casting of ingots & slabs",])
    
    # 3.8 Sponge iron used by EAF
    export <- filter(BACI,Product == base$product$Code[base$product$Name == "Sponge iron"]) %>%
      filter(From == i) 
    
    if(SUT["Direct reduction","Sponge iron"] != 0) 
    {
      value <- SUT["Direct reduction","Sponge iron"] - sum(export$quantity)
      if(value > 0) {SUT["Sponge iron","Electric arc furnace"] <- value} else
      {SUT["Sponge iron","Electric arc furnace"] <- sum(SUT["Direct reduction","Sponge iron"])/2}
    }
    
    # 3.9 Allocating iron ores used by direct reduction
    SUT["Iron ore","Direct reduction"] <- sum(SUT["Direct reduction",])
    
    # 3.10 Allocate iron ores used by Blast furnace 
    export <- filter(BACI,Product == base$product$Code[base$product$Name == "Iron ore"]) %>%
      filter(From == i) 
    value <- SUT["Mining","Iron ore"] - sum(export$quantity)
    if(value > 0) {SUT["Iron ore","Blast furnace"] <- value} else
    {SUT["Iron ore","Blast furnace"] <- sum(SUT["Blast furnace",])/2}
    
    # 3.11 Allocating pig iron to BOF
    export <- filter(BACI,Product == base$product$Code[base$product$Name == "Pig iron"]) %>%
      filter(From == i) 
    value <- SUT["Blast furnace","Pig iron"] - sum(export$quantity)
    if(value > 0) {SUT["Pig iron","Oxygen blown & open hearth furnace"] <- value} else
    {SUT["Pig iron","Oxygen blown & open hearth furnace"] <- sum(SUT["Oxygen blown & open hearth furnace",])*0.4}
    
    # 3.12 Allocate Scrap steel to BOF and EAF
    
    SUT["Scrap steel","Oxygen blown & open hearth furnace"] <- sum(SUT["Oxygen blown & open hearth furnace",])*0.15
    SUT["Scrap steel","Electric arc furnace"] <- sum(SUT["Electric arc furnace",]) - SUT["Direct reduction","Electric arc furnace"]
    
    # 4. Save SUT
    # Check if subfolder in processed data exists and if not create it
    path_set <- paste0(path$IE_Processed,"/SUT")
    if(!dir.exists(path_set)) dir.create(path_set)
    # Setting decimals to two digits
    SUT <- round(SUT,2)
    print(min(SUT))
    # Decompose SUT into single elements, that is supply, use, final demand, inputs from nature and eol scrap
    Use <- SUT[base$product$Name,base$industry$Name]
    Supply <- SUT[base$industry$Name,base$product$Name]
    # What is final demand in monetary IO is called hereafter boundary output
    BoundaryOutput <- SUT[base$product$Name,c("FinalConsumption","Environment")]
    
    # What is value added in monetary IO is called hereafter boundary input
    
    Nature <- matrix(SUT["InputsFromNature",base$industry$Name],1,nrow(base$industry))
    EolScrap <- matrix(SUT["EOLScrap",base$industry$Name],1,nrow(base$industry))
    
    # Remove all column and row names
    colnames(Use) <- NULL
    rownames(Use) <- NULL
    colnames(Supply) <- NULL
    rownames(Supply) <- NULL
    colnames(BoundaryOutput) <- NULL
    rownames(BoundaryOutput) <- NULL
    colnames(Nature) <- NULL
    rownames(Nature) <- NULL
    colnames(EolScrap) <- NULL
    rownames(EolScrap) <- NULL
    
    # Write Supply, Use, In- and Output to root folder, note that because write.csv will always export colnames
    # had to use the write.tabel function.
    
    write.table(Supply,file = paste0(path_set,"/",year,"_DomesticSupply_Region",i,".csv"),
                col.names = FALSE,
                row.names = FALSE,
                sep = ",")
    
    write.table(Use,file = paste0(path_set,"/",year,"_DomesticUse_Region",i,".csv"),
                col.names = FALSE,
                row.names = FALSE,
                sep = ",")
    
    write.table(BoundaryOutput,file = paste0(path_set,"/",year,"_BoundaryOutput_Region",i,".csv"),
                col.names = FALSE,
                row.names = FALSE,
                sep = ",")
    
    write.table(Nature,file = paste0(path_set,"/",year,"_InputsFromNature_Region",i,".csv"),
                col.names = FALSE,
                row.names = FALSE,
                sep = ",")
    
    write.table(EolScrap,file = paste0(path_set,"/",year,"_EolScrap_Region",i,".csv"),
                col.names = FALSE,
                row.names = FALSE,
                sep = ",")
    
    # Check if the mother directory really exists and write tables to file
    if(dir.exists(path$mother))
    {
      write.table(Supply,file = paste0(path$mother,"Data/IE/",year,"_DomesticSupply_Region",i,".csv"),
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
      write.table(Use,file = paste0(path$mother,"Data/IE/",year,"_DomesticUse_Region",i,".csv"),
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
      write.table(BoundaryOutput,file = paste0(path$mother,"Data/IE/",year,"_BoundaryOutput_Region",i,".csv"),
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
      write.table(Nature,file = paste0(path$mother,"Data/IE/",year,"_InputsFromNature_Region",i,".csv"),
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
      write.table(EolScrap,file = paste0(path$mother,"Data/IE/",year,"_EolScrap_Region",i,".csv"),
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
      
    }
    # In case for debugging:
    #print(paste0("Region ",i," of ",n_reg))
    #print(paste0("Minimum value: ",min(SUT)))
  }
  print("IEDataProcessing_PIOLab_BuildingDomesticTables finished.")
}
