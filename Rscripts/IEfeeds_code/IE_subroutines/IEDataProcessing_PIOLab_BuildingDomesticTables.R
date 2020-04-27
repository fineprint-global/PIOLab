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
  # !diagnostics off
  
  print("IEDataProcessing_PIOLab_BuildingDomesticTables initiated.")
  
  # Add path to IE classification setting file:
  
  path[["IE_classification"]] <- paste0(path$Settings,"/Base/",IEdatafeed_name,"_BaseSectorClassification.xlsx")
  
  # Load prorating function and Number2File:
  
  source(paste0(path$Subroutines,"/Prorate.R"))
  
  source(paste0(path$Subroutines,"/Numbers2File.R"))
 
  
  Codes <- list( "WIO" = read.csv(file = paste0(path$IE_Processed,"/EXIOWasteMFAIO/IO_codes.csv") ) )
  
  Setting <- list( "WSA" = read.xlsx(xlsxFile = paste0(path$Settings,"/datafeeds_settings/WSA_settings.xlsx"), sheet = 1 ),
                   "IE" = read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"), sheet = 1 ))
  
  Setting$WSA["path"] <- paste0(path$IE_Processed,"/WSA/WSA_",year,"_",Setting$WSA$FeedName,".csv")
  
  
  # Load flow data where all regions are included in one object:
  
  data <- list( "FinalDemand" =  read.csv( paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_","FinalDemand.csv") ),
                "Fab2Demand" = read.csv( paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_","Fabrication2FinalDemand.csv") ),
                "BACI" =  read.csv(paste0(path$IE_Processed,"/BACI/BACI_",year,".csv") ),
                "Eol" = read.csv(paste0(path$IE_Processed,"/EOL/EOL_",year,".csv") ),
                "IRP" = read.csv(paste0(path$IE_Processed,"/IRP/IRP_",year,".csv") ),
                "Grade" = read.csv(paste0(path$IE_Processed,"/Grades/IronOreGrades.csv") ),
                "Yield" = select(read.csv(paste0(path$IE_Processed,"/WSA/SteelIndustryYields.csv")),Process,Average),
                "WSA" = lapply(X = Setting$WSA$path, FUN = read.csv),
                "Forgings" = read.csv(paste0(path$IE_Processed,"/AligningData/Forgings_",year,".csv") ) 
              )
  
  # Load WSA Source2Root concordances
  
  set <- read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"),sheet = 2)
  
  path_sel <- list("flow" = paste0(path$Concordance,"/WSA/",
                                   set$date[set$aggregator == "sector"],"_WSA_Source2Root_Product.csv"),
                   "process" = paste0(path$Concordance,"/WSA/",
                                      set$date[set$aggregator == "sector"],"_WSA_Source2Root_Industry.csv")
                   )
  
  S2R <- list( "WSA" = list( "industry" = as.matrix( read.csv(path_sel$process,header = FALSE) ),
                             "product" = as.matrix( read.csv(path_sel$flow,header = FALSE) )
                            )
              )
  
  # Store root to mother concordances in list object:
  
  R2M_sel <- list( "WSA" = list( "industry" = R2M$process,
                                 "product" = R2M$flow
                                 ) 
                   )
  
  # Normalize source to root concordances (create maps)
  
  for(j in 1:2) S2R$WSA[[j]] <- as.matrix( S2R$WSA[[j]] / rowSums(S2R$WSA[[j]]) ) 
  
  S2M <- list("WSA" = list())  # Create empty list to store source to mother maps
  
  # Create Source to mother map:
  
  for(j in 1:2) S2M$WSA[[j]] <- S2R$WSA[[j]] %*% R2M_sel$WSA[[j]]
  
  # Load SUT templates
  
  SUT_temp <- list( "Supply" = as.matrix( read.xlsx(path$IE_classification, sheet = 5,rowNames = TRUE) ),
                    "Use" = as.matrix( read.xlsx(path$IE_classification, sheet = 6,rowNames = TRUE) )
                  )
  
  # Load allocation function
  
  # source(paste0(path$Subroutines,"/AllocateSupply2Use.R"))
  
  # Define general variables:
  
  # num <- list("pro" = nrow(base$product),
  #             "ind" = nrow(base$industry),
  #             "reg" = nrow(base$region),
  #             "va" = nrow(base$input),
  #             "fd" = nrow(base$demand) )
             
  for(i in 1:num$reg)
  {
    print(paste("Compiling",base$region$Name[i]))
    
    # Create empty SUT:
    
    SUT <- data.frame(matrix(0,(num$flow + num$process + num$input),(num$flow + num$process + num$demand)))
    
    colnames(SUT) <- c(base$process$Name,
                       base$flow$Name,
                       base$demand$Name)
    
    rownames(SUT) <- c(base$process$Name,
                       base$flow$Name,
                       base$input$Name)
    
    SUT <- as.matrix(SUT) # Transform to matrix write numbers using matrix indices
    
    
    ### Flows associated with fabrication ###
    
    
    # Load data on fabrication use:
    
    data_sel <- read.csv(paste0(path$IE_Processed,"/Cullen/FabricationUse_",
                            year,"_",base$region$Name[i],".csv"),
                     header = FALSE)
    
    # Store matrix index in list:
    
    index <- list("row" = num$process + filter(base$flow,Type == "Finished") %>% pull(Code),
                  "col" = filter(base$process,Type == "Final") %>% pull(Code))
  
    SUT[index$row,index$col] <- as.matrix(data_sel)  # Write into table
        
    # Load data on fabrication scrap output
    
    data_sel <- read.csv(paste0(path$IE_Processed,"/Cullen/FabricationScrap_",
                            year,"_",base$region$Name[i],".csv"))
    
    # Matrix indices in list:
    
    index <- list("row" = data_sel$base,
                  "col" = num$process + filter(base$flow, Name == "Fabrication scrap") %>% pull(Code))
    
    SUT[index$row,index$col] <- data_sel$Quantity  # Write scrap flows into table
    
    # Read data for useful fabrication output
    
    data_sel <- select(data$Fab2Demand,base.from,sector.from,Quantity) %>% filter(base.from == i) %>% 
      group_by(base.from,sector.from) %>% summarise(Quantity = sum(Quantity)) %>% ungroup(base.from,sector.from)
    
    # Store matrix index in list:
    
    index <- as.matrix(data.frame("row" = filter(base$process,Type == "Final") %>% pull(Code),
                                  "col" = num$process + filter(base$flow,Type == "Final") %>% pull(Code)
                                  )
                       )

    SUT[index] <- data_sel$Quantity  # Write fab output to table
    
    
    # Read data on intermediate use of manufacturing output
    
    data_sel <- data$Fab2Demand %>% filter(base.from == i,base.to == i) %>% 
      select(sector.from,sector.to,Quantity) %>% group_by(sector.from,sector.to) %>% 
      summarise(Quantity = sum(Quantity)) %>% ungroup(sector.from,sector.from)
    
    # Read base sector codes:
    
    index <-data.frame( "row" = num$process + filter(base$flow,Type == "Final") %>% pull(Code),
                        "col" = filter(base$process,Type == "Final") %>% pull(Code) )
                       
    data_sel$sector.from <- index$row[data_sel$sector.from]  # Exchange WIO codes with base row codes
    
    data_sel$sector.to <- index$col[data_sel$sector.to] # Exchange WIO codes with base col codes
    
    # Copy base codes into data frame and transform into matrix:
    
    index <- as.matrix(data_sel[,c("sector.from","sector.to")])   
    
    SUT[index] <- data_sel$Quantity
    
    
    # Read flows from final production sector (manuf.) to final demand category:
    
    data_sel <- data$FinalDemand %>% filter(base.from == i,base.to == i) %>% 
      select(sector,demand,Quantity) %>% group_by(sector,demand) %>% 
      summarise(Quantity = sum(Quantity)) %>% ungroup(sector,sector,demand)
    
    # Read base sector codes:
    
    index <-data.frame( "row" = filter(base$process,Type == "Final") %>% pull(Code))
    
    data_sel$sector <- index$row[data_sel$sector]  # Exchange WIO code with base sector code
    
    data_sel$demand <- data_sel$demand + num$process + num$flow  # Exchange WIO with base sector code
    
    index <- as.matrix( data_sel[,c("sector","demand")] )  # Create matrix for indices
    
    SUT[index] <- data_sel$Quantity
    
    
    ### Output of steelmaking sector ###
    
    # Read WSA id's of processed data that is relevant:
    
    Code_sel <- Setting$WSA %>% filter(Type %in% c("Primary","Secondary","Finished")) %>% 
      select(id, FeedName, Type)
    
    # Use id to extract a list containing the processed data:
    
    data_sel <- data$WSA[Code_sel$id]
    
    # Create vector in list to store the data for country i:
    
    Value <- list( "Source" = vector( mode="integer", length= length(data_sel) ) )
    
    # Write production numbers in vector:
    
    for( j in 1:nrow(Code_sel) ) 
    {
      if(i %in% data_sel[[j]]$base) Value$Source[j] <- filter(data_sel[[j]],base == i) %>% pull(Quantity)
    }
    
    # Map raw data to mother product classification:
    
    Value[["Mother"]] <- colSums( S2M$WSA[[2]][Code_sel$id,] * Value$Source )
    
    # Read indices of steelmaking sector outputs
    
    index <- list( "row" = filter(base$process,Type %in% c("Primary","Secondary","Finished")) %>% pull(Code),
                   "col" = filter(base$flow,Type %in% c("Primary","Secondary","Finished")) %>% pull(Code) 
                  )
    
    Value[["Supply"]] <- SUT_temp$Supply %*% diag(Value$Mother) # Allocate products to industries
    
    # Write values into SUT (note addition of number of industries to column indices):
    
    SUT[index$row, num$process + index$col] <- Value$Supply[index$row,index$col]
    
    ### Add forging production ###
    
    if(i %in% data$Forgings$base)
    {
      SUT["Steel casting and forging","Forgings"] <- filter(data$Forgings,base == i) %>% pull(Quantity)
    }
    
    # Read indices of secondary i.e. crude steel:
    
    index <- data.frame("industry" = filter(base$process,Name %in% paste("Continuous casting of",c("slabs","billets","blooms"))) %>% pull(Code),
                        "product" = filter(base$flow,Name %in% c("Slabs","Billets","Blooms")) %>% pull(Code)
                        )

    
    # Create map to calculate crude steel (secondary) output by estimating demand of downstream processes:
    
    Map <- t( SUT_temp$Use[index$product,] ) / colSums( SUT_temp$Use[index$product,] ) 
    
    Map[is.na(Map)] <- 0
    
    # Estimate crude steel demand/output:
    
    Value <- colSums( Map *  rowSums(SUT[base$process$Code, num$process + base$flow$Code]) )
    
    index$product <- index$product + num$process  # Change product (col) code to write in SUT
    
    SUT[as.matrix(index)] <- Value  # Write value
    
    
    ### Estimate forming scrap  ###
    
    # indices of forming processes:
    
    index <- list( "industry" = filter(base$process, Type == "Finished") %>% pull(Code) )
    
    # Output of forming processes:
    
    Value <- rowSums(SUT[index$industry, num$process + base$flow$Code])
    
    # Scrap = ( Useful output / yield ) - Useful output: 
    
    Value <- ( Value / filter(data$Yield, Process == "Hot rolling yield") %>% pull(Average) ) - Value
    
    SUT[index$industry,"Forming scrap"] <- Value  # Write values in SUT
    
    ##############################################
    ### Estimate final outputs of steel sector ###
    ##############################################
    
    # Read process factor for top gas per pig iron:
    
    factor <- filter(Setting$IE, item == "TopGasPerPigIron") %>% pull(value)
    
    
    # Write emissions in final output quadrant:
    
    SUT["Blast furnace","Atmosphere"] <- SUT["Blast furnace","Pig iron"] * factor
    
    
    # Read slag per unit pig iron and allocate amount to output to landfill
    
    factor <- filter(Setting$IE, item == "SlagPerPigIron") %>% pull(value)
    
    SUT["Blast furnace","Landfill"] <- SUT["Blast furnace","Pig iron"] * factor
    
    
    # Read gangue per unit sponge iron factor and add amount to landfill:
    
    factor <- filter(Setting$IE, item == "GanguePerSpongeIron") %>% pull(value)
    
    SUT["Direct reduction","Landfill"] <- SUT["Direct reduction","Sponge iron"] * factor
    
    
    # Read oxygen blown furnace yield and use this for both OBF and open hearth furnace:
    
    factor <- filter(data$Yield,Process == "BOF yield") %>% pull(Average)
    
    # Read sector indices: 
    
    index <- data.frame("industry" = filter(base$process, Name %in% c("Basic oxygen converter","Open hearth furnace")) %>% pull(Code),
                        "product" = num$process + filter(base$flow, Name %in% c("Liquid steel OBF","Liquid steel OHF")) %>% pull(Code)
                        )
    
    # Write flow to landfill:
    
    SUT[index$industry,"Landfill"] <- ( SUT[as.matrix(index)] / factor ) - SUT[as.matrix(index)] 
    
    
    # Read electric arc furnace yield and estimate waste flow:
    
    factor <- filter(data$Yield,Process == "EAF yield") %>% pull(Average)
    
    Value <- SUT["Electric arc furnace","Liquid steel EAF"] # EAF steel production
    
    SUT["Electric arc furnace","Landfill"] <- ( Value / factor ) - Value
    
    
    # Read slab and billet caster yields:
    
    factor <- filter( data$Yield,Process %in% c("Slab caster yield","Billet caster yield") ) %>% pull(Average)
    
    index <- data.frame("industry" = filter(base$process,Name %in% paste("Continuous casting of",c("slabs","billets"))) %>% pull(Code),
                        "product" = num$process + filter(base$flow,Name %in% c("Slabs","Billets") ) %>% pull(Code)
                        )
    
    
    SUT[index$industry,"Forming scrap"] <- ( SUT[as.matrix(index)] / factor ) - SUT[as.matrix(index)]
    
    
    # Read bloom caster yield and use this for both blooms and ingost:
    
    factor <- filter( data$Yield,Process == "Bloom caster yield" ) %>% pull(Average)
    
    index <- data.frame("industry" = filter(base$process,Name %in% c("Continuous casting of blooms","Ingot casting")) %>% pull(Code),
                        "product" = num$process + filter(base$flow,Name %in% c("Blooms","Ingots") ) %>% pull(Code)
                        )
    
    SUT[index$industry,"Forming scrap"] <- ( SUT[as.matrix(index)] / factor ) - SUT[as.matrix(index)] 
    
    
    ########################################################
    ### Fill use table of forming processes (= Finished) ###
    ########################################################

    
    # Read indices of products that are used by these processes 
    # Note: Hot rolled coil-sheet-strip is handled separatly:
    
    index <- list( "product" = filter( base$flow, Name %in% c("Slabs","Billets","Blooms","Ingots") ) %>% pull(Code),
                   "industry" = filter( base$process, Type == "Finished" ) %>% pull(Code),
                   "interind" = filter( base$process, Type == "Secondary" ) %>% pull(Code)
                  )
    
    # Read domestic output of intermediate inputs and of process:
    
    DomOut <- list( "Process" = rowSums(SUT[index$industry,]),
                    "Intermed" = rowSums(SUT[index$interind,])
                  )
    
    # Read use structure from template and create map:
    
    Map <- SUT_temp$Use[index$product, index$industry]  
    
    # Create prorated map:
    
    Map <- Prorate(Map, DomOut$Process)
    
    Map[is.na(Map)] <- 0
    
    
    # Read export of intermediates:
    
    export <- filter(data$BACI,From == i,Product %in% index$product) %>% select(Product,Quantity) %>% 
      group_by(Product) %>% summarise(Quantity = sum(Quantity)) %>% ungroup(Product)
    
    
    offset <- min(index$product)-1  # Offset product codes to write in Map
    
    if(length(export) >= 1)
    {
      Value <- DomOut$Intermed
      Value[export$Product-offset] <- DomOut$Intermed[export$Product-offset] - export$Quantity
    }
    
    # Check if exports are unavailable and if so set to zero:
    
    if( length(export) == 0 )
    {
      Value <- DomOut$Intermed
    }
    
    
    
    # Check if negatives exists and replace them with domestic production:
    
    for(j in 1:length(Value)) if(Value[j] < 0) Value[j] <- DomOut$Intermed[j]  
     
    # Mulitply with map and write in use table
    
    SUT[num$process + index$product, index$industry] <- Map * Value
    
    
    # Estimate use of hot rolled coil-sheet-strip products:
    # Note that the use of hot rolled CSS by manufacturing is already accounted for by the WIO model extension
    
    index <- list( "product" = filter( base$flow, Name == "Hot rolled coil-sheet-strip" ) %>% pull(Code),
                   "rolling" = filter( base$process, Name %in% c("Tube welding","Cold rolling mill") ) %>% pull(Code),
                   "manufacturing" = filter( base$process, Type == "Final" ) %>% pull(Code),
                   "interind" = filter( base$process, Name == "Hot strip mill" ) %>% pull(Code)
                  )
    
    # Read domestic output of intermediate inputs (product) and of relevant processes:
    
    DomOut <- list( "rolling" = rowSums(SUT[index$rolling,]),
                    "interind" = sum(SUT[index$interind,]),
                    "manufacturing" = sum(SUT[index$interind,])
                  )
    
    SUT[num$process + index$product, index$rolling] <- DomOut$rolling
    
    
    # Estimate liquid steel use of casting:
    
    index <- list( "product" = filter( base$flow, Name %in% paste("Liquid steel",c("OBF","OHF","EAF")) ) %>% pull(Code),
                   "industry" = filter( base$process, Type == "Secondary" ) %>% pull(Code),
                   "interind" = filter( base$process, Name %in% c("Basic oxygen converter","Open hearth furnace","Electric arc furnace") ) %>% pull(Code)
                  )
    
    # Read domestic output of intermediate inputs and of process:
    
    DomOut <- list( "industry" = rowSums(SUT[index$industry,]),
                    "Interind" = rowSums(SUT[index$interind,])
                  )
    
    # Read use structure from template and create map:
    
    Map <- SUT_temp$Use[index$product, index$industry]  
    
    Map[is.na(Map)] <- 0
    
    Map <- Prorate( Map,  DomOut$industry)  # Prorate map
    
    Value <- Map * DomOut$Interind      # Multiply map with steel production
    
    SUT[num$process + index$product,index$industry] <- Value  # Write into table
    
    
    ### Primary inputs ###
    
    factor <- filter(Setting$IE, item == "AirPerPigIron") %>% pull(value)
    
    SUT["Air","Blast furnace"] <- SUT["Blast furnace","Pig iron"] * factor
    
    factor <- filter(Setting$IE, item == "CokePerPigIron") %>% pull(value)
    
    SUT["Coke","Blast furnace"] <- SUT["Blast furnace","Pig iron"] * factor
    
    factor <- filter(Setting$IE, item == "FluxPerPigIron") %>% pull(value)
    
    SUT["Flux","Blast furnace"] <- SUT["Blast furnace","Pig iron"] * factor
    
    # EoL scrap: 
    
    if(i %in% data$Eol$base) SUT["End-of-Life Scrap","Scrap preparation"] <- data$Eol$Quantity[data$Eol$base == i] 
    
    # IRP iron ore extraction
    
    if(i %in% data$IRP$base) 
    {
      crude <- data$IRP$Quantity[data$IRP$base == i]
      
      SUT["Crude Ore","Mining"] <- crude
      
      # if no ore grades are available for a region, assume 0.6:
      
      if(i %in% data$Grade$base) concen <- data$Grade$Concentration[data$Grade$base == i]
      
      if(!i %in% data$Grade$base) concen <- 0.6
      
      # Calculate iron content:
      
      iron <- crude * concen
      
      if(concen <= 0.6) ore <- iron / 0.6  # iron ore weight when 60% grade (ready for shipping) 
      
      if(concen > 0.6) ore <- iron / concen  # when concentration is > 0.6: not change
      
      SUT["Mining","Landfill"] <- crude - ore  # Waste to landfill (supply-side of mining)
      
      SUT["Mining","Iron ore"] <- ore  # Supply of iron ore (now 60% grade) by mining
    }
    
 
    ### Allocate iron ore use to ironmaking ###
    
    index <- list( "supply" = data.frame("industry" = filter(base$process, Name %in% c("Blast furnace","Direct reduction")) %>% pull(Code),
                                         "product" = num$process + filter(base$flow, Name %in% c("Pig iron","Sponge iron")) %>% pull(Code)
                                         ),
                   "use" =  data.frame("industry" = filter(base$process, Name == "Mining") %>% pull(Code),
                                       "product" = num$process + filter(base$flow, Name == "Iron ore") %>% pull(Code)
                                      )
                  )
    
    # Read factors for iron ore inputs:
    
    factor <- filter(Setting$IE, item %in% c("OrePerPigIron","OrePerSpongeIron") ) %>% pull(value)
    
    # Read primary iron output and estimate total iron ore input with factors:
    
    TotIn <- SUT[ as.matrix(index$supply) ] * factor 
    
    DomOut <- SUT[as.matrix(index$use)]  # Read domestic production of iron ore
    
    if(DomOut > 0)
    {
      # Read export of iron ore:
      
      export <- filter(data$BACI,From == i,Product %in% (index$use$product-num$process) ) %>% select(Product,Quantity) %>% 
        group_by(Product) %>% summarise(Quantity = sum(Quantity)) %>% ungroup(Product) %>% pull(Quantity)
      
      if( length(export) == 0 ) export <- 0  # Check if exports are unavailable and if so set to zero
      
      Value <- DomOut - export  
      
      if(Value < 0) Value <- DomOut  # Set to domestic production if exports too large
      
      Map <- (TotIn/sum(TotIn))  # Create map
      
      Map[is.na(Map)] <- 1  # Set 1 if NA
      
      # Write values into table using map:
      
      SUT[index$use$product,index$supply$industry] <- Map * Value
      
    }
    
    ### Add forming and scrap use by steelmaking ###
    
    steel <- data.frame("ind" = filter(base$process, Name %in% c("Basic oxygen converter","Electric arc furnace")) %>% pull(Code),
                        "pro" = num$process + filter(base$flow, Name %in% paste("Liquid steel",c("OBF","EAF"))) %>% pull(Code)
                        )
    
    # Read scrap per unit output factors:
    
    factor <- filter(Setting$IE, item %in% c("ScrapPerBOFsteel","ScrapPerEAFsteel") ) %>% pull(value)
    
    # Create map from output values:
    
    Map <- ( SUT[as.matrix(steel)] * factor ) / sum(SUT[as.matrix(steel)] * factor)
    
    Map[is.na(Map)] <- 0
    
    # Multiply with forming scrap supply and write in table 
    
    SUT["Forming scrap",steel$ind] <- Map * sum(SUT[,"Forming scrap"])
    
    
    
    ### Write fabrication scrap use of waste management (scrap preparation) ###
    
    SUT["Fabrication scrap","Scrap preparation"] <- sum(SUT[,"Fabrication scrap"])
    
    
    ### Write steel scrap output in supply table ###
    
    SUT["Scrap preparation","Steel scrap"] <- sum(SUT[,"Scrap preparation"])
    
    
    ### Write steel scrap use by steelmaking ###
    
    # Read exports of steel scrap:
    
    index <- list("industry" = filter(base$process, Name %in% c("Basic oxygen converter","Electric arc furnace")) %>% pull(Code),
                  "product" = num$process + filter(base$flow, Name == "Steel scrap") %>% pull(Code)
                  )
    
    DomOut <- SUT["Scrap preparation","Steel scrap"]  # Read domestic production of steel scrap
    
    export <- filter(data$BACI,From == i,Product %in% (index$product-num$process) ) %>% select(Product,Quantity) %>% 
      group_by(Product) %>% summarise(Quantity = sum(Quantity)) %>% ungroup(Product) %>% pull(Quantity)
    
  
    
    if(length(export) >= 1)
    {
      Value <- DomOut - export
    }
    
    if( length(export) == 0 ) # Check if exports are unavailable and if so set to zero
    {
      Value <- DomOut
    }
   
    if(Value < 0) Value <- DomOut  # Check if exports are larger
    
    SUT[index$product,index$industry] <- Map * Value  # Write into table 
    
    
    ### Allocate pig and sponge iron to steelmaking ###
    
    index <- list("industry" = filter(base$process, Name %in% c("Basic oxygen converter","Open hearth furnace","Electric arc furnace")) %>% pull(Code),
                  "product" = filter(base$flow, Name %in% c("Pig iron","Sponge iron") ) %>% pull(Code)
                  )
    
    steel <- data.frame("ind" = filter(base$process, Name %in% c("Basic oxygen converter","Open hearth furnace","Electric arc furnace")) %>% pull(Code),
                        "pro" = num$process + filter(base$flow, Name %in% paste("Liquid steel",c("OBF","OHF","EAF"))) %>% pull(Code)
                        )
    
    iron <- data.frame("ind" = filter(base$process, Name %in% c("Blast furnace","Direct reduction") ) %>% pull(Code),
                       "pro" = num$process + filter(base$flow, Name %in% c("Pig iron","Sponge iron") ) %>% pull(Code)
                        )
    
    # Estimate steel and iron outputs:
    
    DomOut <- list("steel" = SUT[as.matrix(steel)],
                   "iron" = SUT[as.matrix(iron)]
                   )
    
    # Read export of pig iron and sponge iron:
    
    export <- filter(data$BACI,From == i,Product %in% (iron$pro - num$process) ) %>% select(Product,Quantity) %>% 
      group_by(Product) %>% summarise(Quantity = sum(Quantity)) %>% ungroup(Product)
    
    offset <- min(iron$pro - num$process)-1
    
    if(length(export) >= 1)
    {
      Value <- DomOut$iron
      Value[export$Product-offset] <- DomOut$iron[export$Product-offset] - export$Quantity
    }
    
    # Check if exports are unavailable and if so set to zero:
    
    if( length(export) == 0 )
    {
      Value <- DomOut$iron
    }
    
    for(j in 1:length(Value)) if(Value[j] < 0) Value[j] <- DomOut$iron[j]  
    
    factor <- filter(Setting$IE, item %in% c("PigIronPerBOFsteel","SpongeIronPerEAFsteel") ) %>% pull(value)
    
    Map <- SUT_temp$Use[index$pro,index$ind]  # Create map for allocation
    
    Demand <- DomOut$steel * c( factor[1],1,factor[2] )  # Estimate iron demand
    
    Map <- Prorate(Map,Demand) # prorate the map
    
    Value <- Map * Value  # Allocate values with map to use
    
    SUT[(index$pro + num$process), index$industry] <- Value  # Write value in table
    
    
    ### Save SUT ###
    
    # Check if subfolder in processed data exists and if not create it
    
    path_set <- paste0(path$IE_Processed,"/SUT")
    
    if(!dir.exists(path_set)) dir.create(path_set)
    
    # Setting decimals to two digits
    
    SUT <- round(SUT,2)
    
    print(min(SUT))
    
    # Decompose SUT into single elements, that is supply, use, final demand, inputs from nature and eol scrap
    
    Use <- SUT[base$flow$Name,base$process$Name]
    
    Supply <- SUT[base$process$Name,base$flow$Name]
    
    # Final output (= final demand + landfill + atmosphere):
    
    FinalOutput <- SUT[base$process$Name,base$demand$Name]

    # Primary inputs:
    
    PrimaryInput <- SUT[base$input$Name,base$process$Name]

    # Remove all column and row names:
    
    colnames(Use) <- NULL
    rownames(Use) <- NULL
    colnames(Supply) <- NULL
    rownames(Supply) <- NULL
    colnames(FinalOutput) <- NULL
    rownames(FinalOutput) <- NULL
    colnames(PrimaryInput) <- NULL
    rownames(PrimaryInput) <- NULL

    # Write tables to folder:

    Numbers2File(Supply, paste0(path_set,"/",year,"_Supply_Region",i,".csv") )
    Numbers2File(Use, paste0(path_set,"/",year,"_Use_Region",i,".csv") )
    Numbers2File(FinalOutput, paste0(path_set,"/",year,"_FinalOutput_Region",i,".csv") )
    Numbers2File(PrimaryInput, paste0(path_set,"/",year,"_PrimaryInput_Region",i,".csv") )
    
    print(paste0("Minimum value: ",min(SUT)))
  }
}
