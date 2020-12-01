################################################################################
# Creates country profiles for a selected set of material flow indicators

Compile_RegionProfile <- function(IOT,Code)
{
  
  U <- SUT$U
  S <- SUT$S
  Z <- IOT$Z
  L <- IOT$L
  
  y <- IOT$y
  y <- Agg(y, rep(Code$Y$RegionCode,each = 6),2)
  
  e <- IOT$e
  
  x <- rowSums(L %*% y) # Estimate gross industry output from variables
  
  q <- colSums(S)  # intermediate output of products
  
  # Write product output in matrix for ease of access:
  Q <- matrix(data = q, nrow = num$pro, ncol = num$reg)
  colnames(Q) <- base$region$Name
  rownames(Q) <- base$product$Name
  
  # Load and store raw data:
  Setting <- list( "WSA" = read.xlsx(xlsxFile = paste0(path$settings,"/datafeeds_settings/WSA_settings.xlsx"), sheet = 1 ),
                   "IE" = read.xlsx(xlsxFile = paste0(path$settings,"/datafeeds_settings/IE_settings.xlsx"), sheet = 1 ))
  
  Setting$WSA["path"] <- paste0(path$IE_processed,"/WSA/WSA_",job$year,"_",Setting$WSA$FeedName,".csv")
  
  RawData <- list( "FinalDemand" = read.csv( paste0(path$IE_processed,"/EXIOWasteMFAIO/",job$year,"_","FinalDemand.csv") ),
                   "Fab2Demand" = read.csv( paste0(path$IE_processed,"/EXIOWasteMFAIO/",job$year,"_","Fabrication2FinalDemand.csv") ),
                   "BACI" = read.csv( paste0(path$IE_processed,"/BACI/BACI_",job$year,".csv") ),
                   "Eol" = read.csv( paste0(path$IE_processed,"/EOL/EOL_",job$year,".csv") ),
                   "IRP" = read.csv( paste0(path$IE_processed,"/IRP/IRP_",job$year,".csv") ),
                   "Grade" = read.csv( paste0(path$IE_processed,"/Grades/IronOreGrades.csv") ),
                   "Yield" = select(read.csv( paste0(path$IE_processed,"/WSA/SteelIndustryYields.csv")),Process,Average),
                   "WSA" = lapply(X = Setting$WSA$path, FUN = read.csv),
                   "Forgings" = read.csv(paste0(path$IE_processed,"/AligningData/Forgings_",job$year,".csv") ) 
                  )
  
  # Load and aggregate population data
  pop <- read.xlsx(paste0(path$repo,"/input/EXIOBASE population data.xlsx"),sheet = 3) %>%
    select(EXIOcode,as.character(job$year)) 
  
  pop <- pop[1:49,]
  colnames(pop) <- c("RegionCode","value")
  
  # If the IOT is industry by industry, select industry codes:
  if(nrow(IOT$Z) == ncol(IOT$Z) & nrow(IOT$Z) == ( num$ind * num$reg )) sec_sel <- "Industry"
  
  # If the IOT is product by product, select product codes:
  if(nrow(IOT$Z) == ncol(IOT$Z) & nrow(IOT$Z) == ( num$pro * num$reg )) sec_sel <- "Product"
  
  index <- Code$Z[Code$Z$EntityName == sec_sel,]  # Select codes
  

  
  # Set up calculation function
  calculate <- function(data,i)
  {
    # Calculate multipliers and flow matrix
    MP <- L * intens[,i]
    FP <- MP%*%y
    # Aggregate into region by region flow matrix
    
    FP <- Agg(x = FP, aggkey = index$RegionCode, dim = 1)
    
    flow <- Code$V$Entity[i]
    
    data[flow] <- rowSums(FP)
    data[paste0("RMC_",flow)] <- colSums(FP)
    data[paste0("Import_",flow)] <- colSums(FP) - diag(FP)
    data[paste0("Export_",flow)] <- rowSums(FP) - diag(FP)
    
    return(data)
  }
  
  data <- data.frame("Index" = 1:length(unique(Code$Z$RegionName)),
                     "Region" = unique(Code$Z$RegionName),
                     "Population" = pop$value)
  
  # Calculate direct intensities
  intens <- e/x
  # intens[intens == Inf] <- 0
  
  for(i in Code$V$index)
  {
    print(Code$V$Entity[i])
    data <- calculate(data,i)
  }
  
  
  Compare_IronMaking <- data[,c("Index","Region")]  # Create empty sheet for comparison
  Compare_IronMaking["PigIron_AISHA"] <- Q[rownames(Q) == "Pig iron",]  # Write IO result
  Compare_IronMaking["PigIron_RawData"] <- 0  # Create column for raw data
  df <- RawData$WSA[[2]]  # Read raw data from IE
  Compare_IronMaking[df$base,"PigIron_RawData"] <- df$Quantity  # Write raw data  
  
  #Compare_IronMaking["SlagPerUnit"] <- data["Landfill"]/Compare_IronMaking["PigIron_AISHA"]
  #Compare_IronMaking["TopGasPerUnit"] <- data["Atmosphere"]/Compare_IronMaking["PigIron_AISHA"]
  #Compare_IronMaking["IronOrePerUnit"] <- colSums(U[,IO_codes_industry$Index[IO_codes_industry$SectorName == "Blast furnace"]])/Compare_IronMaking["PigIron_AISHA"]
  #Compare_IronMaking["CokePerUnit"] <- v[E_codes$index[E_codes$Entity == "Coke"],IO_codes_industry$Index[IO_codes_industry$SectorName == "Blast furnace"]]/Compare_IronMaking["PigIron_AISHA"]
  #Compare_IronMaking["AirPerUnit"] <- v[E_codes$index[E_codes$Entity == "Air"],IO_codes_industry$Index[IO_codes_industry$SectorName == "Blast furnace"]]/Compare_IronMaking["PigIron_AISHA"]
  
  Compare_SteelMaking <- data[,c("Index","Region")]
  Compare_SteelMaking["OBFSteel_AISHA"] <-  Q[rownames(Q) == "Liquid steel OBF",]  # Write IO result
  Compare_SteelMaking["OBFSteel_RawData"] <- 0
  df <- RawData$WSA[[4]]  # Read raw data from IE
  Compare_SteelMaking[df$base ,"OBFSteel_RawData"] <- df$Quantity 
  
  Compare_DE <- data[,c("Index","Region","Crude Ore")]
  Compare_DE["RawData"] <- 0
  Compare_DE[RawData$IRP$base,"RawData"] <- RawData$IRP$Quantity
  
  Compare_EOL <- data[,c("Index","Region","End-of-Life Scrap")]
  Compare_EOL["RawData"] <- 0
  Compare_EOL[RawData$Eol$base,"RawData"] <- RawData$Eol$Quantity
  
  # Result: Direct flows
  y <- Agg(x = y,aggkey = index$RegionCode,dim = 1)
  
  data["FinalDemand"] <- colSums(y)
  
  Z <- Agg(x = Z,aggkey = index$RegionCode,1)
  Z <- Agg(x = Z,aggkey = index$RegionCode,2)
  
  Direct <- Z + y
  
  data["Import"] <- colSums(Direct) - diag(Direct)
  data["Export"] <- rowSums(Direct) - diag(Direct)
  data["DMC"] <- data[Code$V$Entity[4]] + data["Import"] - data["Export"]
  
  # DMC total
  E_sel <- Code$V$Entity[Code$V$Boundary == "Input"]
  Input <- data[E_sel[1]]
  for(i in 2:length(E_sel)) Input <- Input + data[E_sel[i]]
  data["DMC_inclAll"] <- data["DMC"] + Input
  
  # RMC supply total
  Input <- data[paste0("RMC_",E_sel[1])]
  for(i in 2:length(E_sel)) Input <- Input + data[paste0("RMC_",E_sel[i])]
  data["RMC_Supply"] <- Input
  
  # For RMC Use
  E_sel <- Code$V$Entity[Code$V$Boundary == "Output"]
  Output <- data[paste0("RMC_",E_sel[1])]
  for(i in 2:length(E_sel)) Output <- Output + data[paste0("RMC_",E_sel[i])]
  data["RMC_Use"] <- data["FinalDemand"] +  Output
  
  Compare_PerCapita <- data[,c("DMC","RMC_Crude Ore","DMC_inclAll","RMC_Supply","RMC_Use")]
  Compare_PerCapita <- Compare_PerCapita/data$Population
  Compare_PerCapita <- cbind(data[,c("Index","Region")],Compare_PerCapita) 
    
  # Write results to output folder
  
  export <- list("Compare_DE" = Compare_DE,
                 "Compare_EOL" = Compare_EOL,
                 "Compare_IronMaking" =  Compare_IronMaking,
                 "Compare_SteelMaking" = Compare_SteelMaking,
                 "Compare_PerCapita" = Compare_PerCapita,
                 "Other" =  data)
  
  sheetnames <- list("DE","EoLScrap","PigIron","SteelBOF","PerCap","All")
  
  write.xlsx(export,
             file = paste0(path$repo,"/output/",job$folder,"_RegionProfiles_",job$year,".xlsx"),
             sheetName = sheetnames)
  
  return(export)
}






