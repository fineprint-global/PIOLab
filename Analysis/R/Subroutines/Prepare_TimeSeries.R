################################################################################
# Prepare data set for time series of PSUTs and PIOTs

# Read IO model codes
Code <<- Load_IOCodes()           

# Set path where tables will be saved
folder <- paste0(path$output,"/Tables/")

# Make sure that old processing outputs are deleted before continuing 
if( dir.exists( folder ) ) unlink( folder, recursive = TRUE)
dir.create(folder)
dir.create(paste0(folder,"gPSUT"))
dir.create(paste0(folder,"gPIOT"))

# Transform and clean codes (i.e. labels) for processes
Codes_processes <- Code$Z %>% filter(EntityCode == 1)
Codes_processes$Index <- 1:nrow(Codes_processes)
Codes_processes$SectorIndex <- NULL
Codes_processes$EntityName <- "Process"
colnames(Codes_processes)[6:7] <- c("ProcessCode", "ProcessName")

# Save process codes in folder
write.table( x = Codes_processes, file = paste0(folder, "Codes_Processes.csv"), row.names = FALSE, sep = ";")

# Transform and clean codes (labels) for flows
Codes_flows <- Code$Z %>% filter(EntityCode == 2)
Codes_flows$Index <- 1:nrow(Codes_flows)
Codes_flows$SectorIndex <- NULL
Codes_flows$EntityName <- "Flow"
colnames(Codes_flows)[6:7] <- c("FlowCode","FlowName")

# Save flow codes in folder
write.table( x = Codes_flows, file = paste0(folder, "Codes_Flows.csv"), row.names = FALSE, sep = ";")

# Transform and clean codes (i.e. lables) for boundary inputs
Codes_BoundaryInputs <- Code$V[1:5,]
Codes_BoundaryInputs$Boundary <- c("SEM","SEM","SEM","Nature","Nature")
colnames(Codes_BoundaryInputs)[1] <- "Index"
Codes_BoundaryInputs["Variable"] <- c("O","O","O","E","E")

# Save boundary input codes in folder
write.table( x = Codes_BoundaryInputs, file = paste0(folder, "Codes_BoundaryInputs.csv"), row.names = FALSE, sep = ";")

# Construct codes for boundary outputs
Codes_BoundaryOutputs <- data.frame( "Index" = 1:2,
                                     "Entity" = c("Solid & liquid residuals", "Gaseous residuals"),
                                     "Boundary" = "SEM",
                                     "Variable" = "P")

# Save boundary output codes in folder
write.table( x = Codes_BoundaryOutputs, file = paste0(folder, "Codes_BoundaryOutputs.csv"), row.names = FALSE, sep = ";")

# Construct codes for final use 
Codes_FinalUse <- data.frame("Index" = 1:(num$reg * 6) ,
                             "RegionCode" = rep( 1:num$reg, each = 6),
                             "RegionName" = rep( base$region$Abbrev, each = 6),
                             "CatgoryCode" =  rep( 1:6, num$reg ),
                             "CatgoryName" =  rep( base$demand$Name[1:6], num$reg ) )

# Save final use codes in folder
write.table( x = Codes_FinalUse, file = paste0(folder, "Codes_FinalUse.csv"), row.names = FALSE, sep = ";")


# Compile PSUTs and PIOTs for the time series
for( year in 2008:2017 )
{
  # Selecting year
  job$year <- year
  
  # Create PSUT folder for specific year 
  tmp_path <- paste0( folder, "gPSUT/", year,"/" )
  dir.create( tmp_path )
  
  # Load PSUTs
  SUT <<- Load_SUT("Results")       # Create supply use tables from raw data
  
  # Write variables/tables to folder
  Numbers2File(SUT$S, paste0(tmp_path,"S.csv") )
  Numbers2File(SUT$U, paste0(tmp_path,"U.csv") )
  Numbers2File(SUT$y, paste0(tmp_path,"Y.csv") )
  Numbers2File(SUT$v[1:3,], paste0(tmp_path,"O.csv") )
  Numbers2File(SUT$v[4:5,], paste0(tmp_path,"E.csv") )
  Numbers2File(SUT$w, paste0(tmp_path,"P.csv") )
  
  # Create PIOT folder for specific year
  tmp_path <- paste0( folder, "gPIOT/", year,"/" )
  dir.create( tmp_path )
  
  # Create PIOTs
  IOT <<- Build_IOT(SUT,"ixi")      # Compile IO model
  
  # Write variables/tables to folder
  Numbers2File(IOT$Z, paste0(tmp_path,"Z.csv") )
  Numbers2File(IOT$L, paste0(tmp_path,"L.csv") )
  Numbers2File(IOT$y, paste0(tmp_path,"Y.csv") )
  Numbers2File(IOT$x, paste0(tmp_path,"x_new.csv") )
  Numbers2File(SUT$v[1:3,], paste0(tmp_path,"O.csv") )
  Numbers2File(SUT$v[4:5,], paste0(tmp_path,"E.csv") )
  Numbers2File(SUT$w, paste0(tmp_path,"P.csv") )
  
  # Perform diagnostic tests and plot blast furnace ratios
  Diagnostics()
  Plot_Ratios()
}


