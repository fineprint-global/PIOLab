# This script computes the results for a single sector

reg <- "Austria"
sec_ind <- "Construction"
sec_pro <- "Construction work"

index <- list( "ind" = Code$Z[Code$Z$EntityCode == 1,],
               "pro" = Code$Z[Code$Z$EntityCode == 2,] )

sector <- list( "ind" = index$ind %>% filter(RegionName == reg, SectorName == sec_ind) %>% 
                  select(RegionCode, RegionName, EntityName, SectorName, SectorIndex),
                "pro" = index$pro %>% filter(RegionName == reg, SectorName == sec_pro) %>% 
                  select(RegionCode, RegionName, EntityName, SectorName, SectorIndex) )



for(year in 2008:2017)
{
  job$year <- year
  print(job$year)
  
  # Create supply use tables from raw data
  SUT <<- Load_SUT("Results")       
  
  # Create PIOTs
  IOT <<- Build_IOT(SUT,"ixi")      # Compile IO model

  ## Compute direct material inputs to target sector
  tmp <- SUT$U[,sector$ind$SectorIndex]
  tmp <- cbind("year" = job$year, index$pro, "value" = tmp) %>% filter(value > 0.001, SectorName != sec_pro) %>% 
    select(year, RegionCode, RegionName, SectorName, value)
  
  if(job$year == 2008)
  {
    result <- tmp
  }else
  {
    result <- rbind(result, tmp)
  }
  
  ## Calculate iron ore footprint of target sector/region
  
  # Aggregate final demand and filter target sector
  y <- Agg(IOT$y, rep(base$region$Name, each = 6), 2)
  y <- y[,colnames(y) == reg]
  
  # Calculate Ore footprints
  q <- IOT$e[, base$input$Code[base$input$Name == "Crude Ore"] ]/IOT$x
  
  # Material multipliers
  MP <- IOT$L * q  
  # Material fooptrints
  FP <- t(t(MP) * y)  
  FP <- Agg(FP, index$ind$RegionName, 1)
  FP <- FP[,sector$ind$SectorIndex]
  
  FP <- data.frame("year" = year,
                   "source_region" = base$region$Name,
                   "value" = FP)
  rownames(FP) <- NULL
  
  if(job$year == 2008)
  {
    result_FP <- FP
  }else
  {
    result_FP <- rbind(result_FP, FP)
  }
}

info <- base$product %>% select(Name, Type_2)
info$Type_2[info$Type_2 == "Final product"] <- "Manufactured product" 

result <- left_join(result, info, by = c("SectorName" = "Name") )
colnames(result)[6] <- "Type"

result_FP <- result_FP[result_FP$value > 0.001, ]

# Write result to xlsx file
filename <- str_c(path$output, "/Sector_Analysis_",reg,"_",sec_ind,".xlsx") 

list_of_datasets <- list("direct_steel_use" = result, "indirect_ore_use" = result_FP)
write.xlsx(list_of_datasets, file = filename)
