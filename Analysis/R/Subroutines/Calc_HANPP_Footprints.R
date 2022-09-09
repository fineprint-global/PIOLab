
# Loading concordances
conco_root_poly <-  read.csv(file = "./Analysis/input/EPIP/concordance_root_poly.csv")
conco_root_SNL <-  read.csv(file = "./Analysis/input/EPIP/concordance_SNL_roots.csv")
conco_root <- read.csv(file = "./Analysis/input/EPIP/biome_in_root.csv", sep = ";")[,2:3]
R2B <- read.csv( str_c( path$root, "/ConcordanceLibrary/Region Aggregators/032_RegionAggregator.csv"), header = FALSE )
R2B <- as.matrix(R2B)

# Loading NPP0 coefficients of polygons
load(file = str_c("./Analysis/input/EPIP/NPP0_of_polygons.R") )
NPP_poly <- statistic_vals
remove(statistic_vals)
NPP_poly[is.na(NPP_poly)] <- 0
NPP_poly <- data.frame("intensity" = NPP_poly, "PID" = 1:21060 )

# Load SNL production data 
Iron_extraction <- read.csv(file = "./Analysis/input/EPIP/Iron Ore_2016_in_roots.csv") %>% 
  select(GID_id, value)
colnames(Iron_extraction)[2] <- "extraction"


## Transforming EPIP data into base classification

# Filter only polygons that belong to root regions with iron ore extraction
Iron_polygons <- conco_root_poly[conco_root_poly$GID_id %in% Iron_extraction$GID_id,]

# join polygons with NPP intensity and calculate NPP/HANPP
Iron_polygons <- left_join(Iron_polygons, NPP_poly, by = "PID")
Iron_polygons["NPP"] <- Iron_polygons$AREA * Iron_polygons$intensity
Iron_polygons <- Iron_polygons %>% select("GID_id", "AREA", "NPP")

# Aggregate impact data to national level
EPIP_national <- left_join(Iron_polygons, conco_root, by = "GID_id") %>% 
  select("GID_0","AREA","NPP") %>% group_by(GID_0) %>% 
  summarise(AREA = sum(AREA), NPP = sum(NPP))

# Aggregate extraction data to national level
Iron_extraction <- left_join(Iron_extraction, conco_root, by = "GID_id") %>% 
  select("GID_0", "extraction") %>% group_by(GID_0) %>% 
  summarise(extraction = sum(extraction))

# Write EPIP into root region format and aggregate to base classification
E <- data.frame( matrix(0,nrow = 221, ncol = 3) )
colnames(E) <- c("extraction", "AREA", "HANPP")

index <- match(EPIP_national$GID_0, root$region$RootCountryAbbreviation)
E$AREA[index] <- EPIP_national$AREA
E$HANPP[index] <- EPIP_national$NPP
index <- match(Iron_extraction$GID_0, root$region$RootCountryAbbreviation)
E$extraction[index] <- Iron_extraction$extraction
E <- as.matrix(E)
E <- t( t(E) %*% R2B )


## Calculate footprints
x <- rowSums(IOT$L %*% IOT$y)  # Estimate new gross production
q <- IOT$e[, base$input$Code[base$input$Name == "Crude Ore"] ]/x

MP <- IOT$L * q  # Material multipliers

# First for all countries
FP <- MP %*% IOT$y  # Material fooptrints
FP <- Agg(FP, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
FP <- Agg(FP, Code$Z %>% filter(EntityName == "Industry") %>% pull(RegionCode), 1)
colnames(FP) <- base$region$Name
rownames(FP) <- base$region$Name

Q <- E/rowSums(FP)
region_group <- base$region[,2:4]
region_group$Region[15] <- "United States"
region_group$Region[16] <- "Japan"

HANPP <- FP * Q[,3]
HANPP <- melt(HANPP)
colnames(HANPP) <- c("FromRegion","ToRegion","value")
HANPP["indicator"] <- "HANPP"
HANPP <- left_join(HANPP, region_group, by = c("FromRegion" = "Name"))
colnames(HANPP)[5:6] <- c("FromAbbrev", "FromRegionGroup")
HANPP <- left_join(HANPP, region_group, by = c("ToRegion" = "Name"))
colnames(HANPP)[7:8] <- c("ToAbbrev", "ToRegionGroup")

LandUse <- FP * Q[,2]
LandUse <- melt(LandUse)
colnames(LandUse) <- c("FromRegion","ToRegion","value")
LandUse["indicator"] <- "LandUse"
LandUse <- left_join(LandUse, region_group, by = c("FromRegion" = "Name"))
colnames(LandUse)[5:6] <- c("FromAbbrev", "FromRegionGroup")
LandUse <- left_join(LandUse, region_group, by = c("ToRegion" = "Name"))
colnames(LandUse)[7:8] <- c("ToAbbrev", "ToRegionGroup")

DE <- melt(FP)
colnames(DE) <- c("FromRegion","ToRegion","value")
DE["indicator"] <- "Extraction"
DE <- left_join(DE, region_group, by = c("FromRegion" = "Name"))
colnames(DE)[5:6] <- c("FromAbbrev", "FromRegionGroup")
DE <- left_join(DE, region_group, by = c("ToRegion" = "Name"))
colnames(DE)[7:8] <- c("ToAbbrev", "ToRegionGroup")

FINAL <- rbind(HANPP, LandUse, DE)

write.xlsx(FINAL, file = str_c(path$output,"/IronOre_Footprint_2016_HANPP_LandUse.xlsx" ) )

Plot_Impact_Footprint(FP_data = FP)
