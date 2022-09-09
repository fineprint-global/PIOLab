
R2B <- read.csv( str_c( path$root, "/ConcordanceLibrary/Region Aggregators/032_RegionAggregator.csv"), header = FALSE )
R2B <- as.matrix(R2B)

# Loading distance metrics (DPA_raw)
load(file = str_c("./Analysis/input/EPIP/Distance_protected_areas_",job$year,".R") )
DPA_raw <- DPA_raw[[3]]
rownames(DPA_raw) <- DPA_raw$ISO3
DPA_raw[,1] <- NULL

# Load SNL production data 
Iron_extraction <- read.csv(file = str_c("./Analysis/input/EPIP/Iron Ore_",job$year,"_in_roots.csv") ) %>% 
  select(GID_id, value)
colnames(Iron_extraction)[2] <- "extraction"

# Write EPIP into root region format and aggregate to base classification
E <- data.frame( matrix(0,nrow = 221, ncol = 5) )
# colnames(E) <- c("extraction", "AREA", "HANPP")

index <- match(rownames(DPA_raw), root$region$RootCountryAbbreviation)

E[index,] <- DPA_raw
E <- as.matrix(E)
E <- t( t(E) %*% R2B )
E_total <- colSums(E)/sum(E)
E <- E / rowSums(E)
E <- t(E)
E[,is.na(colSums(E))] <- E_total


## Calculate footprints
x <- rowSums(IOT$L %*% IOT$y)  # Estimate new gross production
q <- IOT$e[, base$input$Code[base$input$Name == "Crude Ore"] ]/x

MP <- IOT$L * q  # Material multipliers

# First for all countries
FP <- MP %*% IOT$y  # Material footprints
FP <- Agg(FP, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
FP <- Agg(FP, Code$Z %>% filter(EntityName == "Industry") %>% pull(RegionCode), 1)
colnames(FP) <- base$region$Name
rownames(FP) <- base$region$Name

FP <- E %*% FP

write.xlsx(FP, file = str_c(path$output,"/FP_2016_DistanceProtectedAreas.xlsx" ) )



