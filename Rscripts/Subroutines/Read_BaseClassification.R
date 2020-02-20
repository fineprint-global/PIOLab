################################################################################
# This script reads .mat files (if available) to determine the base classifcations

reg_path <- paste0(path$Processed,"/RegionAggFile4R.mat")

if(file.exists(reg_path))
{
  # Read selected aggregators
  reg_map <- readMat(reg_path)  # for regions
  reg_map <- c(reg_map$out)
  unlink(reg_path)  # Delete the file
  
  # Import matrix
  RegionAggregator <- read.csv(reg_map,stringsAsFactors=FALSE, sep = ",",header = FALSE)  
  
  # Read the number of regions from the name of the aggregator 
  RegionAgg <- substr(reg_map,nchar(reg_map)-23,nchar(reg_map)-21)
  
  base <- list("region" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",RegionAgg,"_BaseRegionClassification.xlsx"),sheet = 1),
               "industry" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 1),
               "product" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 2))
  
  remove(reg_map,RegionAgg)
  
} else
{
  # In cases when the code is not executed on the server and no specific region aggregation is 
  # given in the initial estimate, set it to 5 (the smallest reg classification at the moment)
  
  if(!exists("test_regagg")) test_regagg <- 5
  
  # Import matrix
  RegionAggregator <- read.csv(paste0(path$Concordance,"/Region Aggregators/00",test_regagg,"_RegionAggregator.csv"),
                      stringsAsFactors=FALSE, sep = ",",header = FALSE)  
  
  base <- list("region" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/00",test_regagg,"_BaseRegionClassification.xlsx"),sheet = 1),
               "industry" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 1),
               "product" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 2))
  remove(test_regagg)
}

# Import product aggregator for different cases
if(IEdatafeed_name == "Ind20Pro22v1")
{
  ProductAggregator <- read.csv(paste0(path$Concordance,"/Sector Aggregators/22ProV1_SectorAggregatorProducts.csv"),
                                stringsAsFactors=FALSE, sep = ",",header = FALSE)
}

remove(reg_path)
