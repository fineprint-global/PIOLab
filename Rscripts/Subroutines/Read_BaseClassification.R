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
  R2M <- list("region" = as.matrix( read.csv(reg_map,stringsAsFactors=FALSE, sep = ",",header = FALSE) ) )  
  
  # Read the number of regions from the name of the aggregator 
  RegionAgg <- substr(reg_map,nchar(reg_map)-23,nchar(reg_map)-21)
  
  base <- list("region" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",RegionAgg,"_BaseRegionClassification.xlsx"),sheet = 1),
               "process" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 1),
               "flow" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 2),
               "demand" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 3),
               "input" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 4))
  
  remove(reg_map,RegionAgg)
  
} else
{
  # In cases when the code is not executed on the server and no specific region aggregation is 
  # given in the initial estimate, set it to 5 (the smallest reg classification at the moment)
  
  if(!exists("test_regagg")) test_regagg <- "005"
  
  # Import matrix
  R2M <<- list("region" = as.matrix( read.csv(paste0(path$Concordance,"/Region Aggregators/",test_regagg,"_RegionAggregator.csv"),
                                              stringsAsFactors=FALSE, sep = ",",header = FALSE)
                                    )
              )
  
  base <<- list("region" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",test_regagg,"_BaseRegionClassification.xlsx"),sheet = 1),
                "process" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 1),
                "flow" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 2),
                "demand" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 3),
                "input" = read.xlsx(paste0(path$Concordance,"/LabelsAndCodes/",IEdatafeed_name,"_BaseSectorClassification.xlsx"),sheet = 4))
  
  remove(test_regagg)
}

num <<- list("flow" = nrow(base$flow),
             "process" = nrow(base$process),
             "region" = nrow(base$region),
             "input" = nrow(base$input),
             "demand" = nrow(base$demand) 
             )

remove(reg_path)
