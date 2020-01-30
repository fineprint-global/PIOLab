#################################################
#   This function produces a region concordance # 
#   from the IELab concordance matrix           #
#   that is used to aggregate the raw data      #
#################################################

Root2Base_RegionAggregator <- function(IEdatafeed_name)
{
  # Adjust the following if statements in case to use different separators for Windows and Linux
  sys <- Sys.info()
  
  # if(sys[1] == "Windows")
  # {
  #   reg_agg <- read.csv(path_agg,stringsAsFactors=FALSE, sep = ",",header = FALSE)  
  # }
  
  # if(sys[1] == "Linux")
  # {
  reg_agg <- read.csv(paste0(path$Concordance,"/Region Aggregators/",IEdatafeed_name,"_RegionAggregator.csv"),
                      stringsAsFactors=FALSE, sep = ",",header = FALSE)  
  # }
  
  reg_agg <- t(reg_agg)*(1:ncol(reg_agg))
  rownames(reg_agg) <- NULL
  reg_agg <- melt(reg_agg)
  reg_agg <- filter(reg_agg,value > 0) %>% select(-value)
  colnames(reg_agg) <- c("base","root")
  
  return(reg_agg)
}
