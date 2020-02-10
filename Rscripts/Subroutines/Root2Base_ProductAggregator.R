Root2Base_ProductAggregator <- function(IEdatafeed_name)
{
  # Adjust the following if statements in case to use different separators for Windows and Linux
  #sys <- Sys.info()
  
  # if(sys[1] == "Windows")
  # {
  #   reg_agg <- read.csv(path_agg,stringsAsFactors=FALSE, sep = ",",header = FALSE)  
  # }
  
  # if(sys[1] == "Linux")
  # {
  prod_agg <- read.csv(paste0(path$Concordance,"/Sector Aggregators/",IEdatafeed_name,"_SectorAggregatorProducts.csv"),
                      stringsAsFactors=FALSE, sep = ",",header = FALSE)  
  # }
  
  prod_agg <- t(prod_agg)*(1:ncol(prod_agg))
  rownames(prod_agg) <- NULL
  prod_agg <- melt(prod_agg)
  prod_agg <- filter(prod_agg,value > 0) %>% select(-value)
  colnames(prod_agg) <- c("base","root")
  
  return(prod_agg)
}
