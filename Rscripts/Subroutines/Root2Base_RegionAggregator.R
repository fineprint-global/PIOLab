#################################################
#   This function produces a region concordance # 
#   from the IELab concordance matrix           #
#   that is used to aggregate the raw data      #
#################################################

Root2Base_RegionAggregator <- function(RegionAggregator)
{
  reg_agg <- t(RegionAggregator)*(1:ncol(RegionAggregator))
  rownames(reg_agg) <- NULL
  reg_agg <- melt(reg_agg)
  reg_agg <- filter(reg_agg,value > 0) %>% select(-value)
  colnames(reg_agg) <- c("base","root")
  
  return(reg_agg)
}
