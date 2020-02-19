Root2Base_ProductAggregator <- function(ProductAggregator)
{
  prod_agg <- t(ProductAggregator)*(1:ncol(ProductAggregator))
  rownames(prod_agg) <- NULL
  prod_agg <- melt(prod_agg)
  prod_agg <- filter(prod_agg,value > 0) %>% select(-value)
  colnames(prod_agg) <- c("base","root")
  
  return(prod_agg)
}
