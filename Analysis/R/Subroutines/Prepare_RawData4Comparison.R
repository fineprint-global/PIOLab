################################################################################
# This script prepares raw data for comparison with the final table


# test_regagg <- "032"
github <- "C:/Users/hwieland/Github workspace"
path$input <- paste0(github,"/PIOLab/Analysis/input/",test_regagg,"/")
              

for(year in 2008:2017) IEFeed_PIOLab_WSA(year,path)

for(year in 2008:2017) IEFeed_PIOLab_BACI(year,path)

for(year in 2008:2014) IEFeed_PIOLab_IRP(year,path)


# Read RHS of data feeds for IRP trade (import/export) and transform into base classification

type <- c("imports","exports")

# Load aggregator
R2M <- read.csv( paste0(path$Concordance,"/Region Aggregators/",test_regagg,"_RegionAggregator.csv"), header = FALSE )
R2M <- as.matrix(R2M)

for(t in type)
{
  for(year in 2008:2014)
  {
    tmp <- read.csv( paste0( path$Processed,"/IRP",t,"/IRP",t,"_RHS_",year,".csv" ), header = FALSE )    
    tmp <- as.matrix(tmp)
    tmp <- t( t(tmp) %*% R2M )
    
    write.table(tmp, paste0(path$input,"UNEP-IRP/",t,"_",year,".csv"), row.names = FALSE, col.names = FALSE, sep = ",")
  }
}



# Load aggregator
R2M_49 <- read.csv( paste0(path$Concordance,"/Region Aggregators/049_RegionAggregator.csv"), header = FALSE )
R2M_49 <- as.matrix(R2M_49)

# Prepare materials extension for the EXIOBASE MRIO

for(year in 1995:2014)
{
  source(paste0(path$Subroutines,"/Read_ExtractionIRP.R"))
  
  q <- matrix(0, nrow = nrow(root$region), ncol = 1)
  q[data$Code] <- data$Quantity
  
  q <- t( t(q) %*% R2M_49 )
  
  Numbers2File(q, paste0(path$root,"Analysis/input/EXIOBASE/EXIOBASE_CrudeOre_Extension_",year,".csv") )
}



