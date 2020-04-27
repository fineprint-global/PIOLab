################################################################################
# Load root to mother sector aggregators


if(IEdatafeed_name == "Ind30Pro39v1")
{
  
  set <- read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"),sheet = 2)
  
  path_sel <- list("flow" = paste0(path$Concordance,"/Sector Aggregators/",
                                   set$date[set$aggregator == "sector"],"_",num$flow,"Products_SectorAggregator.csv"),
                   "process" = paste0(path$Concordance,"/Sector Aggregators/",
                                      set$date[set$aggregator == "sector"],"_",num$process,"Industries_SectorAggregator.csv")
                   )
  
  
  R2M[["flow"]] <- as.matrix( read.csv(path_sel$flow,stringsAsFactors=FALSE, sep = ",",header = FALSE) )
  
  R2M[["process"]] <- as.matrix( read.csv(path_sel$process, stringsAsFactors=FALSE, sep = ",",header = FALSE) )
    
  remove(set,path_sel)
}

