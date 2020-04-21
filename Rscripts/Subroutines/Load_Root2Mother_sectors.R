################################################################################
# Load root to mother sector aggregators



if(IEdatafeed_name == "Ind20Pro22v1")
{
  ProductAggregator <- read.csv(paste0(path$Concordance,"/Sector Aggregators/22ProV1_SectorAggregatorProducts.csv"),
                                stringsAsFactors=FALSE, sep = ",",header = FALSE)
  
  IndustryAggregator <- read.csv(paste0(path$Concordance,"/Sector Aggregators/20IndV1_SectorAggregatorIndustries.csv"),
                                 stringsAsFactors=FALSE, sep = ",",header = FALSE)
}

if(IEdatafeed_name == "Ind30Pro40v1")
{
  
  ProductAggregator <- read.csv(paste0(path$Concordance,"/Sector Aggregators/40ProV1_SectorAggregatorProducts.csv"),
                                stringsAsFactors=FALSE, sep = ",",header = FALSE)
  
  IndustryAggregator <- read.csv(paste0(path$Concordance,"/Sector Aggregators/30IndV1_SectorAggregatorIndustries.csv"),
                                 stringsAsFactors=FALSE, sep = ",",header = FALSE)
}

if(IEdatafeed_name == "Ind30Pro39v1")
{
  
  set <- read.xlsx(xlsxFile = paste0(path$Settings,"/datafeeds_settings/IE_settings.xlsx"),sheet = 2)
  
  path_sel <- list("flow" = paste0(path$Concordance,"/Sector Aggregators/",
                                   set$date[set$aggregator == "product"],"_",num$flow,"Products_SectorAggregator.csv"),
                   "process" = paste0(path$Concordance,"/Sector Aggregators/",
                                      set$date[set$aggregator == "industry"],"_",num$process,"Industries_SectorAggregator.csv")
                   )
  
  
  R2M[["flow"]] <- as.matrix( read.csv(path_sel$flow,stringsAsFactors=FALSE, sep = ",",header = FALSE) )
  
  R2M[["process"]] <- as.matrix( read.csv(path_sel$process, stringsAsFactors=FALSE, sep = ",",header = FALSE) )
    
  remove(set,path_sel)
}

