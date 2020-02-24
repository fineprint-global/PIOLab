#################################################
#                                               #
# This Feed takes EXIOBASE 3.6 and aggregating  #
#     the database in order to apply the        #
#           MFA-Waste-IO approach               #
#                                               #
#################################################


IEFeed_PIOLab_EXIOWasteMFAIO <- function(year,path)
{
  print("IEFeed_PIOLab_EXIOWasteMFAIO initiated.")
  # Function for aggregating the IOT
  Agg <- function(x)
  {
    x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
    return(x)
  }
  
  # 1. Loading the original EXIOBASE 3.6 variables
  # Transaction matrix:
  load(paste0(path$Raw,"/EXIOWasteMFAIO/",year,"_Z.RData"))
  # Final demand:
  load(paste0(path$Raw,"/EXIOWasteMFAIO/",year,"_Y.RData"))
  # IO sector codes:
  load(paste0(path$Raw,"/EXIOWasteMFAIO/","IO.codes.RData")) 
  IO.codes <- IO.codes %>% select(Index,Product.Name,Country.Code)
  regions <- unique(IO.codes$Country.Code)
  # Final demand codes
  load(paste0(path$Raw,"/EXIOWasteMFAIO/","Y.codes.RData"))
  
  # Load the root classifciation
  root_class <- root$region %>% select(Code,ISO2digitCode)
  
  # Load region aggregator
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  
  # Load source-to-root and calculate Source2Base concordances
  Source2Root <- read.csv(paste0(path$Concordance,"/EXIOBASE/Source2Root_Region_EXIOBASE.csv"),
                          header = FALSE)
  
  Root2Base <- as.matrix(RegionAggregator)
    
  Source2Root <- as.matrix(Source2Root)
  # Create map from concordance
  Source2Root <- Source2Root/rowSums(Source2Root)
  Root2Base <- as.matrix(Root2Base)
  # Create Source-to-base matrix
  Source2Base <- Source2Root %*% Root2Base
  
  # Create concordance
  Source2Base <- t(Source2Base)*(1:ncol(Source2Base))
  rownames(Source2Base) <- NULL
  Source2Base <- melt(Source2Base)
  Source2Base <- filter(Source2Base,value > 0) %>% select(-value)
  colnames(Source2Base) <- c("base","root")
  
  IO.codes <- data.frame(IO.codes,"NewProducts" = "NEC",stringsAsFactors = FALSE)
  IO.codes["base"] <- rep(Source2Base$base,each = 200)
  
  # select steel containing commodities
  steel.id <- c(117:125,150)
  IO.codes$NewProducts[IO.codes$Product.Name %in% IO.codes$Product.Name[steel.id]] <- IO.codes$Product.Name[IO.codes$Product.Name %in% IO.codes$Product.Name[steel.id]] 

  # Create key for aggregation
  IO.codes <- data.frame(IO.codes,
                         "Key" = paste0(as.character(IO.codes$base),"§",IO.codes$NewProducts),
                         stringsAsFactors = FALSE)
  
  # Aggregate transaction matrix
  colnames(Z) <- IO.codes$Key
  Z <- Agg(Z)
  Z <- t(Z)
  colnames(Z) <- IO.codes$Key
  Z <- Agg(Z)
  Z <- t(Z)

  # Aggregate final demand
  Y.codes["base"] <- rep(Source2Base$base,each = 7) 
  colnames(Y) <- Y.codes$base
  Y <- Agg(Y)
  Y <- t(Y)
  colnames(Y) <- IO.codes$Key
  Y <- Agg(Y)
  Y <- t(Y)
  # Rearrange columns so that the region code of the base table is in increasing order 
  Y <- Y[,as.character(1:max(Source2Base$base))]
  
  # Create new IO.codes to rearrange the variables so that base region codes increases
  IO.codes <- colnames(Z)
  IO.codes <- as.data.frame(str_split_fixed(IO.codes, "§", 2),stringsAsFactors = FALSE)
  colnames(IO.codes) <- c("base","commodity")
  storage.mode(IO.codes$base) <- "numeric"
  IO.codes <- IO.codes[order(IO.codes$base),]
  IO.codes$index <- 1:nrow(IO.codes)
  IO.codes$Key <- paste0(as.character(IO.codes$base),"§",IO.codes$commodity)
  IO.codes <- IO.codes[,c("index","base","commodity","Key")]
  
  # New Y.codes 
  Y.codes <- data.frame("base" = 1:max(Source2Base$base),
                        "Region.Name" = base$region$Name, 
                        stringsAsFactors = FALSE)
  
  # Rearrange Z & Y (if necessary)
  Z <- Z[,IO.codes$Key]
  Z <- t(Z)
  Z <- Z[,IO.codes$Key]
  Y <- t(Y)
  Y <- Y[,IO.codes$Key]
  Y <- t(Y)

  # Compute new variables of the aggreagted table
  x <- rowSums(Z) + rowSums(Y)
  A <- t(t(Z)/x)
  A[is.na(A)] <- 0
  
  # Delete input/flows not containing steel
  A[IO.codes$index[IO.codes$commodity == "NEC"],] <- 0
  A[,IO.codes$index[IO.codes$commodity == "NEC"]] <- 0
  L <- solve(diag(nrow(A))-A)
  
  # Construct new final demand (now only steel-containing products) 
  Y[IO.codes$index[IO.codes$commodity == "NEC"],] <- 0

  # Remove NEC product from WIO model
  L <- L[IO.codes$index[IO.codes$commodity != "NEC"],IO.codes$index[IO.codes$commodity != "NEC"]]
  Y <- Y[IO.codes$index[IO.codes$commodity != "NEC"],]
  x <- rowSums(L%*%Y)
  IO.codes <- IO.codes[IO.codes$commodity != "NEC",]
  IO.codes$index <- 1:nrow(IO.codes)
  
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$IE_Processed,"/EXIOWasteMFAIO")
  if(!dir.exists(path_set)) dir.create(path_set)
  
  write.table(L,file = paste0(path_set,"/",year,"_L.csv"),row.names = FALSE,col.names = FALSE,sep = ",")
  write.table(Y,file = paste0(path_set,"/",year,"_Y.csv"),row.names = FALSE,col.names = FALSE,sep = ",")
  write.table(x,file = paste0(path_set,"/",year,"_x.csv"),row.names = FALSE,col.names = FALSE,sep = ",")
  save(IO.codes,file = paste0(path_set,"/IO.codes.RData"))
  save(Y.codes,file = paste0(path_set,"/Y.codes.RData"))
  
  print("IEFeed_PIOLab_EXIOWasteMFAIO finished.")
  
}