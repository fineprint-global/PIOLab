#################################################
#                                               #
# This Feed takes EXIOBASE 3.6 and aggregating  #
#     the database in order to apply the        #
#           MFA-Waste-IO approach               #
#                                               #
#################################################


IEFeed_PIOLab_EXIOWasteMFAIOV2 <- function(year,path)
{
  
  print("IEFeed_PIOLab_EXIOWasteMFAIOV2 initiated.")
  
  
  # Set path tpo functions:
  
  fun <- list(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"),
              paste0(path$Subroutines,"/Agg.R"),
              paste0(path$Subroutines,"/Numbers2File.R"))
  
  lapply(fun,source)  # Load functions
  
  
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  
  # Load source-to-root and calculate Source2Base concordances
  
  Source2Root <- read.csv(paste0(path$Concordance,"/EXIOBASE/Source2Root_Region_EXIOBASE.csv"),
                          header = FALSE)
  
  # Root2Mother <- as.matrix(RegionAggregator)
  
  Source2Root <- as.matrix(Source2Root)
  
 
  
  Source2Root <- Source2Root/rowSums(Source2Root)   # Create map from concordance
  
  # Root2Mother <- as.matrix(Root2Mother)
  
  # Create Source-to-base matrix
  
  Source2Mother <- Source2Root %*% R2M$region
  
  # Create concordance
  
  Source2Mother <- t(Source2Mother)*(1:ncol(Source2Mother))
  
  rownames(Source2Mother) <- NULL
  
  Source2Mother <- melt(Source2Mother)
  
  Source2Mother <- filter(Source2Mother,value > 0) %>% select(-value)
  
  colnames(Source2Mother) <- c("MotherRegion","SourceRegion")
  
  
  # Loading IO codes:
  
  Code <- list("Source" = list("Product" = read.csv(paste0(path$Raw,"/EXIOWasteMFAIO/IO_product_codes.csv")),
                               "Industry" = read.csv(paste0(path$Raw,"/EXIOWasteMFAIO/IO_industry_codes.csv")),
                               "Demand" = read.csv(paste0(path$Raw,"/EXIOWasteMFAIO/IO_finaldemand_codes.csv")) 
                               ) 
               )
               
  
  Code$Source$Product <- left_join(Code$Source$Product,Source2Mother,by = c("RegionCode" = "SourceRegion"))
  
  Code$Source$Industry <- left_join(Code$Source$Industry,Source2Mother,by = c("RegionCode" = "SourceRegion"))
  
  Code$Source$Demand <- left_join(Code$Source$Demand,Source2Mother,by = c("RegionCode" = "SourceRegion"))
  
 
  n_pro <- max(Code$Source$Product$MotherProductCode)  # Number of manufactured products
  
  n_ind <- max(Code$Source$Industry$MotherIndustryCode)  # Number of manufacturing industries
  
  n_reg <- ncol(R2M$region)  # Number of regions
  
  
  # 1. Load EXIOBASE 3.6 mrSUTs
  
  # Supply table (products by industry):
  
  S <- read.csv(paste0(path$Raw,"/EXIOWasteMFAIO/MRSUT_",year,"/supply.csv"),header = FALSE)
  
  S <- as.matrix(S) # Transform into matrix
  
  # Aggregate from source to mother classification:
  
  S <- Agg(S,paste0(Code$Source$Industry$MotherRegion,"-",Code$Source$Industry$MotherIndustryCode),2)
  
  S <- Agg(S,paste0(Code$Source$Product$MotherRegion,"-",Code$Source$Product$MotherProductCode),1)
  
  # Use table (products by industry):
  
  U <- read.csv(paste0(path$Raw,"/EXIOWasteMFAIO/MRSUT_",year,"/use.csv"),header = FALSE)
  
  U <- as.matrix(U)  # Transform into matrix
  
  # Aggregate from source to mother:
  
  U <- Agg(U,paste0(Code$Source$Industry$MotherRegion,"-",Code$Source$Industry$MotherIndustryCode),2)
  
  U <- Agg(U,paste0(Code$Source$Product$MotherRegion,"-",Code$Source$Product$MotherProductCode),1)
  
  
  # Final demand block (product by final demand category)
  
  Y <- read.csv(paste0(path$Raw,"/EXIOWasteMFAIO/MRSUT_",year,"/final_demand.csv"),header = FALSE)
  
  Y <- as.matrix(Y)  # Transform into matrix
  
  # Aggregate from source to mother:
  
  Y <- Agg(Y,paste0(Code$Source$Demand$MotherRegion,"-",Code$Source$Demand$FinalDemandCode),2)
  
  Y <- Agg(Y,paste0(Code$Source$Product$MotherRegion,"-",Code$Source$Product$MotherProductCode),1)
  
  
  x <- colSums(S)  # Gross production of all industry

  q <- rowSums(S)  # Gross production of all products
  
  
  D <- t(S/q)  # Matrix of all commodity output proportions (industry by product)
  
  D[is.na(D)] <- 0  # Set NaN (due to zero gross output) to zero
  
  C <- t(t(S)/x)  # Matrix of all industry output proportions (product by industry)
  
  C[is.na(C)] <- 0  # Set NaN (due to zero gross output) to zero
  
  
  B <- t(t(U)/x) # All commodity by industry coefficient matrix
  
  B[is.na(B)] <- 0   # Set NaN (due to zero gross output) to zero
  
  A <- B %*% D  # Calculate pro-by-pro technology matrix
  
  
  filter <- rep(c(0,rep(1,n_pro)),n_reg)  # Create filter vector
  
  A_f <- diag(filter) %*% A %*% diag(filter)  # Apply filter
  
  I <- diag( rep(1, (n_pro+1) * n_reg ) )  # Create Identity matrix
  
  
  # Filter mrSUTs for steel containing sectors and recalculate market share matrices:
  
  S_f <-  diag(filter) %*% S %*% diag(filter)  # Filter supply table
  
  x_f <- colSums(S_f)  # Filter industry output
  
  q_f <- rowSums(S_f)  # Filter product output
  
  
  D_f <- t(S_f/q_f)  # Matrix of all commodity output proportions (industry by product)
  
  D_f[is.na(D_f)] <- 0  # Set NaN (due to zero gross output) to zero
  
  C_f <- t(t(S_f)/x_f)  # Matrix of all industry output proportions (product by industry)
  
  C_f[is.na(C_f)] <- 0  # Set NaN (due to zero gross output) to zero
  
  
  
  TRM_f <- solve( I - A_f ) %*% C_f  # Calculate pro-by-ind total requirement matrix
  
  Y_f <- D_f %*% diag(filter) %*% Y  # Filter and calculate final demand by industry output
  
  print( sum(TRM_f %*% rowSums(Y_f)) / sum(x) )  # Share of manuf. steel in production
  
  
  Code[["Mother"]] <- list( "Sector" = colnames(A),
                            "Demand" = colnames(Y) )  # Create new codes for WIO-MFA model
  
  
  # Split colname string into region and sector code:
  
  Code$Mother$Sector <- as.data.frame(str_split_fixed(Code$Mother$Sector, "-", 2),stringsAsFactors = FALSE)
  
  Code$Mother$Demand <- as.data.frame(str_split_fixed(Code$Mother$Demand, "-", 2),stringsAsFactors = FALSE)
  
  # Change storage mode to numeric:
  
  Code$Mother$Sector <- as.data.frame(apply(Code$Mother$Sector, c(2),as.numeric))
  
  Code$Mother$Demand <- as.data.frame(apply(Code$Mother$Demand, c(2),as.numeric))
  
  # Add indices:
  
  Code$Mother$Sector["index"] <- 1:nrow(Code$Mother$Sector)
  
  Code$Mother$Demand["index"] <- 1:nrow(Code$Mother$Demand)
  
  
  # Set colnames:
  
  colnames(Code$Mother$Sector) <- c("base","sector","index")
  
  colnames(Code$Mother$Demand) <- c("base","demand","index")
  
  
  # Remove NEC sectors and export demand category (which is always zero):
  
  Code$Mother$Sector <- Code$Mother$Sector[Code$Mother$Sector$sector != 0,] 
  
  Code$Mother$Demand <- Code$Mother$Demand[Code$Mother$Demand$demand != 7,]
  
  
  # Re-order region list:
  
  Code$Mother$Sector <- Code$Mother$Sector[order(Code$Mother$Sector$base),] 
  
  Code$Mother$Demand <- Code$Mother$Demand[order(Code$Mother$Demand$base),] 
  
  
  remove(A,x,q,Y,S,U)  # Remove tables not needed
  
  
  # Filter only WIO-MFA sectors:
  
  A <- A_f[Code$Mother$Sector$index,Code$Mother$Sector$index]
  
  S <- S_f[Code$Mother$Sector$index,Code$Mother$Sector$index]
  
  TRM <- TRM_f[Code$Mother$Sector$index,Code$Mother$Sector$index]
  
  Y <- Y_f[Code$Mother$Sector$index,Code$Mother$Demand$index]
  
  
  # Adapt indices to new model classification:
  
  Code$Mother$Sector$index <- 1:nrow(Code$Mother$Sector)
  
  Code$Mother$Demand$index <- 1:nrow(Code$Mother$Demand)
  
  
  # Check if subfolder in processed data exists and if not create it
  
  path_set <- paste0(path$IE_Processed,"/EXIOWasteMFAIO")
  
  if(!dir.exists(path_set)) dir.create(path_set)
  
  
  # Write codes and tables to folder: 
  
  write.csv(x = Code$Mother$Sector,
            file =  paste0(path$IE_Processed,"/EXIOWasteMFAIO/IO_codes.csv"),
            row.names = FALSE)
  
  write.csv(x = Code$Mother$Demand, 
            file = paste0(path$IE_Processed,"/EXIOWasteMFAIO/Y_codes.csv"),
            row.names = FALSE )
  
  
  Numbers2File(Y,paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Y.csv"))
  
  Numbers2File(A,paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_A.csv"))
  
  Numbers2File(TRM,paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_L.csv"))
  
  
  
  print(paste0( "Share of final demand in total gross production: ", 
                 round( sum(Y)/sum(TRM %*% Y),digits = 2) )
        ) 
  
  
  print("IEFeed_PIOLab_EXIOWasteMFAIOV2 finished.")
  
}
