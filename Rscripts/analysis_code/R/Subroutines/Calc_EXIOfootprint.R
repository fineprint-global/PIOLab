

Calc_EXIOfootprint <- function()
{
  # 1. Load EXIOBASE 3.6 mrSUTs
  
  # Load codes:

  Code <- list("Product" = read.csv(paste0(path$raw,"/EXIOWasteMFAIO/IO_product_codes.csv")),
               "Industry" = read.csv(paste0(path$raw,"/EXIOWasteMFAIO/IO_industry_codes.csv")),
               "Demand" = read.csv(paste0(path$raw,"/EXIOWasteMFAIO/IO_finaldemand_codes.csv")) 
               ) 
              
  # Load extension data:
  
  DE <- read.csv(paste0(path$processed,"/IRPextraction/IRPextraction_RHS_",job$year,".csv"),header = FALSE) 
  DE <- as.matrix(DE)
  
  # Load root to mother concordance:
  
  R2M <- read.csv( paste0( path$concordance,"/Region Aggregators/049_RegionAggregator.csv"), header = FALSE )
  R2M <- as.matrix(R2M)  # Transform into matrix
  
  DE <- data.frame("Code" = 1:49,
                   "value" = colSums( t(DE) %*% R2M ) )  # EXIOBASE region classification and codes
  
  ex_iron <- 33  # Product code of iron ore in EXIOBASE
  
  DE["key"] <- paste0(DE$Code,"-",ex_iron)
  
  Code$Product["key"] <- paste0( Code$Product$RegionCode,"-",Code$Product$ProductCode )
  
  e <- vector( mode = "numeric",length = nrow(Code$Product) )  # Create vector as extension
  
  e[Code$Product$Index[ Code$Product$key %in% DE$key ]] <- DE$value 

  
  # Supply table (products by industry):
  
  S <- read.csv(paste0(path$raw,"/EXIOWasteMFAIO/MRSUT_",job$year,"/supply.csv"),header = FALSE)
  S <- as.matrix(S) # Transform into matrix
  
  # Use table (products by industry):
  
  U <- read.csv(paste0(path$raw,"/EXIOWasteMFAIO/MRSUT_",job$year,"/use.csv"),header = FALSE)
  U <- as.matrix(U)  # Transform into matrix
  
  
  # Final demand block (product by final demand category)
  
  Y <- read.csv(paste0(path$raw,"/EXIOWasteMFAIO/MRSUT_",job$year,"/final_demand.csv"),header = FALSE)
  Y <- as.matrix(Y)  # Transform into matrix
  
  
  x <- colSums(S)                 # Gross production of all industry
  q <- rowSums(S)                 # Gross production of all products
  
  D <- t(S/q)                     # Matrix of all commodity output proportions (industry by product)
  D[is.na(D)] <- 0                # Set NaN (due to zero gross output) to zero
  
  # C <- t(t(S)/x)  # Matrix of all industry output proportions (product by industry)
  # C[is.na(C)] <- 0  # Set NaN (due to zero gross output) to zero
  
  
  B <- t(t(U)/x)                  # All commodity by industry coefficient matrix
  B[is.na(B)] <- 0                # Set NaN (due to zero gross output) to zero
  
  A <- B %*% D                    # Calculate pro-by-pro technology matrix
  I <- diag( rep( 1,nrow(A) ) )   # Create identity matrix
  L <- solve(I - A)               # Create inverse
  
  intens <- e/q                   # Direct intensities
  intens[is.na(intens)] <- 0
  
  MP <- L * intens                # Total Requirement Multipliers
  
  FP <- MP %*% Y      # Calculate footprints
  
  FP <- Agg(FP,Code$Product$RegionCode,1)  # Aggregate products/rows
  FP <- Agg(FP,Code$Demand$RegionCode,2)  # Aggregate demand/columns
  
  if(num$reg != 49)
  {
    # Load mother-2-mother (M2M) aggregator (49 to 40 regions)
    M2M <- read.xlsx( paste0(path$concordance,'/Region Aggregators/RegionAggregator_49_to_',num$reg,'.xlsx') )
    M2M <- as.matrix( M2M[,5:44] )
    
    # Aggregate to actual mother classification
    FP <- t(M2M) %*% FP %*% M2M
  }
  
  return(FP)
}
  
  
  