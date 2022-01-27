

IEDataProcessing_PIOLab_ScalingProcessInputs <- function(year,path)
{
  for(r in 1:num$region)
  {
    print( paste("Scaling process inputs of region",r))
    
    # Load domestic tables of region r
    S <- read.csv( paste0(path$IE_Processed,"/SUT/",year,"_Supply_Region",r,".csv"), header = FALSE)
    U <- read.csv( paste0(path$IE_Processed,"/SUT/",year,"_Use_Region",r,".csv"), header = FALSE)
    W <- read.csv( paste0(path$IE_Processed,"/SUT/",year,"_FinalOutput_Region",r,".csv"), header = FALSE)
    P <- read.csv( paste0(path$IE_Processed,"/SUT/",year,"_PrimaryInput_Region",r,".csv"), header = FALSE)
    
    # Create objects to store total inputs and outputs for estimating scaling factor
    Out <- data.frame( "S" = rowSums(S),
                       "W" = rowSums(W[base$process$Code,]) )
    
    In <- as.data.frame( matrix(NA, nrow = num$process, ncol = num$region + 1) )
    colnames(In) <- c( as.character(1:num$region), "P")
    
    In[,r] <- colSums(U)
    In[,num$region+1] <- colSums(P)
    
    IM <- list()
    
    REGimport <- (1:num$region)[-r]
    
    # Loop over trade partners and store values
    for(s in REGimport)
    {
      # Store original import tables in list
      IM[[s]] <- read.csv( paste0(path$IE_Processed,"/SUT/",year,"_IntermediateTrade_",s,"_",r,".csv"), header = FALSE)
      
      In[,s] <- colSums(IM[[s]])  # calculate inputs
    }
    
    # Estimate scaling factor
    Scale <- data.frame("In" = rowSums(In), "Out" = rowSums(Out) ) 
    Scale["factor"] <- Scale$Out / Scale$In
    Scale$factor[is.na(Scale$factor)] <- 1  # Write 1 for no production
    Scale$factor[Scale$factor == Inf] <- 1  # Write 1 for no production
    
    P <- t( t(P) * Scale$factor )  # Scale boundary inputs and store in folder
    Numbers2File( P, paste0(path$IE_Processed,"/SUT/",year,"_PrimaryInput_Region",r,".csv") )
    
    U <- t( t(U) * Scale$factor )  # Scale domestic use table and store in folder
    Numbers2File( U, paste0(path$IE_Processed,"/SUT/",year,"_Use_Region",r,".csv") )
    
    # Scale trade matrices
    for(s in REGimport)
    {
      tmp <- IM[[s]] 
      tmp <- t( t(tmp) * Scale$factor )
      Numbers2File(tmp , paste0(path$IE_Processed,"/SUT/",year,"_IntermediateTrade_",s,"_",r,".csv") )
    }
  }
}