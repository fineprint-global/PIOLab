###################################
# Repo: PIOLab_Analysis
# Function to read the output of the AISHA run

Load_SUT <- function(type)
{
  
  # Read indices of industries:
  
  index <- list( "ind" = Code$Z$Index[Code$Z$EntityCode == 1],
                 "pro" = Code$Z$Index[Code$Z$EntityCode == 2],
                 "dem" = data.frame("index" = 1:(num$reg*10), "sector" = 1:10) )
  
  extract <- function(element)
  {
    table <- read.csv(file = paste0(path$run,"/Results_BigTvy/",job$date,
                                    "_Mother_AllCountries_",job$phase,"_",element,"-",type,"_",
                                    job$year,"_",job$loop,"_Markup001(full).csv"), header = FALSE)
    return( as.matrix(table) )  
  }
  

  T <- extract("T")  # Read transaction matrix
  
  v <- extract("V")                           # Read primary inputs
  v <- v[, index$ind ]                        # Extract insutries only   
  v <- Agg( v, rep(1:13,num$reg), 1)          # Aggregate regional primary inputs into one block
  v <- v[ 1:num$va, ]                         # Remove empty rows
  
  y <- extract("Y")  # Read final demand
  y <- y[, index$dem$index[ index$dem$sector %in% base$demand$Code]]   # Remove empty columns of demand
  
  colnames(y) <- rep(base$demand$Code,num$reg)
  
  
  # Read waste outputs:
  waste_code <- base$demand[base$demand$Type == "Waste",]
  demand_code <- base$demand[base$demand$Type == "Use",]
  
  w <- y[index$ind, colnames(y) %in% as.character(waste_code$Code) ]
  w <- t ( Agg(w,colnames(w),2) )
  
  # Read clean demand:
  y <- y[index$pro, colnames(y) %in% as.character(demand_code$Code) ]
  
  # Use table (dimensions: product by industry)
  U <- T[ index$pro, index$ind ]
  # Supply table (dimensions: industry by product)
  S <- T[ index$ind, index$pro]
  remove(T)
  
  data <- list("S" = S,"U" = U,"v" = v,"y" = y,"w" = w)  # Export SUTs
  
  return(data)
}

