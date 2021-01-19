# This function calculates DMC and other ewMFA indicators from the IOT


Calc_ewMFA <- function(IOT,Code)
{
  
  # If the IOT is industry by industry, select industry codes:
  
  if(nrow(IOT$Z) == ncol(IOT$Z) & nrow(IOT$Z) == ( num$ind * num$reg)) sec_sel <- "Industry"
  
  # If the IOT is product by product, select product codes:
  
  if(nrow(IOT$Z) == ncol(IOT$Z) & nrow(IOT$Z) == ( num$pro * num$reg)) sec_sel <- "Product"
  
  
  Code_sel <- Code$Z$RegionCode[Code$Z$EntityName == sec_sel]  # Select codes
  
  # Read tables:
  
  y <- IOT$y  
  
  Z <- IOT$Z
  
  e <- IOT$e
  
  DE <- Agg( e[,base$input$Code[base$input$Name == "Crude Ore"]], Code_sel, 1)  # Read extraction
  
  # Direct trade flows:
  
  y <- Agg( y, aggkey = Code_sel , dim = 1 )
  
  y <- Agg( y, aggkey = rep( Code$Y$index, each = 6), 2 )
  
  
  Z <- Agg(x = Z, aggkey = Code_sel,1)
  
  Z <- Agg(x = Z, aggkey = Code_sel,2)
  
  Direct <- Z + y
  
  data <- data.frame("index" = unique(Code_sel),
                     "DE" = DE[1:nrow(base$region),1] )
  
  data["Import"] <- colSums(Direct) - diag(Direct)
  
  data["Export"] <- rowSums(Direct) - diag(Direct)
  
  data["DMC"] <- data$DE + data$Import - data$Export
  
  return(data)
  
}

  
  