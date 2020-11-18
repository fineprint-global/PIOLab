
Load_IOCodes <- function()
{
  # Construct codes for analysis:
  
  Z_codes <- data.frame("Index" = 1:(num$reg* (num$ind + num$pro) ),
                        "RegionCode" = rep(base$region$Code,each = (num$ind + num$pro) ),
                        "RegionName" = rep(base$region$Name,each = (num$ind + num$pro) ),
                        "EntityCode" = c(rep(1,num$ind),rep(2,num$pro)),
                        "EntityName" = c(rep("Industry", num$ind),rep("Product", num$pro) ),
                        "SectorCode" = c(base$industry$Code,base$product$Code ),
                        "SectorName" = c(base$industry$Name,base$product$Name),
                        "SectorIndex" = NA,
                        stringsAsFactors = FALSE)
  
  for(i in 1:2)
  {
    Z_codes$SectorIndex[Z_codes$EntityCode == i] <- 1:nrow(Z_codes[Z_codes$EntityCode == i,])  
  }
  
  
  Y_codes <- data.frame("index" = 1:num$reg,
                        "RegionCode" = base$region$Code,
                        "RegionName" = base$region$Name,
                        stringsAsFactors = FALSE)
  
  V_codes <- data.frame("index" = 1:7,
                        "Entity" = c( base$input$Name, base$demand$Name[base$demand$Type == "Waste"] ),
                        "Boundary" = c("Input","Input","Input","Input","Input","Output","Output"),
                        stringsAsFactors = FALSE)
  
  data <- list("Z" = Z_codes,
               "Y" = Y_codes,
               "V" = V_codes)
  
  return(data)
}