################################################################################
# This is a subfunction for the data feeds that process the World Steel Association data

CreateALANGforWSAdata <- function(item_id,RSE,Grandchild,datafeed_name)
{
  # Load specific yearbook
  source(paste0(path$Subroutines,"/Load_YearbookWSA.R"))
  # Load function to align data with root classification
  source(paste0(path$Subroutines,"/Read_ProductionWSA.R"))
  # Select Long Rolled Products
  item_page <- items[[item_id]]$page
  # Read values and align with root classification
  data <- Read_ProductionWSA(path,year,item_page,yb,concord)

  # Loading function for estimating SE with linear regression
  source(paste0(path$Subroutines,"/SE_LogRegression.R"))
  data <- SE_LogRegression(data,RSE$small,RSE$large)

  # Create empty ALANG table with header
  source(paste0(path$Subroutines,"/makeALANGheadline.R"))
  # Extend table with additional columns

  for(i in 1:nrow(data))
  { 
    # Get root_code of region 
    reg_num <- data$Code[i]
    reg_name <- as.character(root$region$Name[reg_num])
    reg_num <- as.character(reg_num)
    # Read extraction value
    value <- as.character(data$Quantity[i])
    # Set SE 
    SE <- as.character(data$SE[i])
  
    # Add command 
    ALANG <- add_row(ALANG,'1' = paste(datafeed_name,reg_name),
                   Value = value,'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
  }
  
  # Add other variables
  ALANG$`Column child` <- "2"
  ALANG$`Column grandchild` <- Grandchild$Column
  ALANG$`Row child` <- "1"
  ALANG$`Row grandchild` <- Grandchild$RoW
  ALANG$`#` <- as.character(1:nrow(ALANG))
  ALANG$Incl <- "Y"
  ALANG$Parts <- "1"
  ALANG$`Pre-map` <- ""
  ALANG$`Post-map` <- ""
  ALANG$`Pre-Map` <- ""
  ALANG$`Post-Map` <- ""
  ALANG$Years <- "1"
  ALANG$Margin <- "1"
  ALANG$Coef1 <- "1"

  return(ALANG)
}