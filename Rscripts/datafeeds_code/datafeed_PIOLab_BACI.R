################################################################################
################################################################################
#
datafeed_name <- "BACI"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

################################################################################
# Set library path when running on suphys server
if(Sys.info()[1] == "Linux"){
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Define location for root directory
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"}else{
  root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"}
################################################################################

# Initializing R script (load R packages and set paths to folders etc.)

source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)

source(paste0(path$Subroutines,"/Load_BACI.R")) # Loading raw data

# Load function to calculate standard errors 

source(paste0(path$Subroutines,"/SE_LogRegression.R"))

RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)

data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)

data <- select(data,-value)

# Set variables
reg_max <- nrow(root$region)
n_yea <- "1"
n_she <- "1"

# Create empty ALANG table with header

source(paste0(path$Subroutines,"/makeALANGheadline.R"))

reg <- list("From" = list("Name" = root$region$Name[data$From], 
                          "Num" = as.character(data$From)),
            "To" = list("Name" = root$region$Name[data$To],
                        "Num" = as.character(data$To)))

prod <- list("Name" = root$product$Name[data$Product],
             "Num" = as.character(data$Product))

value <- list("Quantity" = as.character(round(data$Quantity,digits = 2)),
              "SE" = as.character(round(data$SE,digits = 2)))

# Set length of ALANG file

ALANG_new <- as.data.frame(matrix(0,nrow = nrow(data),ncol = ncol(ALANG)))

colnames(ALANG_new) <- colnames(ALANG)

ALANG <- ALANG_new

remove(ALANG_new)

# Add commands
ALANG$`1` <- paste0("DataFeed BACI product ",prod$Num," from ",reg$From$Name," to ",reg$To$Name)
ALANG$Value <- value$Quantity
ALANG$S.E. <- value$SE
ALANG$Coef1 <- "1"
ALANG$Incl <- "Y"
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Parts <- "1"
ALANG$Years <- n_yea
ALANG$Margin <- n_she
ALANG$`Row parent` <- reg$From$Num
ALANG$`Row child` <- "2"
ALANG$`Row grandchild` <- prod$Num
ALANG$`Column parent` <- reg$To$Num
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- "1-e"


# Call script that writes the ALANG file to the repsective folder in the root

source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace
