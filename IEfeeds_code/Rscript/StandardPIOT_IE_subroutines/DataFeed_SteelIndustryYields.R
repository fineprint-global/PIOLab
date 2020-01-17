#################################################
#                                               #  
# This data feed imports different information  #
# from the WSA report "Yield improvements in    #
#             the steel industry                #
#                                               #
#################################################


DataFeed_SteelIndustryYields <- function(path)
{
  print("DataFeed_SteelIndustryYields initiated.")
  # Import war data
  data <- read.xlsx(paste0(path$Raw,"/WSA/Yields in the steel industry.xlsx"),sheet = 1) %>%
    select(Process,Average,Maximum,Minimum)
  
  # Write to folder 
  write.csv(data,file = paste0(path$Processed,"/WSA/SteelIndustryYields.csv"),row.names = FALSE)
}