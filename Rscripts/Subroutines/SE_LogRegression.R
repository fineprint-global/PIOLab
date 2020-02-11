################################################################################
# 
# This function estimates the standard errors of a given raw data set by linearly regressing
# the relative standard error dx of the smallest data point x to fit the relative standard error 
# dy of the largest data point y. See AISHA manual or Lenzen et al 2010 
# (UNCERTAINTY ANALYSIS FOR MULTI-REGION INPUT–OUTPUT MODELS)
#
# Input variables
# data: raw data in root classification where column one shows the root region code and column two the value
# a: Relative standard error of smallest value
# b: Relative standard error of largest value

SE_LogRegression <- function(data,a,b)
{
  data["SE"] <- 0   # Add empty column for standard errors
  
  # Select minimum and maximum
  mini <- min(data$Quantity)
  maxi <- max(data$Quantity)
  
  sel <- data.frame("Quantity" = c(mini,maxi),
                    "RSE" = c(a,b))
  
  m <- lm(log(RSE) ~ Quantity, data=sel)  # build log-transformed linear regression 
  funct <- function(x) {exp(coef(m)[1])*exp(coef(m)[2]*x)}
  
  # caluclate SE for all numbers and write into data frame
  data$SE <- data$Quantity * funct(data$Quantity)
  
  return(data)  
}


