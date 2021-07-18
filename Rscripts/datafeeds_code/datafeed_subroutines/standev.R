
standev <- function(data, MN, MX)
{
  # Read minimum and maximum for SD regression
  minDATA <- min(data$Quantity)
  maxDATA <- max(data$Quantity)
  
  tmpd <- log10(data$Quantity + 0.1)
  tmpmn <- log10(minDATA)
  tmpmx <- log10(maxDATA)
  
  sd <- MN + (tmpd - tmpmn) * (MX - MN) / (tmpmx - tmpmn)
  
  data[,"SD"] <- sd * data$Quantity
  
  return(data)
}