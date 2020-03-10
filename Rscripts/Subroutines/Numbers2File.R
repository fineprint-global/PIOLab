# Function/Wrapper for writing clean data (without labels or row/col names)

Numbers2File <- function(d,t)
{
  write.table(d,file = t,row.names = FALSE,col.names = FALSE,sep = ",",na = "NaN")
}
