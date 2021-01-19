# Function/Wrapper for writing clean data (without labels or row/col names)

Numbers2File <- function(table,filename)
{
  write.table(table,file = filename,row.names = FALSE,col.names = FALSE,sep = ",",na = "NaN")
}
