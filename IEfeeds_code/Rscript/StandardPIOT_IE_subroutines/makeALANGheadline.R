
makeALANGheadline <- function()
{
  ALANG <- data.frame(as.character(),character(),character(),character(),character(),
                      character(),character(),character(),character(),character(),
                      character(),character(),character(),character(),character(),
                      character(),character(),character(),character(),stringsAsFactors = FALSE)

  colnames(ALANG) <-c("1","Incl","#","Parts","Value","Pre-map","Post-map","S.E.","Pre-Map","Post-Map",
                      "Coef1","Years","Margin","Row parent","Row child","Row grandchild","Column parent","Column child",
                      "Column grandchild")
  
  return(ALANG)
}