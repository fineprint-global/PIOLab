
# Set up wrapper for adding rows to ALANG

NewALANG <- function(name,SE,ALANG)
{
  file <- paste0("S8 ",path$mother,"Data/IE/",gsub("-","",Sys.Date()),
                 "_PIOLab_AllCountriesS8File_",name,year,".csv")
  
  ALANG <- add_row(ALANG,'1' = name,Coef1 = file, S.E. = SE,
                   Value = "I",Incl = "Y",Parts = "1",'Row parent' = "",'Row child' = "",
                   'Row grandchild' = "",'Column parent' = "",'Column child' = "",
                   'Column grandchild' = "",Years = "",Margin = "",'Pre-map' = "",'Post-map' = "",
                   'Pre-Map' = "",'Post-Map' = "")
  
  return(ALANG)
}
# Set up functions for adding rows with wrapper to ALANG
AddRowALANG_CN <- function(name, ALANG)
{
  RSE_sel <- RSE[RSE$item == name,]
  ALANG <- NewALANG(name,paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";CN",RSE_sel$CN,";"),
                    ALANG)
  
  return(ALANG)
}

AddRowALANG <- function(name, ALANG)
{
  RSE_sel <- RSE[RSE$item == name,]
  ALANG <- NewALANG(name,
                    paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";"),
                    ALANG)
  
  return(ALANG)
}
