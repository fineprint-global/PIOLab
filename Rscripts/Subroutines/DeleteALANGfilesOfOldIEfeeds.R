# This subroutine checks if old initial estimate ALANG files exists and deltes them
#
#
# Read content of ALANGfiles folder
AvailableALANGfiles <- data.frame("content" = list.files(path$ALANG, full.names=TRUE),
                                  stringsAsFactors = FALSE)

# Check if file name containts InitialEstimate
IEfiles_math <- apply(AvailableALANGfiles,c(1),FUN=function(x){grep("InitialEstimate", x)})

if(length(IEfiles_math) >= 1)
{
  for(i in 1:length(IEfiles_math))
  {
    filename <- AvailableALANGfiles$content[i]
    
    if(!is.na(IEfiles_math[[i]][1]))
    {
      # Delete file
      unlink(filename)
      print(paste("Deleting old ALANGfiles",filename))  
    }
  }
  remove(i, filename)
}
  

# Removing objects
remove(IEfiles_math,AvailableALANGfiles)

