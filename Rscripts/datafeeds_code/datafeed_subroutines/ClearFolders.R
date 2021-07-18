################################################################################
# This script is executed everytime an data feed is initialized
# The purpose is to clear the respective ALANG and processes data folder 

# Check if ALANG folder exists and create new empty folder for storage
if(dir.exists(path$ALANG)) unlink(path$ALANG,recursive = TRUE) 
dir.create(path$ALANG)

# Check if folder with processed data exists, in case delete and create empty one
if( dir.exists(path$df_Processed) ) unlink(path$df_Processed, recursive = TRUE) 
dir.create(path$df_Processed)
