################################################################################
# Write ALANG file to folder
# 
# This section is executed everytime an data feed is executed.
# It stores the ALANG files to the respective

# Write data frame with ALANG commands as tab-delimited txt-file to root and working directory (mother)
# Note HP: This is probably not the normal procedure, meaning no IE ALANG's in the root
filename <-  paste0(path_set,"/",gsub("-","",Sys.Date()),
                    "_PIOLab_",datafeed_name,"_000_Constraints-",year,"_000_RoWincluded.txt")
print(filename)  

write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t")
