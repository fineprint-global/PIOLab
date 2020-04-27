################################################################################
# This code compares production and trade values of all industries/products
# hanspeter.wieland@wu.ac.at
# April 2020

filename <-  paste0(path$IE_Processed,"/SUT/",gsub("-","",Sys.Date()),
                    "_PIOLab_AllCountriesS8File_AllInOne",year,".csv")

S8 <- read.csv(file = filename,header = FALSE)  

colnames(S8) <- c("x1","x2","y1","y2","y3","z1","z2","z3","t")

Prod <- filter( S8, y1 == z1, y2 == 1, z2 == 2) %>% group_by(z1,z3) %>% 
  summarise(t = sum(t)) %>% ungroup(z3) %>% select(z1,z3,t)

Trade <- filter( S8, y1 != z1, y2 == 2, z2 == 1) %>% group_by(y1,y3) %>% 
  summarise(t = sum(t)) %>%  ungroup(y3) %>% select(y1,y3,t)

df <- left_join(Prod,Trade, by = c("z1" = "y1","z3" = "y3"), suffix = c(".prod",".trade"))

df[,c("t.prod","t.trade")] <- df[,c("t.prod","t.trade")] / 10^3 

df[is.na(df)] <- 0

df["dom"] <- df$t.prod - df$t.trade
df["product"] <- base$product$Name[df$z3]
df["region"] <- base$region$Name[df$z1]


?left_join




