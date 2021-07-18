######################################################################
#Create variables for 73 HS Codes
######################################################################
a1 <- c(73.01, 
       7301.10, 
       7301.20)
a2 <- c(73.02, 
7302.10,
7302.30,
7302.40,
7302.90)
a3 <- c(73.03, 
7303.00)

a4 <- c(73.04, 
       7304.11,
       7304.19,
       7304.22,
       7304.23,
       7304.24,
       7304.29,
       7304.31,
       7304.39,
       7304.41,
       7304.49,
       7304.51,
       7304.59,
       7304.90)
a5 <- c(73.05,
       7305.11,
       7305.12,
       7305.19,
       7305.20,
       7305.31,
       7305.39,
       7305.90)
a6 <- c(73.06,
       7306.11,
       7306.19,
       7306.21,
       7306.29,
       7306.30,
       7306.40,
       7306.50,
       7306.61,
       7306.69,
       7306.90)
a7 <- c(73.07,
       7307.11,
       7307.19,
       7307.21,
       7307.22,
       7307.23,
       7307.29,
       7307.91,
       7307.92,
       7307.93,
       7307.99)
a8 <- c(73.08,
       7308.10,
       7308.20,
       7308.30,
       7308.40,
       7308.90)
a9 <- c(73.09 ,
       7309.00)
a10 <- c(73.10,
       7310.10,
       7310.21,
       7310.29)
a11 <- c(73.11,
       7311.00)
a12 <- c(73.12,
       7312.10,
       7312.90)
a13 <- c(73.13 ,
       7313.00)
a14 <- c(73.14,
       7314.12,
       7314.14,
       7314.19,
       7314.20,
       7314.31,
       7314.39,
       7314.41,
       7314.42,
       7314.49,
       7314.50)
a15 <- c(73.15 ,
       7315.11,
       7315.12,
       7315.19,
       7315.20,
       7315.81,
       7315.82,
       7315.89,
       7315.90)
a16 <- c(73.16,
       7316.00)
a17 <- c(73.17,
       7317.00)
a18 <- c(73.18,
       7318.11,
       7318.12,
       7318.13,
       7318.14,
       7318.15,
       7318.16,
       7318.19,
       7318.21,
       7318.22,
       7318.23,
       7318.24,
       7318.29)
a19 <- c(73.19,
       7319.40,
       7319.90)
a20 <- c(73.20,
       7320.10,
       7320.20,
       7320.90)
a21 <- c(73.21,
       7321.11,
       7321.12,
       7321.19,
       7321.81,
       7321.82,
       7321.89,
       7321.90)
a22 <- c(73.22,
       7322.11,
       7322.19,
       7322.90)
a23 <- c(73.23,
       7323.10,
       7323.91,
       7323.92,
       7323.93,
       7323.94,
       7323.99)
a24 <- c(73.24,
       7324.10,
       7324.21,
       7324.29,
       7324.90)
a25 <- c(73.25,
       7325.10,
       7325.91,
       7325.99)
a26 <- c(73.26,
       7326.11,
       7326.19,
       7326.20,
       7326.90)

products_73 <- c()
for (i in 1:26){
  assign(paste0("products_",get(paste0("a",i))[1]),as.character(get(paste0("a",i))[-1]))
  products_73 <- c(products_73,as.character(get(paste0("a",i))[1]))
}
######################################################################
#Choose relevant columns
######################################################################
columns <- c(1,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
products_class <- c("26","10","12","73")
products_26 <- c("26.01",
                 "26.02","26.03", "26.04","26.06","26.07", 
                 "26.18","26.19")
products_12 <- c("12.01")
products_10 <- c("10.01","10.05","10.06")
products_26_numbers <- c(2601,2602,2603,2604,2606,2607,2618,2619)
products_12_numbers <- c(1201)
products_10_numbers <- c(1001,1005,1006)
######################################################################
#create variables for HS Names column
######################################################################
names_26 <- c("Iron ores and concentrates, including roasted iron pyrites.",'Manganese ores and concentrates, including ferruginous
manganese ores and concentrates with a manganese content of
20%or more, calculated on the dry weight.','Copper ores and concentrates.','Nickel ores and concentrates.','Aluminium ores and concentrates.','Lead ores and concentrates.','Granulated slag (slag sand) from the manufacture of iron or
steel.','Slag, dross (other than granulated slag), scalings and other waste
from the manufacture of iron or steel.')
names_10 <- c('Wheat and Meslin','Maize (Corn)','Rice')
names_12 <- c('Soya beans, whether or not broken.')
HS_Names <- data.frame(cbind(c(products_26_numbers,products_10_numbers,products_12_numbers),c(names_26,names_10,names_12)))
names(HS_Names) <- c('HS Code','HS Names')
HS_Names[,1] <- as.numeric(c(products_26_numbers,products_10_numbers,products_12_numbers)) 
######################################################################
#create variables vor HS codes
######################################################################
products_26.01 <- c("2601.00", "2601.10","2601.11", "2601.12", "2601.20")
products_26.02 <- c("2602.00")
products_26.03 <- c("2603.00")
products_26.04 <- c("2604.00")
products_26.06 <- c("2606.00")
products_26.07 <- c("2607.00")
products_26.18 <- c("2618.00")
products_26.19 <- c("2619.00")
ores <- c("26.01", "26.02","26.03","26.04", "26.06","26.07")
######################################################################
#create variables for country names and short names
######################################################################
countries <- c("Bolivia","Brazil","Chile", "China", "Colombia", "Costa Rica", "India", "Indonesia","Mexico", "Pakistan", "Panama","Paraguay", "Philippines", "Peru", "Sri Lanka", "United States", "Uruguay", "Venezuela")
short <- c("BO","BR","CN","CL","CO","CR","IN","ID","MX","PK","PN","PY","PE","PH","LK","US","UY","VE")
codes <- c(68,76,152,156, 170,188,699,360,484,586,591,600,604,608,144,804,858,862) #baci country codes
length(countries)
length(codes)
len <- length(short)
######################################################################
#LOAD ALL DATA
######################################################################
load_all_data <- function(filename){
  #read data
  data <- data.frame(read_excel(filename))    
  ##############
  #replace NA by 0 in relevant columns
  for (i in columns){
    ind <- is.na(data[,i])
    data[ind,i] <- 0
  }
  ##############
  #choose only relevant columns
  data <- data[,columns]
  return(data <- data)
}

######################################################################
#LOAD SPLIT DATA
#gets a filename and a number, then load all files with the numbers from 1,2,3... at the end 
######################################################################
load_split_data <- function(filename,n){
  for(m in 1:n){
    #set different filenames with a new ending
    filename1 <- paste0(substr(filename,start = 0, stop = 70),"-",m, substr(filename,start = 71,stop = 75))
    #append the files together in one df:
    if(m==1){datasp <- data.frame(read_excel(paste0(filename1)))
    }else{datasp <- rbind(datasp, data.frame(read_excel(paste0(filename1))))}
  }
  #datasp
  return(datasp)
}

######################################################################
#PIVOT_NEXT_GEN
#gets a filename and computes the PANJIVA Part for the HS Code in the filename 
######################################################################

PIVOT_NEXT_GEN <- function(filename){
  #get current 2 digit HS  from filename
  HS <- substr(filename, start = 64, stop = 65)
  
  #define columnames of interest
  colu <- c('Date','HS.Code','Shipment.Origin','Port.of.Unlading.Country','Port.of.Lading','Port.of.Unlading','Weight..KG.')
  
  #for 10er HS Codes use the split data loading else read the file normally
  if(HS_2dig_now==products_class[2]){
    datapiv <-load_split_data(filename,10)[,colu]
  }else{datapiv <- data.frame(read_excel(filename))[,colu]}
  
  
  datapiv[colu[1]] <- paste0(year)    #add year   
  datapiv[is.na(datapiv[,2]),2] <- 'Unknown' #all NA to Unknown
  datapiv[is.na(datapiv[,3]),3] <- 'Unknown' #all NA to Unknown
  datapiv[is.na(datapiv[,4]),4] <- 'Unknown' #all NA to Unknown
  datapiv[is.na(datapiv[,5]),5] <- paste0('Unknown(',datapiv[is.na(datapiv[,5]),3],")")  #all NA Ports to Unknown(Country)
  datapiv[is.na(datapiv[,6]),6] <- paste0('Unknown(',datapiv[is.na(datapiv[,6]),4],")")  #all NA Ports to Unknown(Country)
  
  #for each HS Codes create a seperate data set 
  for(j in get(paste0("products_",HS,"_numbers"))){
    
    #filter for each HS Code
    data_filtered <- 
      datapiv %>%
      subset(grepl(j, HS.Code, fixed = TRUE))
    
    ###############
    #Saves a HS specific dataset and need a helper variable to change iterative the HS Code in each data set
    assign(paste0("just_",j),data_filtered)
    helper <- get(paste0("just_",j))
    helper$HS.Code <- j           
    assign(paste0("just_",j), helper)
    ###############
    #save datasets of the aggregated data
    assign(paste0("aggr_",j), aggregate(Weight..KG. ~ ., data = get(paste0("just_",j)),FUN = sum, na.action = NULL))
    #append them together
    if(j==get(paste0("products_",HS,"_numbers"))[1]){dfaggr <- get(paste0("aggr_",j))
    }else{dfaggr <- rbind(dfaggr,get(paste0("aggr_",j)))}
  }
  #defind columnnames
  pivnames <<- c('Year','HS Code', 'Database', 'Exporter', 'Importer', 'Port of Lading', 'Port of Unlading', 'Value')
  #add Database column
  dfaggr <- cbind(dfaggr,"Source"=rep("PANJIVA",nrow(dfaggr)))
  #right order of the df
  dfaggr <- dfaggr[,c(1,2,8,3,4,5,6,7)]
  #name the columns new
  names(dfaggr) <- pivnames
  #transform KG in Tonns
  dfaggr$Value <- dfaggr$Value/1000 
  return(dfaggr)
}

######################################################################
#PIVOT_NEXT_GEN
#compute the BACI Part of the TAble for the specific year
######################################################################

PIVOT_NEXT_GEN_BACI <- function(){
  
  #####################################################################
  #Load BAci data
  #####################################################################
  
  # Initializing R script (load R packages and set paths to folders etc.)
  source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))
  root_folder <<- root_folder1
  
  # Call script to clear ALANG and processed data folders of the present data feed & therefore set diffrent paths before
  path[["df_Processed"]] <- paste0(path$Processed,"/",datafeed_name)  # Add datafeed specific path for output data
  path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)
  path[["set"]] <- paste0(path$root,"ProcessedData/",datafeed_name)
  path$ALANG <- paste0(path$ALANG,"/",datafeed_name)
  
  source(paste0(root_folder,"Rscripts/Subroutines/Load_BACI.R")) # Loading raw data
  
  # define column names of interest
  colu <- c('year','hs6','From','To','value','Quantity')
  
  #save and filter data which is globaly available from BACI set
  datapiv <- data[as.numeric(data$hs6)/100000 > 1,]
  datapiv[is.na(datapiv[,1]),1] <- 'Unknown'   #all NA to Unknown
  datapiv[is.na(datapiv[,2]),2] <- 'Unknown'   #all NA to Unknown
  datapiv[is.na(datapiv[,3]),3] <- 'Unknown'   #all NA to Unknown
  datapiv[is.na(datapiv[,4]),4] <- 'Unknown'   #all NA to Unknown
  #all ports in BACI get the string: Unknown
  datapiv <- cbind(datapiv[,-7],'Port of Lading' = paste0('Unknown(',datapiv[,3] ,')'),'Port of Unlading' = paste0('Unknown(',datapiv[,4] ,')'))
  
  datapiv[,2] <- as.character(as.numeric(datapiv[,2])/100) # devide BAci HS codes by 100 to seperate the 4digit from the last 2 digits
  datapiv[,7] <- as.character(datapiv[,7])
  datapiv[,8] <- as.character(datapiv[,8])

  #for each HS Codes create a seperate data set
  for(j in c(products_10_numbers,products_12_numbers,products_26_numbers)){
    
    #filter for each HS Code
    data_filtered <- 
      datapiv %>%
      subset(grepl(j, hs6, fixed = TRUE))
    
    ###############
    #save the data set and need helper again for iterative HS Code changing
    assign(paste0("just_",j),data_filtered)
    helper <- get(paste0("just_",j))
    helper$hs6 <- j
    assign(paste0("just_",j), helper)
    
    ###############
    #save aggregated datasets and append them together
    assign(paste0("aggr_",j), aggregate(Quantity ~ ., data = get(paste0("just_",j)),FUN = sum, na.action = NULL))
    if(j==products_10_numbers[1]){dfaggr <- get(paste0("aggr_",j))
    }else{dfaggr <- rbind(dfaggr,get(paste0("aggr_",j)))}
  }
  #read baci product and region codes
  classi <- list("region" = read.csv(paste0(path$Raw,"/BACI/country_code_baci92.csv"),stringsAsFactors=FALSE),
                 "product" = read.csv(paste0(path$Raw,"/BACI/product_code_baci92.csv"),stringsAsFactors=FALSE))
  # Insert Taiwan region into baci list
  classi$region$iso3[classi$region$i == 490] <- "TWN"
  #define names and rename the imported data for Export...
  names(classi$region) <- c("EXPORTER", "iso2",    "iso3",    "From" )
  dfaggr  <-  left_join(dfaggr, classi$region, by = 'From')
  #...and Import
  names(classi$region) <- c("IMPORTER", "iso2",    "iso3",    "To" )
  dfaggr <- left_join(dfaggr, classi$region, by = 'To')
  #delete unused columns
  dfaggr <- dfaggr[,c(-3,-4,-5,-10,-11,-13,-14)]
  #add Database column
  dfaggr <- cbind(dfaggr, 'Database' = as.character(rep('BACI',nrow(dfaggr))))
  #put columns in the right order
  dfaggr <- dfaggr[,c(1,2,8,6,7,3,4,5)]
  #and rename them again
  names(dfaggr) <- c('Year','HS Code', 'Database', 'Exporter', 'Importer', 'Port of Lading', 'Port of Unlading', 'Value')
  #change Port values to Unknown(COUNTRY)
  dfaggr[,6] <- paste0('Unknown(',dfaggr[,4] ,')')
  dfaggr[,7] <- paste0('Unknown(',dfaggr[,5] ,')')
  return(dfaggr)
}

