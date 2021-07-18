
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
       7312.90,
       73.13 ,
       7313.00)
a13 <- c(73.14,
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
a14 <- c(73.15 ,
       7315.11,
       7315.12,
       7315.19,
       7315.20,
       7315.81,
       7315.82,
       7315.89,
       7315.90)
a15 <- c(73.16,
       7316.00)
a16 <- c(73.17,
       7317.00)
a17 <- c(73.18,
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
a18 <- c(73.19,
       7319.40,
       7319.90)
a19 <- c(73.20,
       7320.10,
       7320.20,
       7320.90)
a20 <- c(73.21,
       7321.11,
       7321.12,
       7321.19,
       7321.81,
       7321.82,
       7321.89,
       7321.90)
a21 <- c(73.22,
       7322.11,
       7322.19,
       7322.90)
a22 <- c(73.23,
       7323.10,
       7323.91,
       7323.92,
       7323.93,
       7323.94,
       7323.99)
a23 <- c(73.24,
       7324.10,
       7324.21,
       7324.29,
       7324.90)
a24 <- c(73.25,
       7325.10,
       7325.91,
       7325.99)
a25 <- c(73.26,
       7326.11,
       7326.19,
       7326.20,
       7326.90)

products_73 <- c()
for (i in 1:25){
  assign(paste0("products_",get(paste0("a",i))[1]),as.character(get(paste0("a",i))[-1]))
  products_73 <- c(products_73,as.character(get(paste0("a",i))[1]))
}




#Choose relevant columns
######################################################################
columns <- c(1,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
products_class <- c("26","73")
products_26 <- c("26.01", "26.18","26.19")
products_26.01 <- c("2601.00", "2601.10","2601.11", "2601.12", "2601.20")
products_26.18 <- c("2618.00")
products_26.19 <- c("2619.00")
######################################################################
#create variables for country names and short names
######################################################################
countries <- c("Brazil","China", "Chile", "Peru", "United States")
short <- c("BR","CN","CL","P","US")
######################################################################
#LOAD ALL DATA
######################################################################
load_all_data <- function(filename){
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
#CREATE COUNTRY DATA
######################################################################
create_country_data <- function(filename, country){
  ###############
  #load whole dataset
  data <- load_all_data(filename)
  ###############
  #returns country datasets
  return(data[data$Shipment.Origin==countries[countries==country],])
}
######################################################################
#HS_CODES
####################################################################
details_HS_whole <- function(filename){
  ###############
  #first load data
  data <- load_all_data(filename)
  ###############
  #create empty data.frame
  rnames <- c("Percentage on all shipments of HS code")
  len <- 1:length(rnames)
  df <- data.frame(A=len,B=len,C=len,D=len,E=len, row.names = rnames)
  names(df) <- products
  ###############
  #unique HS codes from the data
  un_HS <<- unique(data$HS.Code)
  print(paste0("number of unique HS codes: ",length(un_HS)))
  print(paste0("thereof more than one HS code: ",sum(str_count(un_HS, pattern = ";")+1>1)))
  for(x in products){
    df[,x] <- round(sum(str_count(data$HS.Code, pattern = x))*100/nrow(data),2)
    round(sum(str_count(data$HS.Code, pattern = x))*100/nrow(data),2)
  }
  return(df)
}
####################################################################
#PORTS LADING
####################################################################
details_Port_Lad <- function(filename){
  HS <- substr(filename, start = 64, stop = 68)
  ###############
  #first create data for the countries
  len_data <- 0
  for(x in 1:length(countries)){
    assign(paste0(short[x],HS),create_country_data(filename,countries[x]))
    len_data <- len_data + nrow(get(paste0(short[x],HS)))
  }
  
  ###############
  #LADING
  PL <- "Port_Lad"
  ###############
  #settle current products and categories
  current_prods <- get(paste0("products_",HS_4dig_now))
  rnames <- c("Number of unique ports lading", "Percentage of NA for ports lading","Share of all shipments","Share in export")
  
  ###############
  #generate informations for 4 digit HS code
  
  #Names of unique ports of lading saved in extra variables
  for (x in paste0(short,HS)){assign(paste0("names_",x,PL),unique(get(x)$Port.of.Lading[unique(get(x)$Port.of.Lading)!=0]))}
  
  ###############
  #create empty vectors
  NU <- c("a","b", 1:5)
  PN <- c("a","b", 1:5)
  PS <- c("a","b", 1:5)
  
  for (x in 1:5){
    
    #Numbers of unique Ports lading
    NU[1] <- rnames[1]
    NU[2] <- HS_4dig_now
    NU[x+2] <- length(get(paste0("names_",short[x],HS,PL)))
  
    #Percentage of NA for ports lading for different countries
    y <- get(paste0(short[x],HS))
    PN[1] <- rnames[2]
    PN[2] <- HS_4dig_now
    PN[x+2] <- round(sum(y$Port.of.Lading==0)*100/nrow(y),2)
  
    #Percentage of HS code for different countries (use len_data)
    PS[1] <- rnames[3]
    PS[2] <- HS_4dig_now
    PS[x+2] <- round(nrow(get(paste0(short[x],HS)))*100/len_data,2)
  }
  ###############
  #create data frame
  df <- data.frame(rbind(NU,PN,PS))
  
  ###############
  #start for loop for current products
  for (i in current_prods){
    #i <- current_prods[4]
    #Names of unique ports of lading saved in extra variables
    for (x in paste0(short,HS)){
      #HS code is in this rows index
      ind1 <- grepl(i,get(x)$HS.Code , fixed = TRUE)
      assign(paste0("names_",x,PL,i),unique(get(x)[ind1,]$Port.of.Lading[unique(get(x)[ind1,]$Port.of.Lading)!=0]))
    }
    
    ###############
    #create empty vectors
    NU <- c("a","b", 1:5)
    PN <- c("a","b", 1:5)
    PS <- c("a","b", 1:5)
    
    for (x in 1:5){
      #create index of rows for different hs code
      ind1 <- grepl(i, get(paste0(short[x],HS))$HS.Code , fixed = TRUE)

      #Numbers of unique Ports lading
      NU[1] <- rnames[1]
      NU[2] <- i
      NU[x+2] <- length(get(paste0("names_",short[x],HS,PL,i)))
      
      #Percentage of NA for ports lading for different countries
      y <- get(paste0(short[x],HS))
      PN[1] <- rnames[2]
      PN[2] <- i
      PN[x+2] <- round(sum(y[ind1,]$Port.of.Lading==0)*100/nrow(y),2)
      
      #Percentage of Shipment different countries (use len_data)
      PS[1] <- rnames[4]
      PS[2] <- i
      PS[x+2] <- round(nrow(get(paste0(short[x],HS))[ind1,])*100/nrow(get(paste0(short[x],HS))),2)
    }  
    df1 <- data.frame(rbind(NU,PN,PS))
    df <- rbind(df,df1)
  }
  write.table(df,file = paste0(root_folder,"Panjiva/",HS_4dig_now,"_",year,"_","lad.csv"),row.names = FALSE,col.names = c("Info","HS Code",countries),sep = ",",na = "NaN")
  #source(paste0(root_folder,"Rscripts/Subroutines/Numbers2File.R"))
  #Numbers2File(df,"000.csv")
}
####################################################################
#PORTS UNLADING
####################################################################
details_Port_Unlad <- function(filename){
  HS <- substr(filename, start = 64, stop = 68)
  ###############
  #first create data for the countries
  len_data <- 0
  for(x in 1:length(countries)){
    assign(paste0(short[x],HS),create_country_data(filename,countries[x]))
    len_data <- len_data + nrow(get(paste0(short[x],HS)))
  }
  
  ###############
  #UNLADING
  PU <- "Port_Unad"
  ###############
  #settle current products and categories
  current_prods <- get(paste0("products_",HS_4dig_now))
  rnames <- c("Number of unique ports unlading", "Percentage of NA for ports unlading")
  
  ###############
  #generate informations for 4 digit HS code
  
  #Names of unique ports of lading saved in extra variables
  for (x in paste0(short,HS)){assign(paste0("names_",x,PU),unique(get(x)$Port.of.Unlading[unique(get(x)$Port.of.Unlading)!=0]))}
  
  ###############
  #create empty vectors
  NU <- c("a","b", 1:5)
  PN <- c("a","b", 1:5)

  for (x in 1:5){
    
    #Numbers of unique Ports unlading
    NU[1] <- rnames[1]
    NU[2] <- HS_4dig_now
    NU[x+2] <- length(get(paste0("names_",short[x],HS,PU)))
    
    #Percentage of NA for ports unlading for different countries
    y <- get(paste0(short[x],HS))
    PN[1] <- rnames[2]
    PN[2] <- HS_4dig_now
    PN[x+2] <- round(sum(y$Port.of.Unlading==0)*100/nrow(y),2)
    
  }
  ###############
  #create data frame
  df <- data.frame(rbind(NU,PN))
  
  ###############
  #start for loop for current products
  for (i in current_prods){
    #i <- current_prods[4]
    #Names of unique ports of unlading saved in extra variables
    for (x in paste0(short,HS)){
      #HS code is in this rows index
      ind1 <- grepl(i,get(x)$HS.Code , fixed = TRUE)
      assign(paste0("names_",x,PU,i),unique(get(x)[ind1,]$Port.of.Unlading[unique(get(x)[ind1,]$Port.of.Unlading)!=0]))
    }
    
    ###############
    #create empty vectors
    NU <- c("a","b", 1:5)
    PN <- c("a","b", 1:5)
    
    
    for (x in 1:5){
      #create index of rows for different hs code
      ind1 <- grepl(i, get(paste0(short[x],HS))$HS.Code , fixed = TRUE)
      
      #Numbers of unique Ports unlading
      NU[1] <- rnames[1]
      NU[2] <- i
      NU[x+2] <- length(get(paste0("names_",short[x],HS,PU,i)))
      
      #Percentage of NA for ports unlading for different countries
      y <- get(paste0(short[x],HS))
      PN[1] <- rnames[2]
      PN[2] <- i
      PN[x+2] <- round(sum(y[ind1,]$Port.of.Unlading==0)*100/nrow(y),2)
    }  
    df1 <- data.frame(rbind(NU,PN))
    df <- rbind(df,df1)
  }
  write.table(df,file = paste0(root_folder,"Panjiva/",HS_4dig_now,"_",year,"_","unl.csv"),row.names = FALSE,col.names = c("Info","HS Code",countries),sep = ",",na = "NaN")
  #source(paste0(root_folder,"Rscripts/Subroutines/Numbers2File.R"))
  #Numbers2File(df,"000.csv")
}

