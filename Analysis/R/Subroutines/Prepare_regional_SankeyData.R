################################################################################
# This function aggregates the global PSUTs for drawing Sankeys in eSankey

Prepare_regional_SankeyData <- function(r)
{
  
  # r contains a string selecting regions (either single country or a country group, e.g. Europe)
  # See country and country group list in object base$region
  
  # For test purposes and code development, we select an arbitrary country here
  r <- "Europe"
  
  # Compile IO model
  IO <- Build_IOT(SUT,"ixi")      
  
  # Read final demand and aggregate across demand categories
  Y <- Agg(x = IO$y, rep(1:num$reg, each = 6), 2)
  
  # Read Leontief inverse L, boundary input/output blocks e and gross production x and technology matrix A
  L <- IO$L
  e <- IO$e   # See Code$V for labels
  x <- IO$x
  Z <- IO$Z
  
  # Create technology matrix A, by dividing Z columnswise with x
  A <- t( t(Z)/x )
  
  # Aggregate e into total boundary inputs and outputs
  # e <- Agg(x = e, Code$V$Boundary, 2)
  
  # Check that total inputs (almost) equal total outputs
  sum(e[,1:5])   # Boundary inputs (BI)
  sum(e[,6:7],Y) # Boundary outputs (BO)
  
  # Create direct intensities 
  f <- e/x
  f[is.na(f)] <- 0   # Remove NA just in case
  
  
  ### Now the compilation of the region-specific flow matrix starts ###
  
  # Select final demand of region r
  if (r %in% base$region$Name){
    y_r <- Y[, Code$Y$index[ Code$Y$RegionName == r ] ]  ### Rene start here!
  }
  if (r %in% base$region$Region& r!='China'){
    rind = base$region$Code[base$region$Region==r]
    y_r <- rowSums(Y[, rind ])
  }
  
  # Calculate gross output reflecting only flows associated with Y of r
  x_r <- as.vector( L %*% y_r )
  
  # Use A to estimate the new flow matrix Z, now only flows associated with y of r
  Z_r <- t( t(A) * x_r )
  
  # For checking the results, export matrix Z_r to xlsx file
  write.xlsx(Z_r, file = paste0( path$output,"/Sankey/Data/",job$year,"/Z_regional_sankey_",r,"_fulldetail.xlsx"), colnames = TRUE)
  
  # For testing the reuslt, aggregate the flow matrix Z and write it to xlsx file
  Z_r_test <- Agg(Z_r, aggkey = Code$Z$SectorName[Code$Z$EntityCode == 1], 1)
  Z_r_test <- Agg(Z_r_test, aggkey = Code$Z$SectorName[Code$Z$EntityCode == 1], 2)
  write.xlsx(Z_r_test, file = paste0( path$output,"/Sankey/Data/",job$year,"/_Z_regional_sankey_",r,"_aggregated.xlsx"), colnames = TRUE)
  
  # Calculate new BI & BO, stored in object B_r now reflecting only flows associated with Y of r
  B_r <- f * x_r
  colnames(B_r) <- Code$V$Entity
  
  # Check that total inputs and outputs, that are associates with r, balance out
  dif <- round( ( sum(B_r[,1:5]) - sum(B_r[,6:7],y_r) ) * 100 / sum(B_r[,1:5]),digits = 2 )
  print(paste("discrepancy of inputs minus outputs:",dif,"%") )
  remove(dif)
  
  # Create data frame to check process balances
  bal <- data.frame(Code$Z[ Code$Z$EntityCode == 1,],
                    "Intermediate_input" = colSums(Z_r),
                    "Boundary_input" = rowSums(B_r[,1:5]),
                    "Intermediate_output" = rowSums(Z_r),
                    "Boundary_output" = rowSums(B_r[,6:7]),
                    "Final_use" = y_r,
                    stringsAsFactors = FALSE) 
  
  bal["Total_input"] <- bal$Intermediate_input + bal$Boundary_input
  bal["Total_output"] <- bal$Intermediate_output + bal$Boundary_output + bal$Final_use
  
  write.xlsx(bal, file = paste0( path$output,"/Sankey/Data","/",job$year,"/Process_balances_regional_sankey_",r,".xlsx"), colnames = TRUE)
  

  ### Creating aggregated data set for sankey of region r ###
  
  # First, compute new codes for aggregating the orginal tables
  Code_r <- Code   # Rene here!
  
  if (r %in% base$region$Name){
    Code_r$Z <- Code_r$Z %>% filter(EntityCode == 1) %>% mutate(Key = "RoW") %>% 
    mutate(Key = replace(Key, RegionName == r, "Dom") )  %>% mutate(Key = paste0(Key,"§",SectorName)) %>% 
    select(RegionName,SectorCode, SectorName, SectorIndex,Key)
  }
  if (r %in% base$region$Region & r!='China'){
    Code_r$Z <- Code_r$Z %>% filter(EntityCode == 1) %>% mutate(Key = "RoW") %>% 
      mutate(Key = replace(Key, RegionName %in% base$region$Name[base$region$Region==r], "Dom") )  %>% mutate(Key = paste0(Key,"§",SectorName)) %>% 
      select(RegionName,SectorCode, SectorName, SectorIndex,Key)
  }

  # Aggregate all tables with key
  Z_r <- Agg( Z_r, Code_r$Z$Key, 1 )
  Z_r <- Agg( Z_r, Code_r$Z$Key, 2 )
  y_r <- Agg(y_r, Code_r$Z$Key, 1)
  B_r <- Agg(B_r, Code_r$Z$Key, 1)
  
  # Rearrange order of tables in case this is necessary (order depends on country that is selected as r)
  if(colnames(Z_r)[1] != 'Dom§Mining') Z_r <- Z_r[ c(31:60,1:30), c(31:60,1:30) ]
  if(rownames(y_r)[1] != 'Dom§Mining') y_r <- y_r[ c(31:60,1:30), ]
  if(rownames(B_r)[1] != 'Dom§Mining') B_r <- B_r[ c(31:60,1:30), ]
  
  # Check again that total inputs and outputs, that are associates with r, balance out
  dif <- round( ( sum(B_r[,1:5]) - sum(B_r[,6:7],y_r) ) * 100 / sum(B_r[,1:5]),digits = 2 )
  print(paste("discrepancy of total boundary inputs minus total boundary outputs:",dif,"%") )
  remove(dif)
  
  
  
  
  
  
  
  # Create objects for storing sankey data; dom = domestic tables; IM = imports
  
  dom <- list( "Z" = matrix(data = 0, nrow = num$ind, ncol = num$ind),
               "Y" = matrix(data = 0, nrow = num$ind, ncol = 1),
               "IN" = matrix(data = 0, nrow = num$va, ncol = num$ind ),
               "OUT" = matrix(data = 0, nrow = num$ind, ncol = nrow(SUT$w)) )
  
  im <- list( "Z" = matrix( data = 0, nrow = num$ind, ncol = num$ind ),
              "Y" = matrix( data = 0, nrow = num$ind, ncol = 1) )
  
  
  row <- list( "Z" = matrix(data = 0, nrow = num$ind, ncol = num$ind),
               "Y" = matrix( data = 0, nrow = num$ind, ncol = 1),
               "IN" = matrix(data = 0, nrow = num$va, ncol = num$ind ),
               "OUT" = matrix(data = 0, nrow = num$ind, ncol = nrow(SUT$w)) )
  
  # Read and store domestic table of country i and 
  dom$Z <- Z_r[ c(1:30), c(1:30) ]
  dom$Y <- y_r[c(1:30)]
  dom$IN <- t( B_r[c(1:30),1:5] )
  dom$OUT <- B_r[c(1:30),6:7]
  
  im$Z <- Z_r[c(31:60),c(1:30)]+Z_r[c(1:30),c(31:60)]
  im$Y <- y_r[c(31:60)]
  
  row$Z <- Z_r[c(31:60),c(31:60)]
  row$IN <- t( B_r[c(31:60),1:5] )
  row$OUT <- B_r[c(31:60),6:7]
  
  ## Remove own-use flows (on the diagonal) for the Sankeys
  diag(dom$Z) <- 0
  diag(im$Z) <- 0
  #########NEW
  diag(row$Z) <- 0
  
  #set all cells to 0 where we have cells with the same aggregation group
  agg_key <- base$industry$Aggregate  # Select predefined aggregater
  
  #Problem is in agg_key due the different manufacturing flows
  # for(i in agg_key){
  #   # i=agg_key[1]
  #   ind <- which(agg_key==i)
  #   for (k in ind){
  #     for (l in ind){
  #       dom$Z[k,l] <- 0
  #       im$Z[k,l] <- 0
  #       row$Z[k,l] <- 0
  #     }
  #   }
  # }
  
  ## Aggregating sectors into groups
  
  # For the Sankey, we need 2 different aggregations for manufacturing sectors. 
  # One is for the Z matrix (more aggregated) and one for the Y (less aggregated) matrix
  
  Y_disagg <- list( "dom" = Agg(x = dom$Y, aggkey = base$industry$Aggregate, dim = 1),
                    "im" = Agg(x = im$Y, aggkey = base$industry$Aggregate, dim = 1),
                    'row' = Agg(x = row$Y, aggkey = base$industry$Aggregate, dim = 1))
  

  # Create aggregate of all manufacturing
  agg_key[ base$industry$Type %in% "Final" ] <- "Manufacturing"

  dom$Z <- Agg(x =  dom$Z, aggkey = agg_key, dim = 1)
  dom$Z <- Agg(x =  dom$Z, aggkey = agg_key, dim = 2)

  dom$Y <- Agg(x = dom$Y, aggkey = agg_key, dim = 1)
  dom$IN <- Agg(x = dom$IN, aggkey = agg_key, dim = 2)
  dom$OUT <- Agg(x = dom$OUT, aggkey = agg_key, dim = 1)

  im$Z <- Agg(x =  im$Z, aggkey = agg_key, dim = 1)
  im$Z <- Agg(x =  im$Z, aggkey = agg_key, dim = 2)

  im$Y <- Agg(x = im$Y, aggkey = agg_key, dim = 1)

  ########NEW
  row$Z <- Agg(x =  row$Z, aggkey = agg_key, dim = 1)
  row$Z <- Agg(x =  row$Z, aggkey = agg_key, dim = 2)
  row$Y <- Agg(x = row$Y, aggkey = agg_key, dim = 1)
  row$IN <- Agg(x = row$IN, aggkey = agg_key, dim = 2)
  row$OUT <- Agg(x = row$OUT, aggkey = agg_key, dim = 1)

  ## Prepare more aggregated Z,Y,IN,OUT for export to eSankey
  # Note that boundary inputs and outputs are aggregated into one item each here
  
  # Create empty table to merge all tables into one
  result <- matrix( data = 0,
                    nrow = ( 3 * nrow(dom$Z) + 2 ),
                    ncol = ( ncol(dom$Z) + 1#ncol(dom$Y) 
                             + 1 ) )
  
  # Merge transaction matrices
  result[1:(3*nrow(dom$Z)) ,1:nrow(dom$Z)] <- rbind( dom$Z, im$Z, row$Z )                
  
  # Merge final demand
  result[1:(3*nrow(dom$Z)), ncol(dom$Z)+1 ] <- c(dom$Y,im$Y, rep(0,length(im$Y)))
  
  # Add boundary inputs
  result[ nrow(result)-1, 1:ncol(dom$Z)  ] <- colSums( dom$IN )
  result[ nrow(result), 1:ncol(dom$Z)  ] <- colSums( row$IN )
  
  # Add boundary outputs
  result[ 1:nrow(dom$Z), ncol(result) ] <- rowSums( dom$OUT )
  result[ (nrow(dom$Z)*2+1):(nrow(dom$Z)*3), ncol(result) ] <- rowSums( row$OUT )
  
  rownames(result) <- c( paste( "Domestic -",rownames(dom$Z) ),
                         paste( "Foreign - ",rownames(dom$Z) ),
                         paste( "ROW - ",rownames(dom$Z) ),
                         "Boundary inputs DOM","Boundary inputs ROW" )
  
  colnames(result) <- c( colnames(dom$Z), "Final Use", "Boundary outputs" )
  
  
  ## Prepare more disaggregated final demand for export
  
  ###########NEW
  rownames(Y_disagg$row)  <- paste( "ROW -" ,rownames(Y_disagg$im) )
  disagg_names = rownames(Y_disagg$im)
  disagg_names = disagg_names
  
  rownames(Y_disagg$dom)  <- paste( "Domestic -" ,rownames(Y_disagg$dom) ) 
  rownames(Y_disagg$im)  <- paste( "Foreign -" ,rownames(Y_disagg$im) ) 
  
  
  result_disagg <- rbind( Y_disagg$dom, Y_disagg$im, Y_disagg$row)  # Combine in one data frame
  
  colnames(result_disagg) <- "Final Use"
  
  ## Put both results in one list for writing to xlsx
  
  out <- list("agg" = result,
              "disagg" = result_disagg,
              'disagg_names'= disagg_names)#substr(disagg_names,11,100))
  
  # Change units to mega tonnes
  out$agg <- out$agg / 10^6   
  out$disagg <- out$disagg / 10^6
  
  
  ###################################################################### dataframe editing for python code
  main <- data.frame()
  resround <- round(out$agg,2)
  
  BI=list(c('BI Crude Ore', 'Mining'),
      c('BI Coke/air/flux','Reduction'),
      c('BI Eol-Scrap','Scrap preparation'))
  
  BO=list(c('Mining', 'BO Gangue'),
      c('Reduction','BO Blast furnance\ngas & slag'),
      c('Steelmaking','BO Steelmaking Slag'))
  
  FU=list(c('Construction','Construction'),
      c('Manufacture of machinery and equipment n.e.c.','Machinery and \nequipment n.e.c.'),
      c('Manufacture of motor vehicles, trailers and semi-trailers','Motor vehicles, trailers \nand semi-trailers'),
      c('Manufacturing n.e.c.','Manufacturing n.e.c.'),
      c('Manufacture of other transport equipment','Other transport \nequipment')
      )
  
  for (i in 1:length(rownames(resround))){
    values <- resround[i,]
    source <- rownames(resround)[i]
    target <- names(values)
    main <- rbind(main,data.frame('source'=source,'target'=target,'value'=values))
  }
  main <- main[main$value!=0,]  
  main$type <- ''
  main$type[grepl('Domestic',main$source)] <- 'D'
  main$type[grepl('Foreign',main$source)] <- 'F'
  main$type[grepl('ROW',main$source)] <- 'ROW'
  main$type[grepl('Boundary',main$target) & grepl('ROW',main$source)==F] <- 'BO'
  main$type[grepl('Boundary',main$source) & grepl('ROW',main$source)==F] <- 'BI'
  main$source <- as.character(main$source)
  main$target <- as.character(main$target)
  for (i in colnames(resround)){
    main$source[grepl(i,main$source)] <- i
  }
  
  for(i in 1:3){
    if (length(main[grepl('Boundary',main$source) & main$target==BI[[i]][2],]$source)>0){
      main[grepl('Boundary',main$source) & main$target==BI[[i]][2],]$source <- BI[[i]][1]
    }
    if (length(main[grepl('Boundary',main$target) & main$source==BO[[i]][1],]$target)>0){
      main[grepl('Boundary',main$target) & main$source==BO[[i]][1],]$target <- BO[[i]][2]  
    }
  }
  
  
  second <- data.frame(round(out$disagg,2))
  second$type <- ''
  second$type[grepl('Domestic',rownames(second))] <- 'D'
  second$type[grepl('Foreign',rownames(second))] <- 'F'
  second$type[grepl('ROW',rownames(second))] <- 'ROW'
  second$target <- ''
  for (i in 1:nrow(second)){
    j <- gregexpr(pattern =' ',rownames(second)[i])[[1]][2]+1
    second$target[i] <- substr(rownames(second)[i],j,nchar(rownames(second)[i]))
  }
  colnames(second)[1] <- 'value'
  second <- second[second$value!=0,]
  second$source <- colnames(resround)[length(colnames(resround))-3]
  second <- second[order(-second$value),]
  rownames(second) <- 1:length(rownames(second))-1
  out$FU_values <- second
  
  main <- rbind(main,second)
  main <- main[main$target!='Final Use',]
  
  for (i in 1:5){
    main[main$target==FU[[i]][1],]$target <- FU[[i]][2]  
  }
  
  rownames(main) <- 1:length(rownames(main))-1
  out$pivot <- main
  #############################save aggr sums for the plot in 
  flowvec <- c()
  valvec <- c()
  for (i in unique(main$source)){
    valvec <- c(valvec,sum(main$value[main$source==i]))
    flowvec <- c(flowvec,i)
  }
  ssum <- data.frame('flow'=flowvec,'value'=valvec)
  
  flowvec <- c()
  valvec <- c()
  for (i in unique(main$target)){
    valvec <- c(valvec,sum(main$value[main$target==i]))
    flowvec <- c(flowvec,i)
  }
  tsum <- data.frame('flow'=flowvec,'value'=valvec)
  
  rownames(tsum) <- 1:length(rownames(tsum))-1
  rownames(ssum) <- 1:length(rownames(ssum))-1
  
  out$source_sums <- ssum
  out$target_sums <- tsum
  out$BO <- BO
  out$BI <- BI
  out$FU <- FU
  

  
  write.xlsx( out, paste0(path$output,"/Sankey/Data/",job$year,"/",job$year,"_Data_for_",r,"_Sankey.xlsx"), rowNames = TRUE )
}    
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

for (i in base$region$Name){
  Prepare_regional_SankeyData(i)
}
for (i in unique(base$region$Region)){
  if (i=='Africa'){i <- 'RoW Africa'}
  if (i=='Middle East'){i <- 'RoW Middle East'}
  Prepare_regional_SankeyData(i)
}

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
Prepare_SankeyData_2.0 <- function()
{
  IO <- Build_IOT(SUT,"ixi")      # Compile IO model
  
  IO$y <- Agg(x = IO$y, rep(1:num$reg, each = 6), 2)  # Aggregate final demand
  
  # Create objects for storing results; dom = domestic tables; IM = imports
  
  dom <- list( "Z" = matrix(data = 0, nrow = num$ind, ncol = num$ind),
               "Y" = matrix(data = 0, nrow = num$ind, ncol = ncol(IO$y)/num$reg),
               "IN" = matrix(data = 0, nrow = num$va, ncol = num$ind ),
               "OUT" = matrix(data = 0, nrow = num$ind, ncol = nrow(SUT$w)) )
  
  im <- list( "Z" = matrix( data = 0, nrow = num$ind, ncol = num$ind ),
              "Y" = matrix( data = 0, nrow = num$ind, ncol = ncol(IO$y)/num$reg) )
  
  # Loop over each region and aggreate results
  
  for(m in 1:num$reg)
  {
    
    # Read indices of region m
    indi <- Code$Z %>% filter(RegionCode == m, EntityCode == 1) %>%  pull(SectorIndex)
    
    # Read and store domestic table of country i and 
    dom$Z <- dom$Z + IO$Z[indi, indi]
    dom$Y <- dom$Y + IO$y[indi, m]
    dom$IN <- dom$IN + t( IO$e[indi, 1:5] )
    dom$OUT <- dom$OUT + IO$e[indi, 6:7]
    
    for(n in setdiff(1:num$reg,m) )
    {
      # Read indices of importing region
      indi_foreign <- Code$Z %>% filter(RegionCode == n, EntityCode == 1) %>% pull(SectorIndex)
      
      # Read and store imports from region n
      im$Z <- im$Z + IO$Z[ indi_foreign, indi]
      im$Y <- im$Y + IO$y[ indi_foreign, m]
    }
  }
  
  
  ## Remove own-use flows (on the diagonal) for the Sankeys
  
  diag(dom$Z) <- 0
  diag(im$Z) <- 0
  
  ## Aggregating sectors into groups
  
  # For the Sankey, we need 2 different aggregations for manufacturing sectors. 
  # One is for the Z matrix (more aggregated) and one for the Y (less aggregated) matrix
  
  Y_disagg <- list( "dom" = Agg(x = dom$Y, aggkey = base$industry$Aggregate, dim = 1),
                    "im" = Agg(x = im$Y, aggkey = base$industry$Aggregate, dim = 1) )
  
  disagg_names = rownames(Y_disagg$im)
  
  agg_key <- base$industry$Aggregate  # Select predefined aggregater
  
  # Create aggregate of all manufacturing
  agg_key[ base$industry$Type %in% "Final" ] <- "Manufacturing"
  
  dom$Z <- Agg(x =  dom$Z, aggkey = agg_key, dim = 1)
  dom$Z <- Agg(x =  dom$Z, aggkey = agg_key, dim = 2)
  
  dom$Y <- Agg(x = dom$Y, aggkey = agg_key, dim = 1)
  dom$IN <- Agg(x = dom$IN, aggkey = agg_key, dim = 2)
  dom$OUT <- Agg(x = dom$OUT, aggkey = agg_key, dim = 1)
  
  im$Z <- Agg(x =  im$Z, aggkey = agg_key, dim = 1)
  im$Z <- Agg(x =  im$Z, aggkey = agg_key, dim = 2)
  
  im$Y <- Agg(x = im$Y, aggkey = agg_key, dim = 1)
  
  
  ## Prepare more aggregated Z,Y,IN,OUT for export to eSankey
  # Note that boundary inputs and outputs are aggregated into one item each here
  
  # Create empty table to merge all tables into one
  result <- matrix( data = 0,
                    nrow = ( 2 * nrow(dom$Z) + 1 ),
                    ncol = ( ncol(dom$Z) + ncol(dom$Y) + 1 ) )
  
  # Merge transaction matrices
  result[1:(2*nrow(dom$Z)) ,1:nrow(dom$Z)] <- rbind( dom$Z, im$Z )                
  
  # Merge final demand
  result[1:(2*nrow(dom$Z)), ncol(dom$Z)+1 ] <- rbind(dom$Y,im$Y)
  
  # Add boundary inputs
  result[ nrow(result), 1:ncol(dom$Z)  ] <- colSums( dom$IN )
  
  # Add boundary outputs
  result[ 1:nrow(dom$Z), ncol(result) ] <- rowSums( dom$OUT )
  
  rownames(result) <- c( paste( "Domestic -",rownames(dom$Z) ),
                         paste( "Foreign - ",rownames(dom$Z) ),
                         "Boundary inputs" )
  
  colnames(result) <- c( colnames(dom$Z), "Final Use", "Boundary outputs" )
  
  
  ## Prepare more disaggregated final demand for export
  
  rownames(Y_disagg$dom)  <- paste( "Domestic -" ,rownames(Y_disagg$dom) ) 
  rownames(Y_disagg$im)  <- paste( "Foreign -" ,rownames(Y_disagg$im) ) 
  
  result_disagg <- rbind( Y_disagg$dom, Y_disagg$im )  # Combine in one data frame
  
  colnames(result_disagg) <- "Final Use"
  
  ## Put both results in one list for writing to xlsx
  
  out <- list("agg" = result,
              "disagg" = result_disagg,
              'disagg_names'= disagg_names)
  
  # Change units to mega tonnes
  out$agg <- out$agg / 10^6   
  out$disagg <- out$disagg / 10^6
  
  write.xlsx( out, paste0(path$output,"/",job$year,"_Data_for_World_Sankey.xlsx"), rowNames = TRUE )
  
}

Prepare_SankeyData_no_CN <- function()
{
  IO <- Build_IOT(SUT,"ixi")      # Compile IO model
  
  IO$y <- Agg(x = IO$y, rep(1:num$reg, each = 6), 2)  # Aggregate final demand
  
  # Create objects for storing results; dom = domestic tables; IM = imports
  
  dom <- list( "Z" = matrix(data = 0, nrow = num$ind, ncol = num$ind),
               "Y" = matrix(data = 0, nrow = num$ind, ncol = ncol(IO$y)/num$reg),
               "IN" = matrix(data = 0, nrow = num$va, ncol = num$ind ),
               "OUT" = matrix(data = 0, nrow = num$ind, ncol = nrow(SUT$w)) )
  
  im <- list( "Z" = matrix( data = 0, nrow = num$ind, ncol = num$ind ),
              "Y" = matrix( data = 0, nrow = num$ind, ncol = ncol(IO$y)/num$reg) )
  
  # Loop over each region and aggreate results
  
  for(m in 1:num$reg)
  {
    if(m!=17){
      # Read indices of region m
      indi <- Code$Z %>% filter(RegionCode == m, EntityCode == 1) %>%  pull(SectorIndex)
      
      # Read and store domestic table of country i and 
      dom$Z <- dom$Z + IO$Z[indi, indi]
      dom$Y <- dom$Y + IO$y[indi, m]
      dom$IN <- dom$IN + t( IO$e[indi, 1:5] )
      dom$OUT <- dom$OUT + IO$e[indi, 6:7]
      
      for(n in setdiff(1:num$reg,m) )
      {
        if(n!=17){
          # Read indices of importing region
          indi_foreign <- Code$Z %>% filter(RegionCode == n, EntityCode == 1) %>% pull(SectorIndex)
          
          # Read and store imports from region n
          im$Z <- im$Z + IO$Z[ indi_foreign, indi]
          im$Y <- im$Y + IO$y[ indi_foreign, m]
        }
      }
    }
  }
  
  
  ## Remove own-use flows (on the diagonal) for the Sankeys
  
  diag(dom$Z) <- 0
  diag(im$Z) <- 0
  
  ## Aggregating sectors into groups
  
  # For the Sankey, we need 2 different aggregations for manufacturing sectors. 
  # One is for the Z matrix (more aggregated) and one for the Y (less aggregated) matrix
  
  Y_disagg <- list( "dom" = Agg(x = dom$Y, aggkey = base$industry$Aggregate, dim = 1),
                    "im" = Agg(x = im$Y, aggkey = base$industry$Aggregate, dim = 1) )
  
  disagg_names = rownames(Y_disagg$im)
  
  agg_key <- base$industry$Aggregate  # Select predefined aggregater
  
  # Create aggregate of all manufacturing
  agg_key[ base$industry$Type %in% "Final" ] <- "Manufacturing"
  
  dom$Z <- Agg(x =  dom$Z, aggkey = agg_key, dim = 1)
  dom$Z <- Agg(x =  dom$Z, aggkey = agg_key, dim = 2)
  
  dom$Y <- Agg(x = dom$Y, aggkey = agg_key, dim = 1)
  dom$IN <- Agg(x = dom$IN, aggkey = agg_key, dim = 2)
  dom$OUT <- Agg(x = dom$OUT, aggkey = agg_key, dim = 1)
  
  im$Z <- Agg(x =  im$Z, aggkey = agg_key, dim = 1)
  im$Z <- Agg(x =  im$Z, aggkey = agg_key, dim = 2)
  
  im$Y <- Agg(x = im$Y, aggkey = agg_key, dim = 1)
  
  
  ## Prepare more aggregated Z,Y,IN,OUT for export to eSankey
  # Note that boundary inputs and outputs are aggregated into one item each here
  
  # Create empty table to merge all tables into one
  result <- matrix( data = 0,
                    nrow = ( 2 * nrow(dom$Z) + 1 ),
                    ncol = ( ncol(dom$Z) + ncol(dom$Y) + 1 ) )
  
  # Merge transaction matrices
  result[1:(2*nrow(dom$Z)) ,1:nrow(dom$Z)] <- rbind( dom$Z, im$Z )                
  
  # Merge final demand
  result[1:(2*nrow(dom$Z)), ncol(dom$Z)+1 ] <- rbind(dom$Y,im$Y)
  
  # Add boundary inputs
  result[ nrow(result), 1:ncol(dom$Z)  ] <- colSums( dom$IN )
  
  # Add boundary outputs
  result[ 1:nrow(dom$Z), ncol(result) ] <- rowSums( dom$OUT )
  
  rownames(result) <- c( paste( "Domestic -",rownames(dom$Z) ),
                         paste( "Foreign - ",rownames(dom$Z) ),
                         "Boundary inputs" )
  
  colnames(result) <- c( colnames(dom$Z), "Final Use", "Boundary outputs" )
  
  
  ## Prepare more disaggregated final demand for export
  
  rownames(Y_disagg$dom)  <- paste( "Domestic -" ,rownames(Y_disagg$dom) ) 
  rownames(Y_disagg$im)  <- paste( "Foreign -" ,rownames(Y_disagg$im) ) 
  
  result_disagg <- rbind( Y_disagg$dom, Y_disagg$im )  # Combine in one data frame
  
  colnames(result_disagg) <- "Final Use"
  
  ## Put both results in one list for writing to xlsx
  
  out <- list("agg" = result,
              "disagg" = result_disagg,
              'disagg_names'= disagg_names)
  
  # Change units to mega tonnes
  out$agg <- out$agg / 10^6   
  out$disagg <- out$disagg / 10^6
  
  write.xlsx( out, paste0(path$output,"/",job$year,"_Data_for_World-without-China_Sankey.xlsx"), rowNames = TRUE )
  
}

# sum(Z)-sum(diag(Z))#8424067574
# # sum(dom$Z)+sum(im$Z)+sum(row$Z)#7510974898
# sum(Z_r)#2977983171
# sum(Z_r)-sum(diag(Z_r))#2639699049
# sum(dom$Z)+sum(im$Z)+sum(row$Z)#2637973951
# 
# sum(Y[,17])
# sum(y_r)
# sum(dom$Y)+sum(im$Y)



# base$region
# Prepare_SankeyData_2.0()
# Prepare_SankeyData_no_CN()

