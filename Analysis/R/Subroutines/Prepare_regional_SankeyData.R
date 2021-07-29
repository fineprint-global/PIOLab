################################################################################
# This function aggregates the global PSUTs for drawing Sankeys in eSankey

Prepare_regional_SankeyData <- function(r)
{
  
  # r contains a string selecting regions (either single country or a country group, e.g. Europe)
  
  # For test purposes and code development, we select China for now
  r <- "China"
  
  # Please implement code so that we can also create sankeys for country groups (e.g. Europe)
  # See base$region
  
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
  e <- Agg(x = e, Code$V$Boundary, 2)
  
  # Check that total inputs (almost) equal total outputs
  sum(e[,1])   # Boundary inputs (BI)
  sum(e[,2],Y) # Boundary outputs (BO)
  
  # Create direct intensities 
  f <- e/x
  f[is.na(f)] <- 0   # Remove NA just in case
  colnames(f)
  
  ### Now the compilation of the region-specfic flow matrix starts ###
  
  # Select final demand of region r
  y_r <- Y[, Code$Y$index[ Code$Y$RegionName == r ] ]
  
  # Calculate gross output reflecting only flows associated with Y of r
  x_r <- as.vector( L %*% y_r )
  
  # Use A to estimate the new flow matrix Z, now only flows associated with y of r
  Z_r <- t( t(A) * x_r )
  
  # For checking the results, export matrix Z_r to xlsx file
  write.xlsx(Z_r, file = paste0( path$output,"/Z_regional_sankey_",r,"_fulldetail.xlsx"), colnames = TRUE)
  
  # For testing the reuslt, aggregate the flow matrix Z and write it to xlsx file
  Z_r_test <- Agg(Z_r, aggkey = Code$Z$SectorName[Code$Z$EntityCode == 1], 1)
  Z_r_test <- Agg(Z_r_test, aggkey = Code$Z$SectorName[Code$Z$EntityCode == 1], 2)
  write.xlsx(Z_r_test, file = paste0( path$output,"/Z_regional_sankey_",r,"_aggregated.xlsx"), colnames = TRUE)
  
  # Calculate new BI & BO, stored in object B_r now reflecting only flows associated with Y of r
  B_r <- f * x_r
  colnames(B_r)
  
  # Check that total inputs and outputs, that are associates with r, balance out
  sum(B_r[,1])
  sum(B_r[,2],y_r)
  
  # Create data frame to check process balances
  bal <- data.frame(Code$Z[ Code$Z$EntityCode == 1,],
                    "Intermediate_input" = colSums(Z_r),
                    "Boundary_input" = B_r[,1],
                    "Intermediate_output" = rowSums(Z_r),
                    "Boundary_output" = B_r[,2],
                    "Final_use" = y_r,
                    stringsAsFactors = FALSE) 
  
  bal["Total_input"] <- bal$Intermediate_input + bal$Boundary_input
  bal["Total_output"] <- bal$Intermediate_output + bal$Boundary_output + bal$Final_use
  
  write.xlsx(bal, file = paste0( path$output,"/Process_balances_regional_sankey_",r,".xlsx"), colnames = TRUE)
  

  
  
  
  
  
  
  
  
    

  ### Creating aggregated data set for sankey ###
  
  # Create objects for storing sankey data; dom = domestic tables; IM = imports
  
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
              "disagg" = result_disagg)
  
  # Change units to mega tonnes
  out$agg <- out$agg / 10^6   
  out$disagg <- out$disagg / 10^6
  
  write.xlsx( out, paste0(path$output,"/",job$year,"_Data_for_",r,"_Sankey.xlsx"), rowNames = TRUE )
  
}

