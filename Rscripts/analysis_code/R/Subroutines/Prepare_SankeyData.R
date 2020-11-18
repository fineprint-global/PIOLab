################################################################################
# This function aggregates the global PSUTs for drawing Sankeys in eSankey

Prepare_SankeyData <- function()
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
  
  
  ## Remove own-use flows (on the diagonal) fpr the Sankeys
  
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
  
  write.xlsx( out, paste0(path$output,"/SI/Data for Sankey.xlsx"), rowNames = TRUE )
  
}

