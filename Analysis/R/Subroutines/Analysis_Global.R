
Analysis_Global <- function()
{
  
  # Names for plotting, aggregation key and product codes of manufacturing
  
  prod <- list( "name" = c("Metal products",
                           "Machinery",
                           "Office & computer",
                           "Electrical machinery",
                           "Communication",
                           "Precision instruments",
                           "Motor vehicles",
                           "Transport nec",
                           "Manuf. goods nec",
                           "Construction"),
                "agg" = base$product$Aggregate[ base$product$Type == "Final"],
                "sel" = base$product %>% filter(Type == "Final") %>% pull(Code) )
  
  
  Read_Y <- function()
  {
    # Empty data frame for storing values
    df <- data.frame( "index" = 1:10,
                      "tot" = NA,
                      "dom" = NA,
                      "im" =  NA )
    
    y <- Agg( IOT$y, rep( base$region$Code, each = 6), 2 )  # Aggregate final demand categories
    
    
    
    # Loop over all manufacturing products and store values in df
    
    for(i in 1:10)
    {
      # Select manufacturing sector i (rows)
      y_sel <- y[ Code$Z %>% filter( EntityCode == 2, SectorCode == prod$sel[i] ) %>% pull(SectorIndex), ] 
      
      df$tot[ i ] <- sum( y_sel )
      df$dom[ i ] <- sum( diag( y_sel ) )
      df$im[ i ] <- df$tot[ i ] - df$dom[ i ]
    }
    
    df <- df[,-1]  # Remove indices
    
    df <- Agg( df, prod$agg, 1)  # Aggregate into groups for plotting
    
    return( as.data.frame(df) )
  }
  
  Calc_FP <- function(stressor)
  {
    # Empty data frame for storing values
    df <- data.frame( "index" = 1:10,
                      "tot" = NA,
                      "dom" = NA,
                      "im" =  NA )
    
    L <- IOT$L
    e <- IOT$e
    Z <- IOT$Z
    
    x <- rowSums(L %*% IOT$y)  # Estimate new gross production
    
    if(!stressor %in% base$input$Name)
    {
      # Read row indics for stressor
      index <- Code$Z %>% filter(EntityCode == 2, SectorName %in% stressor) %>% pull(SectorIndex)
      
      e <- colSums( Z[index,] )
      
    }else
    {
      # Otherwise take stressor from extension
      e <- e[, base$input$Code[base$input$Name %in% stressor] ] 
    }
    
    intens <-  e / x   # calculate direct intensities
    
    MP <- L * intens  # Material multipliers
    
    
    for( i in 1:length(prod$sel) )
    {
      
      y <- rowSums( IOT$y )  # Aggregate across final userr
      
      # Set all products but i to zero
      
      y[ Code$Z %>% filter( EntityCode == 2, SectorCode != prod$sel[i] ) %>% pull(SectorIndex) ] <- 0
      
      FP <- MP %*% diag( y )  # Material fooptrints
      
      FP <- Agg(FP, rep(base$region$Code, each = num$pro), 1)  # Aggregate rows into source region
      
      FP <- Agg(FP, rep(base$region$Code, each = num$pro), 2)  # Aggregate columns into consuming region
      
      df$tot[i] <- sum( FP )
      
      df$dom[i] <- sum( diag( FP ) )
      
      df$im[i] <- df$tot[i] - df$dom[i]
    }
    
    df <- df[,-1]  # Remove indices
    
    df <- Agg( df, prod$agg, 1)  # Aggregate into groups for plotting
      
    return( as.data.frame(df) )  
  }


  # Read manufacturing flows (domestic vs. trade)
  Y_trade <- Read_Y()
  
  FP_Ore <- Calc_FP("Crude Ore")
  FP_EoL <- Calc_FP("End-of-Life Scrap")
  FP_Iron <- Calc_FP("Pig iron") + Calc_FP("Sponge iron")
  FP_Scrap <- Calc_FP("Steel scrap") + Calc_FP("Forming scrap")
  
  Y_trade <- Y_trade / 10^6  # Change units to mega tonnes
  FP_Iron <- FP_Iron / 10^6
  FP_Scrap <- FP_Scrap / 10^6
  FP_Ore <- FP_Ore / 10^6
  
  
  # Create data frame for direct flows
  
  direct <- data.frame("Product" = c("Products nec", "Machinery", "Motor vehicles","Other transport", "Construction"),
                   "Domestic" = Y_trade$dom,
                   "Import" = Y_trade$im)
  
  direct$Product <- factor( direct$Product, levels = direct$Product[c(5,2,3,1,4)] )
  
  direct <- melt( direct, id.vars = "Product" )   # Transform from wide to long
  
  # Create data frame for iron ore material footprints
  
  ore <- data.frame("Product" = c("Products nec", "Machinery", "Motor vehicles","Other transport", "Construction"),
                    "Domestic" = FP_Ore$dom,
                    "Import" = FP_Ore$im)
  
  ore$Product <- factor( ore$Product, levels = ore$Product[c(5,2,3,1,4)] )
  
  ore <- melt( ore, id.vars = "Product" )   # Transform from wide to long
  
  
  # Create data frame for multipliers for indirect flows of primary iron and scrap 
  
  multiplier <- data.frame("Product" = c("Products nec", "Machinery", "Motor vehicles","Other transport", "Construction"),
                         "Iron" = FP_Iron$tot / Y_trade$tot,
                         "Scrap" = FP_Scrap$tot / Y_trade$tot )
  
  multiplier <- melt( multiplier, id.vars = "Product")   # Transform from wide to long
  multiplier$variable <- as.character(multiplier$variable)
  multiplier$variable[multiplier$variable == "Iron"] <- "Reduced iron"
  multiplier$variable <- factor(multiplier$variable, levels = c("Reduced iron", "Scrap") )
  
  
  ## Create one list for results for annex of the paper
  
  Direct_list <- cbind( "Flow" = "Final use", direct, "Unit" = "Mega  tonnes")
  Ore_list <- cbind( "Flow" = "RMC", ore, "Unit" = "Mega  tonnes" )
  Multiplier_list <- cbind( "Flow" = "Multiplier", multiplier,  "Unit" = "tonnes per tonne" )
  
  List <- rbind(Direct_list, Ore_list, Multiplier_list)
  
  colnames(List)[3:4] <- c("Variable", "Value") 
  
  write.xlsx( List, paste0(path$output,"/SI/",job$year,"_Data for Figure 7-BC.xlsx"), rowNames = FALSE )
  
  
  # Combine direct flows and upstream mutlipliers in one figure
  
  df <- cbind(ore, multiplier[,-1])
  
  colnames(df)[2:5] <- c( "source", "value_s", "Multiplier", "value_m" )
  
  remove(multiplier)
  
  v_colors <- viridis_pal()(2)  # Select colors for bars 
  
  # scale <- 2 * round( max(Y_trade$tot)/ 2 )
  
  ## 2.1. Plotting Overview chart for footprint indicators
  
  scale <- 2400   # This factor scales the secondary y-axis in a way that bot y-axis overlap in terms of grid lines (ticks)
  
  ggplot(data = df) +
    geom_bar( aes(x = Product, y = value_s, fill = source),
              stat = "identity",
              colour="grey") +
    labs(x = "\nManufactured products\n",
         y = "Raw Material Consumption (RMC) of crude iron ore [Mt]") +
    scale_fill_manual( values = v_colors,
                       name = "Origin of ore",
                       labels = c("Domestic/in-country", "International trade") ) +
    theme(legend.position = "bottom",
          legend.title = element_text(colour="black", size=14, face="bold"),
          legend.text = element_text(colour="black", size=12),
          legend.background = element_rect(colour = "grey"),
          panel.grid.minor = element_blank() ) + 
    guides( fill = guide_legend(order = 1),
            shape = guide_legend(order = 0) ) +
    geom_point(size = 5, aes(x = Product, y = ( value_m * scale ),
                   shape = Multiplier,
                   color = Multiplier) ) +
    scale_y_continuous(breaks = seq(0,scale,300),sec.axis = sec_axis(~./scale,
                                           name = "Multiplier [ton per ton]",
                                           breaks = c( .25, .5, .75, 1) ) ) +
    theme( text = element_text(size=18) ) 
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0(job$year,"_Dashboard_Global_1_.png"),
         width = 9, height = 8 )  
  
  
  # 2.2. Plot final consumption of steel
  
  ggplot(data = direct) +
    geom_bar( aes(x = Product, y = value, fill = variable),
              stat = "identity",
              colour="grey") +
    labs(x = "\nManufactured products\n",
         y = "\n[Mt]") +
    scale_fill_manual( values = v_colors,
                       name = "Origin of final product",
                       labels = c("Domestic/in-country", "International trade") ) +
    theme(legend.position = "bottom",
          legend.title = element_text(colour="black", size=14, face="bold"),
          legend.text = element_text(colour="black", size=14),
          legend.background = element_rect(colour = "grey"),
          text = element_text(size=18),
          panel.grid.minor = element_blank() ) + 
    guides( fill = guide_legend(order = 1),
            shape = guide_legend(order = 0) ) +
    scale_y_continuous(breaks = c(150, 300, 450, 600, 750 ) )
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0(job$year,"_Dashboard_Global_2_.png"),
         width = 9, height = 8 )

  
  ## Create table with all numbers for paper annex

   
}  








