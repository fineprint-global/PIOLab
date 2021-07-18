
Analysis_Y <- function(reg)
{
  
  prod_name <- c("Metal products",
                 "Machinery",
                 "Office & computer",
                 "Electrical machinery",
                 "Communication",
                 "Precision instruments",
                 "Motor vehicles",
                 "Transport nec",
                 "Manuf. goods nec",
                 "Construction")
  
  Read_Y <- function(r)
  {
    r <- base$region %>% filter(Abbrev == r) %>% pull(Code) 
    
    y <- IOT$y
    
    y <- y[ , ( (r-1)*6) + 1:6  ]  # Aggregate final demand categories
    
    # Aggregate final demand to totals
    y_total <- Agg(y, rep(1:39,num$reg), 1)
    
    y_total <- y_total[27:36,]  # Select manufacturing products
    
    # Read final demand for domestic production
    y_dom <- y[ Code$Z %>% filter( EntityCode == 2 & RegionCode ==  r ) %>% pull( SectorIndex ) , ]
    
    y_dom <- y_dom[27:36,] # Select manufacturing products
    
    y_im <- y_total - y_dom  # Calculate imports
    
    y <- data.frame("dom" = rowSums( y_dom ),
                    "tot" = rowSums( y_total ),
                    "im" =  rowSums( y_im ) ) 
    return(y)
  }
  
  Calc_FP <- function(stressor,r)
  {
    r <- base$region %>% filter(Abbrev == r) %>% pull(Code) 
    
    L <- IOT$L
    y <- IOT$y
    e <- IOT$e
    Z <- IOT$Z
    
    x <- rowSums(L %*% y)  # Estimate new gross production
    
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
    
    y <- Agg( y, rep(Code$Y$index, each = 6 ), 2 )
    
    MP <- L * intens  # Material multipliers
    
    FP <- MP %*% diag( y[,r] )  # Material fooptrints
    
    FP <- Agg(FP, rep(base$product$Code, num$reg), 2)  # Aggregate columns into final products
    
    FP <- Agg(FP, rep(base$region$Code, each = num$pro), 1)  # Aggregate rows into source region
    
    FP <- FP[,27:36]  # Select manufacturing products
    
    FP <- data.frame("tot" = colSums(FP),
                     "dom" = FP[r,],
                     "im" = colSums(FP) - FP[r,] )
    return(FP)  
  }


  y <- Read_Y( reg )
  FP_Ore <- Calc_FP("Crude Ore", reg)
  FP_EoL <- Calc_FP("End-of-Life Scrap", reg)
  FP_Iron <- Calc_FP("Pig iron", reg) + Calc_FP("Sponge iron", reg)
  FP_Scrap <- Calc_FP("Steel scrap", reg) + Calc_FP("Forming scrap", reg)
  
  df <- data.frame("Product" = prod_name,
                   "Domestic" = y$dom / 10^6,
                   "Import" = y$im / 10^6 )
  
  df <- melt( df, id.vars = "Product" )   # Transform from wide to long
  
  indirect <- data.frame("Product" = prod_name,
                         "Iron" = FP_Iron$tot / y$tot,
                         "Scrap" = FP_Scrap$tot / y$tot )
  
  indirect <- melt( indirect, id.vars = "Product")   # Transform from wide to long
  indirect$variable <- as.character(indirect$variable)
  indirect$variable[indirect$variable == "Iron"] <- "Reduced iron"
  indirect$variable <- factor(indirect$variable, levels = c("Reduced iron", "Scrap") )
  
  df$Product <- factor( df$Product, levels = prod_name)
  
  
  df <- cbind(df, indirect[,-1])
  colnames(df)[2:5] <- c( "source", "value_s", "Multiplier", "value_m" )
  
  remove(indirect)
  
  v_colors <- viridis_pal()(2)  # Select colors for bars 
  
  scale <- 2 * round( ( max(y$tot)/10^6 ) / 2 )
  
  if(reg == "CN") scale <- 200
  #scale <- 20
  
  plot <- ggplot(data = df) +
    geom_bar( aes(x = Product, y = value_s, fill = source),
              stat = "identity",
              colour="grey") +
    labs(x = "\nManufactured products",
         y = "\nApparent steel consumption [ Mega tons ]\n") +
    scale_fill_manual( values = v_colors,
                       name = "Final manufacturing",
                       labels = c("Domestic", "Foreign") ) +
    theme(legend.position = "top",
          legend.title = element_text(colour="black", size=14, face="bold"),
          legend.text = element_text(colour="black", size=12),
          legend.background = element_rect(colour = "grey") ) + 
    guides( fill = guide_legend(order = 1),
            shape = guide_legend(order = 0) )
   
  plot 
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0("Dashboard_",reg,"_1_.png"),
         width = 14, height = 7 )
  
  
  plot <- plot +
  geom_point(size = 5,
             aes(x = Product,
                 y = ( value_m * scale ),
                 shape = Multiplier,
                 color = Multiplier) ) +
    scale_y_continuous(sec.axis = sec_axis(~./scale,
                                           name = "Multiplier [ ton per ton ]\n",
                                           breaks = c( .25, .5, .75, 1) ) )
  plot
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0("Dashboard_",reg,"_2_.png"),
         width = 14, height = 7 )
 
  
  PieChart <- function(stressor,reg)
  {
    # Re-defining function to calculate footprint with source region:
    Calc_FP <- function(stressor,reg)
    {
      r <- base$region %>% filter(Abbrev == reg) %>% pull(Code)
      
      L <- IOT$L
      Z <- IOT$Z
      y <- IOT$y
      e <- IOT$e
      x <- rowSums(L %*% y)  # Estimate new gross production
      
      y <- Agg( y, rep( base$region$Code, each = 6), 2)
      
      if(!stressor %in% base$input$Name)
      {
        # Read row indics for stressor
        index <- Code$Z %>% filter(EntityCode == 2, SectorName == stressor) %>% pull(SectorIndex)
        
        e <- colSums( Z[index,] )
        
      }else
      {
        # Otherwise take stressor from extension
        e <- e[, base$input$Code[base$input$Name == stressor] ]
      }
      
      intens <-  e / x   # calculate direct intensities
      
      MP <- L * intens  # Material multipliers
      FP <- MP %*% diag( y[,r] )  # Material fooptrints
      
      FP <- Agg(FP, Code$Z %>% filter(EntityCode == 2) %>% pull(RegionCode) , 1 )  # Aggregate columns in consuming regions
      FP <- rowSums(FP)
      
      return(FP)  
    }
    
    dat <- data.frame("region" = base$region$Abbrev,
                      "value" = Calc_FP(stressor, reg) )
    
    dat$region <- as.character(dat$region)
    
    dat <- dat[order(dat$value, decreasing = TRUE),]
    
    dat_new <- dat[1:8,]
    dat_new$value[8] <- sum( dat$value[8:num$reg] )
    dat_new$region[8] <- "RoW"
    
    dat <- dat_new
    remove(dat_new)
    
    dat$region <- factor( dat$region, levels = dat$region)
    
    v_colors <- viridis_pal()(8)  # Select colors for bars 
    
    # For printing the total
    value_tot <- round( sum( dat$value ) / 10^6, digits = 0)
    
    dat$value <- round( 100 * dat$value / sum(dat$value), digits = 1)
    
    dat <- dat %>% mutate(posit = cumsum(value) - 0.5*value)
    
    plot <- ggplot(dat , aes(x="", y=value, fill = region)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual( values = v_colors,
                         name = paste0("Source regions of \n",
                                       stressor," footprint ",reg,":" ) ) +
      theme(axis.text.x=element_blank()) + 
      theme_void() +
      geom_text( aes(label = paste0(value,"%") ),
                 position = position_stack(vjust = 0.5),
                 color = "white")
    
    print( paste(stressor,"Footprint source region shares for",reg) )
    print(dat)
    print( paste(stressor,"Footprint of",reg,":",value_tot,"Mt") )
    
    return(plot)
  }
    
  stressor <- "Crude Ore"
  
  pie <- PieChart(stressor,reg) 
  
  pie 
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0("PieChart_",stressor,"_",reg,".png"),
         width = 10, height = 7 )
  
  ## Combine the two plots
  
  ggarrange(plotlist = list(plot,pie),
            ncol = 1,
            nrow = 2,
            heights = c(8,4),
            widths = 12 )
  
  # Write plot to file:
  ggsave(path = path$output,
         filename = "Dashboard_US.png",
         width = 14, height = 14)
  
   
}  








