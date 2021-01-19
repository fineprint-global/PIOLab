

Plot_Rocket <- function() 
{
  
  ### 0. Set paths and select codes ###
  
  Setting <- list( "WSA" = read.xlsx(xlsxFile = paste0(path$settings,"/datafeeds_settings/WSA_settings.xlsx"), sheet = 1 ),
                   "IE" = read.xlsx(xlsxFile = paste0(path$settings,"/Base/IE_settings.xlsx"), sheet = 1 ))
  
  Setting$WSA["path"] <- paste0(path$input,"WorldSteel/WSA_",job$year,"_",Setting$WSA$FeedName,".csv")
  
  # Read WSA id's of processed data that is relevant:
  Code_sel <- Setting$WSA %>% filter(Type %in% c("Primary","Secondary","Finished")) %>% 
    select( id, FeedName, Type )
  
  ### 1. Load raw data and final results for all regions ###
  
  if( job$year > 2014 )
  {
    data <- list( "BACI" =  read.csv( paste0(path$input,"BACI/BACI_",job$year,".csv") ),
                  "WSA" = lapply( Setting$WSA$path, FUN = read.csv) )
  }else
  {
    if(job$year > 2008)
    {
      data <- list( "BACI" =  read.csv( paste0(path$input,"BACI/BACI_",job$year,".csv") ),
                    "IRP" = read.csv(paste0(path$input,"UNEP-IRP/IRP_",job$year,".csv") ),
                    "WSA" = lapply( Setting$WSA$path, FUN = read.csv) )  
    }else
    {
      data <- list( "BACI" =  read.csv( paste0(path$input,"BACI/BACI_",job$year,".csv") ),
                  "IRP" = read.csv(paste0(path$input,"UNEP-IRP/IRP_",job$year,".csv") ),
                  "WSA" = lapply( Setting$WSA$path, FUN = read.csv),
                  "Eol" = read.csv(paste0(path$input,"PauliukEoL/EOL_",job$year,".csv") ) )
    }
    
  }
  
  
  # Read and aggregate final tables across industries for each region 
  S <- SUT$S
  S <- Agg( S, rep(1:num$reg,each = 30), 1 )
  S <- Agg( S, rep(1:39,num$reg), 2 )
  
  v <- SUT$v
  v <- Agg( v, rep(1:num$reg,each = 30), 2 )
  
  U <- SUT$U
  U <- Agg( U, rep(1:num$reg, each = 30), 2 )
  
  ### 2. Select relevant products and make comparison ###
  
  ## 2.1. BACI
  
  # Create new (dummy) object for comparison data
  dummy <- data$BACI[,c("From","Product","To","Quantity")]   
  
  dummy <- dummy %>% filter(From %in% 1:34, To %in% 1:34 )
  
  colnames(dummy) <- c("From.Reg","From.Pro","To","True")
  
  dummy[c("Realized","From")] <- NA
  
  dummy$From <- ( (dummy$From.Reg - 1) * num$pro ) + dummy$From.Pro
  
  dummy$Realized <- U[ as.matrix( dummy[,c("From","To")] ) ]
  
  # Add other variables
  dummy <- data.frame( "RegionCode" = dummy$From.Reg,
                       "RegionName" = base$region$Abbrev[ dummy$From.Reg ] ,
                       "FlowCode" = dummy$From.Pro,
                       "FlowName" = base$product$Name[ dummy$From.Pro ],
                       "FlowLabel" = "BACI trade data",
                       "True" = dummy$True,
                       "Realized" = dummy$Realized )
  
  # Add to object
  Comp <- dummy
  
  ## 2.2. WSA accounts
  
  # Read base codes of relevant WSA feeds
  base_sel <- inner_join(base$product,Code_sel, by =c("feed" = "FeedName") )
  
  # Use id to extract a list containing relevant WSA data:
  data_sel <- data$WSA[base_sel$id]
  
  # Read and compare WSA flows
  
  for( i in 1:nrow(base_sel) )
  {
    # Create empty data frame
    dummy <- data.frame( "RegionCode" = 1:num$reg,
                         "RegionName" = base$region$Abbrev,
                         "FlowCode" = NA,
                         "FlowName" = NA,
                         "FlowLabel" = NA,
                         "True" = 0,
                         "Realized" = 0 )
    
    # Add product code and name
    dummy$FlowCode <- base_sel$Code[i]   
    dummy$FlowName <- base_sel$Name[i]
    dummy$FlowLabel <- "Other data sources"
    
    # Write original/true values
    dummy$True[data_sel[[i]]$base] <- data_sel[[i]]$Quantity
    # Write realized value
    dummy$Realized <- S[, base_sel$Code[i] ]
    
    Comp <- rbind(Comp,dummy)
  }
  
  
  ## 2.3. IRP iron ore extraction
  
  if(job$year <= 2014)
  {
    # Create empty data frame
    dummy <- data.frame( "RegionCode" = 1:num$reg,
                         "RegionName" = base$region$Abbrev,
                         "FlowCode" = NA,
                         "FlowName" = NA,
                         "FlowLabel" = NA,
                         "True" = 0,
                         "Realized" = 0 )
    
    # Read true values
    dummy$True[data$IRP$base] <- data$IRP$Quantity
    # Read realized values
    dummy$Realized <- v[base$input %>% filter(Name == "Crude Ore") %>% pull(Code), ]
    # Set label:
    dummy$FlowLabel <- "Other data sources"
    
    # Add to object
    Comp <- rbind(Comp,dummy)
  }
  
  
  ## 2.4. EoL Scrap 
  
  if(job$year <= 2008)
  {
    # Create empty data frame
    dummy <- data.frame( "RegionCode" = 1:num$reg,
                         "RegionName" = base$region$Abbrev,
                         "FlowCode" = NA,
                         "FlowName" = NA,
                         "FlowLabel" = NA,
                         "True" = 0,
                         "Realized" = 0 )
    
    # Read true values
    dummy$True[data$Eol$base] <- data$Eol$Quantity
    # Read realized values
    dummy$Realized <- v[base$input %>% filter(Name == "End-of-Life Scrap") %>% pull(Code), ]
    # Set label:
    dummy$FlowLabel <- "Other data sources"
    
    # Add to object
    Comp <- rbind(Comp,dummy)
  }
  
  # Set factor levels for showing legend in the right order
  Comp$FlowLabel <- factor(Comp$FlowLabel, levels = c("BACI trade data",
                                                      "Other data sources") )
  
  
  Comp <- Comp[Comp$True != 0, ]
  
  # v_colors <- viridis_pal()(2)  # Select colors from viridis for plotting
  v_colors <- c( "#DCE319FF","#404788FF")
  
  df <- Comp
  
  df$True <- log(df$True, base = 10)
  df$Realized <- log(df$Realized, base = 10)
    
  df$True[df$True < 0] <- 0 
  df$Realized[df$Realized < 0] <- 0 
  df$True[df$True == Inf] <- 0
  df$Realized[df$Realized == Inf] <- 0
    
  plot <- ggplot( data = df, aes(x =  True, y = Realized, color = FlowLabel) ) +
    geom_point( alpha = 0.8, size=0.8) +
    scale_color_manual( values = v_colors ) +
    scale_x_continuous( expand = c(0,0),
                        limits = c(1,9),
                        breaks = 1:9) + 
    scale_y_continuous( expand = c(0,0),
                        limits = c(1,9),
                        breaks = 1:9 ) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, alpha = 0.5) + 
    theme_gray() +
    theme(legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'top') +
    labs(x = "Reported value (log10 tonnes)",
         y = "Realized value (log10 tonnes)") +
    geom_density2d(bins = 4,
                   show.legend = FALSE,
                   contour = TRUE,
                   color = 'dark grey')
    
  plot 
  
  # Save plot: 
  ggsave(path = paste0(path$output), 
         filename = paste0(job$year,"_RocketPlot.png"),
         width = 6, height = 6.5 )
  
  # Export data for SI of the paper
  write.xlsx(x = Comp, file = paste0(path$SI,"/",job$year,"_Data for rocket plot.xlsx") )
  
  return(plot)  

}


