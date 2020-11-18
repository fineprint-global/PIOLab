

Plot_Rocket <- function() 
{
  
  ### 0. Set paths and select codes ###
  
  Setting <- list( "WSA" = read.xlsx(xlsxFile = paste0(path$settings,"/datafeeds_settings/WSA_settings.xlsx"), sheet = 1 ),
                   "IE" = read.xlsx(xlsxFile = paste0(path$settings,"/Base/IE_settings.xlsx"), sheet = 1 ))
  
  Setting$WSA["path"] <- paste0(path$IE_processed,"/WSA/WSA_",job$year,"_",Setting$WSA$FeedName,".csv")
  
  # Read WSA id's of processed data that is relevant:
  Code_sel <- Setting$WSA %>% filter(Type %in% c("Primary","Secondary","Finished")) %>% 
    select( id, FeedName, Type )
  
  ### 1. Load raw data and final results for all regions ###
  
  data <- list( "FinalDemand" =  read.csv( paste0(path$IE_processed,"/EXIOWasteMFAIO/",job$year,"_","FinalDemand.csv") ),
                "Fab2Demand" = read.csv( paste0(path$IE_processed,"/EXIOWasteMFAIO/",job$year,"_","Fabrication2FinalDemand.csv") ),
                "BACI" =  read.csv(paste0(path$IE_processed,"/BACI/BACI_",job$year,".csv") ),
                "Eol" = read.csv(paste0(path$IE_processed,"/EOL/EOL_",job$year,".csv") ),
                "IRP" = read.csv(paste0(path$IE_processed,"/IRP/IRP_",job$year,".csv") ),
                "Grade" = read.csv(paste0(path$IE_processed,"/Grades/IronOreGrades.csv") ),
                "Yield" = select(read.csv(paste0(path$IE_processed,"/WSA/SteelIndustryYields.csv")),Process,Average),
                "WSA" = lapply(X = Setting$WSA$path, FUN = read.csv),
                "Forgings" = read.csv(paste0(path$IE_processed,"/AligningData/Forgings_",job$year,".csv") ) 
                )
  
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
                       "FlowLabel" = "Trade (BACI)",
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
    dummy$FlowLabel <- paste(base_sel$Type_2[i],"(WSA)")
    
    # Write original/true values
    dummy$True[data_sel[[i]]$base] <- data_sel[[i]]$Quantity
    # Write realized value
    dummy$Realized <- S[, base_sel$Code[i] ]
    
    Comp <- rbind(Comp,dummy)
  }
  
  
  ## 2.3. IRP iron ore extraction
  
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
  dummy$FlowLabel <- "Crude ore (IRP)"
  
  # Add to object
  Comp <- rbind(Comp,dummy)
  
  
  ## 2.4. EoL Scrap 
  
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
  dummy$FlowLabel <- "EoL-Scrap (Pauliuk)"
  
  # Add to object
  Comp <- rbind(Comp,dummy)
  
  # Set factor levels for showing legend in the right order
  Comp$FlowLabel <- factor(Comp$FlowLabel, levels = c("Crude ore (IRP)",
                                                      "Reduced iron (WSA)",
                                                      "Liquid steel (WSA)",
                                                      "Castings (WSA)",
                                                      "Finished steel (WSA)",
                                                      "EoL-Scrap (Pauliuk)",
                                                      "Trade (BACI)")
                           )
  
  
  Comp <- Comp[Comp$True != 0, ]
  
  # Create function for plotting
  
  plot <- function(df)
  {
    df$True <- log(df$True, base = 10)
    df$Realized <- log(df$Realized, base = 10)
    
    df$True[df$True < 0] <- 0 
    df$Realized[df$Realized < 0] <- 0 
    df$True[df$True == Inf] <- 0
    df$Realized[df$Realized == Inf] <- 0
    
    x <- ggplot( data = df, aes(x = True , y = Realized) ) +
      geom_point( aes(color = FlowLabel),alpha = 0.8, size=0.8) +
      scale_x_continuous( expand = c(0,0),
                          limits = c(1,9),
                          breaks = 1:9) + 
      scale_y_continuous( expand = c(0,0),
                          limits = c(1,9),
                          breaks = 1:9 ) +
      scale_color_viridis_d(begin = 0, end = 0.95) +
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
    
    return(x)
  }
  
  # Plot overview 
  p <- plot(Comp)
  
  p
  
  # Save plot: 
  ggsave(path = paste0(path$output), 
         filename = paste0("RocketPlot.png"),
         width = 6, height = 6.5 )
  
  # Export data for SI of the paper
  write.xlsx(x = Comp, file = paste0(path$output,"/SI/Data for rocket plot.xlsx") )
  
  
  return(p)

}


