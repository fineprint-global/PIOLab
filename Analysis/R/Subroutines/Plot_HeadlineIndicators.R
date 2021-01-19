


Plot_HeadlineIndicators <- function()
{
  # If the IOT is industry by industry, select industry codes:
  if(nrow(IOT$Z) == ncol(IOT$Z) & nrow(IOT$Z) == ( num$ind * num$reg)) sec_sel <- "Industry"
  
  # If the IOT is product by product, select product codes:
  if(nrow(IOT$Z) == ncol(IOT$Z) & nrow(IOT$Z) == ( num$pro * num$reg)) sec_sel <- "Product"
  
  Code_sel <- Code$Z$RegionCode[Code$Z$EntityName == sec_sel]  # Select codes
  
  # Load and aggregate population data
  pop <- read.xlsx(paste0(path$repo,"/input/EXIOBASE/EXIOBASE population data.xlsx"),sheet = 3) %>%
    select(EXIOcode,as.character(job$year)) 
  
  pop <- pop[1:49,] # Clean pop data
  
  # Load IRP raw data:
  
  Rawdata <- data.frame("DE" = 0,
                        "IM" = read.csv(paste0(path$input,"UNEP-IRP/imports_",job$year,".csv"),header = FALSE),
                        "EX" = read.csv(paste0(path$input,"UNEP-IRP/exports_",job$year,".csv"),header = FALSE) )
  
  tmp <- read.csv(paste0(path$input,"UNEP-IRP/IRP_",job$year,".csv"),header = TRUE)
  Rawdata$DE[tmp$base] <- tmp$Quantity
  
  colnames(Rawdata) <- c("DE","IM","EX")
  
  Rawdata["DMC"] <- Rawdata$DE + Rawdata$IM - Rawdata$EX  # Calculate DMC
  
  # Load root-2-mother concordance and aggregate IRP data to mother classification:
  
  R2M <- read.csv( paste0( path$concordance,"/Region Aggregators/",job$RegAgg,"_RegionAggregator.csv"), header = FALSE )
  R2M <- as.matrix(R2M)  # Transform into matrix
  
  R2M_trans <- function(x)
  {
    y <- colSums( x %*% R2M )
    return(y)
  }
  
  Rawdata["PTB"] <- Rawdata$IM - Rawdata$EX   # Calculate physical trade balance
  
  ## Load WSA data in mother classification:
  
  # Read WSA feed names
  WSA <- list("Settings" = read.xlsx(xlsxFile = paste0(path$settings,"/datafeeds_settings/WSA_settings.xlsx"), sheet = 1 ) )
  
  WSA[["path"]] <- paste0(path$input,"WorldSteel/WSA_",job$year,"_",WSA$Setting$FeedName,".csv")
  
  WSA[["data"]] <- lapply(X = WSA$path, FUN = read.csv, header = TRUE)
  
  Steel <- as.data.frame( matrix(0, nrow = num$reg, ncol = 2) )
  colnames(Steel) <- c("BOF", "EAF")  
  
  Steel$BOF[ WSA$data[[4]]$base ] <- WSA$data[[4]]$Quantity
  Steel$EAF[ WSA$data[[5]]$base ] <- WSA$data[[5]]$Quantity
  
  Rawdata["BOF"] <- Steel$BOF
  Rawdata["EAF"] <- Steel$EAF
  
  ## Load Pauliuk EoL scrap data in mother classification:
  
  if(job$year <= 2008)
  {
    EoL <- read.csv( paste0(path$input,"/PauliukEoL/EOL_",job$year,".csv") )
    
    Rawdata[EoL$base,"EoL"] <- EoL$Quantity
    
    Rawdata[is.na(Rawdata)] <- 0  # In case a raw data source has NA
    
  }else
  {
    Rawdata[1:num$reg,"EoL"] <- 0
  }
  
  # Check for negative values in DMC (e.g. Hungary for 2008 cause EX is bigger than IM):
  
  print("Negative DMC results in UNEP-IRP database:")
  print(Rawdata$DMC[Rawdata$DMC < 0])
  
  # Rawdata$DMC[Rawdata$DMC < 0] <- 0  # Set to zero
  
  IOdata <- Calc_ewMFA(IOT,Code)  # ewMFA indicators from IOT
  
  FP_exio <- Calc_EXIOfootprint()   # Calculate flows of embodied iron ore
  
  Rawdata["RMC"] <- colSums(FP_exio)  # Calculate RMC
  Rawdata["IM_RME"] <- Rawdata$RMC - diag(FP_exio)  # Calculate RME of imports
  Rawdata["EX_RME"] <- rowSums(FP_exio) - diag(FP_exio)  # Calculate RME of exports
  Rawdata["RTB"] <- Rawdata$IM_RME - Rawdata$EX_RME  # Calculate raw material trade balance
  
  
  # Load PIOT variables:
  
  L <- IOT$L
  y <- IOT$y
  e <- IOT$e
  x <- rowSums(L %*% y)  # Estimate new gross production
  intens <- e[, base$input$Code[base$input$Name == "Crude Ore"] ]/x
  
  MP <- L * intens  # Material multipliers
  FP_piot <- MP %*% y  # Material fooptrints
  
  FP_piot <- Agg(FP_piot,Code_sel,1)  # Aggregate rows into source regions
  FP_piot <- Agg(FP_piot, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
  
  IOdata["RMC"] <- colSums(FP_piot)
  
  IOdata["PTB"] <- IOdata$Import - IOdata$Export
  
  IOdata["IM_RME"] <- colSums(FP_piot) - diag(FP_piot)
  
  IOdata["EX_RME"] <- rowSums(FP_piot) - diag(FP_piot)
  
  IOdata["RTB"] <- IOdata$IM_RME - IOdata$EX_RME
  
  
  
  ## Read production values from IOT
  
  # First BOF
  Code_sel <- Code$Z %>% filter( EntityCode == 2, SectorName == "Liquid steel OBF" ) %>%
    select(SectorIndex,RegionCode)
  
  Steel <- SUT$S[,Code_sel$SectorIndex]
  Steel <- colSums(Agg(Steel, Code_sel$RegionCode, 2))
  
  IOdata["BOF"] <- Steel
  
  # Second EAF
  Code_sel <- Code$Z %>% filter( EntityCode == 2, SectorName == "Liquid steel EAF" ) %>%
    select(SectorIndex,RegionCode)
  
  Steel <- SUT$S[,Code_sel$SectorIndex]
  Steel <- colSums(Agg(Steel, Code_sel$RegionCode, 2))
  
  IOdata["EAF"] <- Steel
  
  
  # Third EoL Scrap
  Code_sel <- Code$Z %>% filter( EntityCode == 1, SectorName == "Scrap preparation" ) %>%
    select(SectorIndex,RegionCode)
  
  EoL <- SUT$v[Code$V %>% filter(Entity == "End-of-Life Scrap") %>% pull(index) ,Code_sel$SectorIndex]
  
  IOdata["EoL"] <- EoL
  
  
  ##############################################################################
  
  ### 0. Preparing data and functions ### 
  
  # Function for plotting comparisons:
  
  Bars <- function()
  {
    # Transform wide to long
    dat_agg <- melt( dat_agg, id.vars = c("Region") )                
    
    v_colors <- viridis_pal()(2)  # Define 
    
    plot <- ggplot(data = dat_agg) +
      geom_bar(stat = "identity",
               aes(x = Region,y = value, fill = variable),
               position = "dodge",
               colour="grey",
               width = .7) +
      theme_gray() +
      scale_y_continuous(expand=c(0,0)) + 
      labs(x="Region", y = "Mega tonnes") +
      scale_fill_manual(values = v_colors, name = "Dataset", labels = c(x_lab, y_lab)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.7),
            plot.title =  element_text(hjust = 0.5, face = "bold", size = 19),
            legend.position = "top",
            text = element_text(size=14) )
    
    return(plot)
  }
  
  Scatter <- function()
  {
    
    scatter <- ggplot( data = dat, aes(x = X, y = Y, color = Region ) ) +
      geom_point(alpha = 0.8, size=4, aes(shape = Region) ) +
      scale_shape_manual(values = world_scatter$symbol ) +
      scale_color_manual(values = world_scatter$color) +
      scale_x_continuous( expand = c(intersec,intersec),
                          limits = c(lim[1],lim[2]),
                          breaks = lim[1]:lim[2]) + 
      scale_y_continuous( expand = c(intersec,intersec),
                          limits = c(lim[1],lim[2]),
                          breaks = lim[1]:lim[2] ) +
      labs( x= paste0("",x_lab," [log10 tonnes]"),
            y = paste0(y_lab," [log10 tonnes]") ) +
      theme( panel.grid.minor = element_blank(), 
             legend.position = "top",
             text = element_text(size=14),
             legend.text=element_text(size=14),
             legend.title=element_blank(),
             legend.background = element_rect(fill="gray90", size=.5) ) +
      geom_abline(intercept = 0.15, slope = 1, linetype = 3, alpha = 0.5) + 
      geom_abline(intercept = -0.15, slope = 1, linetype = 3, alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, linetype = 2, alpha = 0.5)
    
    return(scatter)
    
  }
  
  
  ## Selecting indicators and data 
  
  indi_short <- c("DE","DMC","RMC","BOF","EAF","EoL")
  
  indi_long <- c("Domestic Extraction",
                 "Domestic Material Consumption",
                 "Raw Material Consumption",
                 "Steel from Basic Oxygen Furnace",
                 "Steel from Electric Arc Furnace",
                 "End-of-Life Scrap")
  
  indi_add <- c("of crude iron ore", "of ferrous materials", "of crude iron ore","","","")
  
  Raw <- Rawdata[,indi_short]
  PIOLab <- IOdata[,indi_short]
  
  labels <- c("UNEP-IRP database", "UNEP-IRP database","EXIOBASE MRIO","World Steel","World Steel","Pauliuk")  # Labels of data sources
  
  limits <- list( c(5,9.2), c(6,9.2), c(6,9.2), c(5,9), c(5,8), c(5,8) )  # Limits of the scales in the scatter plot
  
  y_lab <<- "PIOLab"  # Label of the realized values
  
  # Empty list objects to store plots for further merging into one overview plot
  bar_plots  <- vector( "list", length = length(indi_short) )
  scatter_plots  <- vector( "list", length = length(indi_short) )
  
  # Simplify Asia and Pacific label for better readability in plot
  world_regions <- base$region$Region
  world_regions[world_regions == 'Asia and Pacific'] <- 'Asia/Pacific' 
  
  # Data frame to consistently link world regions to specific symbols and colors in the scatter plot
  world_scatter <<- data.frame("index" = 1:6,
                               "name" = unique(world_regions)[c(5,1,4,3,2,6)],
                               "symbol" = 15:20,
                               "color" = viridis_pal()(6),
                               stringsAsFactors = FALSE)
  
  
  ### Create csv file with data of Figure x for SI
  
  # Merge data PIOLab data with other data for copmparison
  SI <- cbind( Rawdata[, c("DE","DMC","RMC")], IOdata[,c("DE","DMC","RMC")] )
  
  colnames(SI) <- paste( colnames(SI), c("(UNEP-IRP)", "(UNEP-IRP)", "(EXIOBASE)", rep("(PIOLab)",3)) ) 
  
  SI <- SI[,c(1,4,2,5,3,6)]   # Rearrange for better presentation
  SI <- cbind( base$region, SI )  # Add labels
  
  write.csv(SI, file = paste0(path$SI,"/",job$year,"_ew-MFA Headline Indicators.csv" ), row.names = FALSE )  
  
  
  ## Create csv file with trade data for SI
  
  SI <- cbind( Rawdata[, c("IM","EX","IM_RME","EX_RME")], IOdata[,c("Import","Export","IM_RME","EX_RME")] )
  
  colnames(SI) <- c("Physical imports (UNEP-IRP)", 
                    "Physical exports (UNEP-IRP)",
                    "Raw material equivalent of imports (EXIOBASE)",
                    "Raw material equivalent of exports (EXIOBASE)",
                    "Physical imports (PIOLab)", 
                    "Physical exports (PIOLab)",
                    "Raw material equivalent of imports (PIOLab)",
                    "Raw material equivalent of exports (PIOLab)" ) 
  
  SI <- SI[,c(1,5,2,6,3,7,4,8)]   # Rearrange for better presentation
  SI <- cbind( base$region, SI )  # Add labels
  
  write.csv(SI, file = paste0(path$SI,"/",job$year,"_Trade flow data SI.csv" ), row.names = FALSE )  
  
  
  
  for( i in 1:length(indi_short) )
  {
    
    # Write both reported and realized data in one data frame and scale if required
    dat <- data.frame("Region" = world_regions,
                       "X" = Raw[,i],
                       "Y" = PIOLab[,i],
                       stringsAsFactors = FALSE)
    
    x_lab <- labels[i]  # Read label of comparison data set
    
    
    ## 1.1. Bar plot of aggregates ##
    
    # Aggregate into world regions for plotting bars
    dat_agg <- dat %>% group_by(Region) %>% summarise(X = sum(X), Y = sum(Y)) %>% ungroup(Region)
    
    # Sort descending and use factors for setting order in plots 
    dat_agg <- dat_agg[order(-dat_agg$Y),]   
    dat$Region <- factor(dat$Region, levels = dat_agg$Region)
    dat_agg$Region <- factor(dat_agg$Region, levels = dat_agg$Region)
    
    # Scaling factor for bar plot
    s <- 10^6  
    dat_agg[,c("X","Y")] <- dat_agg[,c("X","Y")] / s  
    
    bar_plots[[i]] <- Bars()
    
    Bars()
    
    ShortName <- paste0(indi_short[i],"_Abs_Bars")  # Define name for plot file
    
    # Write plot to file: 
    ggsave(path = path$output, 
           filename = paste0(job$year,"_",ShortName,".png"),
           width = 6, height = 6 )
    
    ## 1.2. Scatter plot of aggregates ##
    
    dat <- dat[dat$X > 0,]  # Remove all entries that are zero
    
    # Create logarithm here instead of in the plot function
    dat$X <- log10( dat$X )  
    dat$Y <- log10( dat$Y )
    
    dat$Region <- factor(dat$Region, levels = world_scatter$name)
    
    intersec <- 0    # Define set-off i.e. difference between plot area and axis
    lim <- limits[[i]]    # Limits of scales
    
    scatter_plots[[i]] <- Scatter()   # Produce scatter plot
    
    Scatter()
    
    ShortName <- paste0(indi_short[i],"_Abs_Scatter")  # Define name for plot file
    
    # Write plot to file: 
    ggsave(path = path$output, 
           filename = paste0(job$year,"_",ShortName,".png"),
           width = 6, height = 6 )
  }
  
  
  # Remove some labels from the bars for better overview plot
  for( i in 1:length(indi_short) )
  {
    bar_plots[[i]] <- bar_plots[[i]] + 
      theme( axis.title.x = element_blank(),
             legend.title = element_blank() ) +
      ggtitle( paste0( indi_short[i],"\n(",indi_long[i],")\n",indi_add[i],"\n" ) )
  }
  
  
  # Object for all plots, including a missing object for adjusting spacing
  overview <- vector(mode = "list", length = 5)
  
  # Create overview of MFA headline indicators using bars
  overview[[1]] <- ggarrange(plotlist = bar_plots[1:3],
                             ncol = 3,
                             nrow = 1 )
  
  # Overview of MFA headline indicators using scatter plots
  overview[[2]] <- ggarrange(plotlist = scatter_plots[1:3],
                             ncol = 3,
                             nrow = 1,
                             common.legend = TRUE,
                             legend = "bottom")
  
  # Create overview of source data using bars
  overview[[4]] <- ggarrange(plotlist = bar_plots[4:6],
                             ncol = 3,
                             nrow = 1 )
  
  # Overview of source data using scatter plots
  overview[[5]] <- ggarrange(plotlist = scatter_plots[4:6],
                             ncol = 3,
                             nrow = 1,
                             common.legend = TRUE,
                             legend = "bottom")
  
  space <- .7   # Set space between upper and lower group of plots
  
  ggarrange(plotlist = overview,
            ncol = 1,
            nrow = 5,
            heights = c(6,5,space,6,5) )
  
  
  # Write plot to file:
  ggsave(path = path$output,
         filename = paste0(job$year,"_Overview.png"),
         width = 15, height = 22 + space )
  
  
  
  # Create function for compiling trade balances
  
  Plot_TradeBalances <- function(IO, Raw, LongName, ShortName, Yaxis, scale, label)
  {
    Comp <- data.frame(base$region$Code,base$region$Abbrev,IO,Raw)
    
    colnames(Comp) <- c("Code","Name","PIOLab",label)
    
    Comp[,c("PIOLab",label)] <- Comp[,c("PIOLab",label)]/scale  # Change to Mt
    
    Comp$Name <- factor(Comp$Name, levels = unique(Comp$Name) )
    
    Comp <- melt(Comp,id.vars = c("Code","Name"))  # Transform wide to long
    
    v_colors <- viridis_pal()(2)  # Define 
    
    plot <- ggplot(data = Comp) +
      geom_bar(stat = "identity",
               aes(x = Name,y = value, fill = variable),
               position = "dodge",
               colour="grey") +
      theme_gray() +
      scale_y_continuous(expand=c(0,0)) +
      labs(title = LongName, x="Region", y = Yaxis) +
      scale_fill_manual(values = v_colors,name = "", labels = c("PIOLab", label))
    
    plot
    
    # Save plot: 
    ggsave(path = path$output, 
           filename = paste0(job$year,"_",ShortName,".png"),
           width = 14, height = 7 )
    
    return(plot)
  }
  
  
  # Physical Trade Balances:
  
  global_abs <- round( sum(IOdata$Import) / 10^6, digits = 0 )
  LongName <- paste("Physical Trade Balances of",num$reg,"regions\n2008 global trade volume: PIOLab:",global_abs,"Mt")
  ShortName <- "PTB"
  Yaxis <- "Million metric tonnes [Mt]"
  scale <- 10^6
  IO <- IOdata$PTB
  Raw <- Rawdata$PTB
  label <- "UNEP-IRP account"
  
  Plot_TradeBalances(IO,Raw,LongName,ShortName,Yaxis,scale,label)
  
  
  # Raw material trade balance
  
  global_abs <- round( sum(Rawdata$IM_RME) / 10^6, digits = 0 )
  LongName <- paste("Raw Material Trade Balances of",num$reg,"regions\n2008 global trade volume: PIOLab:",global_abs,"Mt")
  ShortName <- "RTB"
  Yaxis <- "Million metric tonnes [Mt]"
  scale <- 10^6
  IO <- IOdata$RTB
  Raw <- Rawdata$RTB
  label <- "EXIOBASE"
  
  Plot_TradeBalances(IO,Raw,LongName,ShortName,Yaxis,scale,label)
  
  
 
}


