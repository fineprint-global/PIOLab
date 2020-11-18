# this function checks the sector balances (industries)


Check_Balance <- function()
{

  code_sel <- filter(Code$Z,EntityCode == 1) %>% select(SectorIndex,RegionName,SectorName) 
  
  df <- data.frame("Use" = ( colSums(SUT$U) + colSums(SUT$v) ),
                   "Supply" = ( rowSums(SUT$S) + colSums( SUT$w ) ) )
  
  # df <- round(df, digits = 3)
  
  
  df["difAbs"] = df$Use - df$Supply   # Absolute difference between input/output
  df["difRel"] =  round( 100 * df$difAbs / df$Supply, digits = 0)  # Relative difference
  
  df <- cbind(code_sel,df)  # Add sector codes

  # Add sector names:
  df <- left_join(df, base$industry[,c("Name","Type")], by = c("SectorName" = "Name") )
  
  # Create new sector names for legend in plots:
  proc_labels <- data.frame("old" = unique(df$Type),
                            "new" = c("Extraction","Iron/Steelmaking","Casting","Rolling & forming","Manufacturing","Recycling"))
  
  df$Type <- proc_labels$new[proc_labels$old  %in%  df$Type]  # Change labels to better name processes
  df$Type <- factor(df$Type, levels =  unique(df$Type) )  # transform to factor for ordering
  
  # Export data for SI of paper
  write.xlsx( df, file = paste0( path$output,"/SI/Data for process balances.xlsx") )
  
  # Transform to log10 scale
  
  df$Supply <- log( df$Supply, base = 10)
  df$Use <- log( df$Use, base = 10)
  
  
  ### 1. Scatter plot ###
  
  v_colors =  viridis_pal()(6)  # Select 6 colors from viridis palette
  
  scatter <- ggplot( data = df, aes(x = Supply, y = Use, color = Type) ) +
    geom_point() +
    scale_colour_manual(values= setNames(v_colors, unique(df$Type) ) ) +
    labs(x = "Process output (log10 tonnes)", 
         y = "Process input (log10 tonnes)") +
    theme_gray() +
    guides(color=guide_legend("  Process type")) +
    geom_abline(intercept = 0.15, slope = 1, linetype = 2, alpha = 0.5) + 
    geom_abline(intercept = -0.15, slope = 1, linetype = 2, alpha = 0.5) +
    scale_x_continuous( expand = c(0,0),
                        limits = c(1,9),
                        breaks = 1:9) + 
    scale_y_continuous( expand = c(0,0),
                        limits = c(1,9),
                        breaks = 1:9 ) +
    theme(panel.grid.minor = element_blank(),
          legend.position = 'top',
          legend.title = element_blank())
    
  
  
  scatter # Show plot
  
  # Save plot: 
  ggsave(path = paste0(path$output), 
         filename = "MatBal_Scatter_Pro.png",
         width = 6, height = 6.5 )
  
  
  
  # v_colors =  viridis_pal()(num$reg)  # Select 6 colors from viridis palette
  
  scatter_reg <- ggplot( data = df, aes(x = Supply, y = Use, color = RegionName) ) +
    geom_point() +
    scale_x_continuous( expand = c(0,0),
                        limits = c(1,9),
                        breaks = 1:9) + 
    scale_y_continuous( expand = c(0,0),
                        limits = c(1,9),
                        breaks = 1:9 ) +
    labs(title = paste("Material balances of",num$ind*num$reg,"processes:\n30 processes per ",num$reg," regions, year 2008, UNIT: metric tons, dashed lines indicate +/- 15% range"),
         x="Total output flow (supply)", y = "Total input flow (use)") +
    theme_gray() +
    guides(color=guide_legend("  Region name")) +
    geom_abline(intercept = 0.15, slope = 1, linetype = 2, alpha = 0.5) + 
    geom_abline(intercept = -0.15, slope = 1, linetype = 2, alpha = 0.5)  
  
  scatter_reg
  
  # Save plot: 
  ggsave(path = paste0(path$output), 
         filename = "MatBal_Scatter_Reg.png",
         width = 10, height = 7 )
  
  # 2. Histogram for total output flow (supply):
  
  num_bin <- 30  # Set number of bins
  
  histo <- ggplot( data = df, aes( x = log10(Supply) ) ) +
    geom_histogram(bins = num_bin, color="darkblue", fill="lightblue") +
    scale_x_continuous(breaks = c(3, 6, 9),label = c("10^3", "10^6", "10^9")) +
    labs(title = paste("Histogram of total output flows (supply) "),
         x="Total output of process", y = "Process count") +
    scale_color_brewer(palette="Paired") + 
    theme_bw()
  
  histo # Show plot
  
  # Save plot: 
  ggsave(path = paste0(path$output), 
         filename = "TotalOutput_Histogram.png",
         width = 10, height = 7 )
  

  return(scatter)

}

