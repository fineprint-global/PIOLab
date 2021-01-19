###############################################################################
# This script compares several ratios in the PIOT with the constraints

Plot_Ratios <- function()
{
  Code_sel <- list( "BF" = Code$Z %>% filter( EntityCode == 1, SectorCode == 2 ) %>%  pull(SectorIndex),
                    "BOF" = Code$Z %>% filter( EntityCode == 1, SectorCode == 4 ) %>%  pull(SectorIndex),
                    "EAF" = Code$Z %>% filter( EntityCode == 1, SectorCode == 6 ) %>%  pull(SectorIndex))
  
  # Read constraints for setting ticks in plot
  Constraints <- read.xlsx( paste0(path$settings,"/Base/IE_settings.xlsx"), sheet = 1 )
  
  Constraints <- Constraints$value[ Constraints$item %in% c("TopGasPerPigIron",
                                                            "SlagPerPigIron",
                                                            "AirPerPigIron",
                                                            "CokePerPigIron",
                                                            "FluxPerPigIron",
                                                            "OrePerPigIron") ]
  
  WSA <- list("PigIron" = paste0(path$IE_processed,"/WSA/WSA_",job$year,"_PigIron.csv"),
              "BOF_steel" = paste0(path$IE_processed,"/WSA/WSA_",job$year,"_SteelOxygenBlownConverter.csv"),
              "EAF_steel" = paste0(path$IE_processed,"/WSA/WSA_",job$year,"_SteelElectricFurnace.csv") )
  
  rawdata <- lapply(X = WSA, FUN = read.csv)
  
  
  ### Blast furnace process ###
  
  # Create data frame for storing results
  realized <- data.frame("index" = 1:num$reg,
                         "region" = base$region$Abbrev,
                         "Air" = NA,
                         "Ore" = NA,
                         "Coke" = NA,
                         "Gas" = NA,
                         "Slag" = NA,
                         "Flux" = NA,
                         "Total" = NA)
  
  # Read results from table 
  realized$Total <- rowSums( SUT$S[Code_sel$BF, ] ) 
  realized$Air <- SUT$v[ base$input %>% filter(Name == "Air") %>% pull(Code), Code_sel$BF ]
  realized$Coke <- SUT$v[ base$input %>% filter(Name == "Coke") %>% pull(Code), Code_sel$BF ]
  realized$Flux <- SUT$v[ base$input %>% filter(Name == "Flux") %>% pull(Code), Code_sel$BF ]
  realized$Ore <- colSums( SUT$U[ Code$Z %>% filter( EntityCode == 2, SectorName == "Iron ore" ) %>%  pull(SectorIndex), Code_sel$BF ] )
  realized$Gas <- SUT$w[ 2, Code_sel$BF ]
  realized$Slag <- SUT$w[ 1, Code_sel$BF ]
  
  # Calculate ratios i.e. intensities
  realized[,c("Ore","Coke","Air","Flux","Gas","Slag")] <- round( realized[,c("Ore","Coke","Air","Flux","Gas","Slag")] / realized$Total, digits = 3)
  
  # Select WSA regions and reorder for plot
  realized <- realized[rawdata$PigIron$base,c("region","Ore","Coke","Air","Flux","Gas","Slag")]
  
  colnames(realized)[2:7] <- c("Input: Iron ore",
                               "Input: Coke",
                               "Input: Air/Oxygen",
                               "Input: Flux",
                               "Output: Gas",
                               "Output: Slag")
  
  realized <- melt(realized)
  realized$variable <- factor(realized$variable, levels = c("Input: Iron ore",
                                                            "Input: Coke",
                                                            "Input: Air/Oxygen",
                                                            "Input: Flux",
                                                            "Output: Gas",
                                                            "Output: Slag") )
  
  y_max <- ceiling( max(realized$value) * 10 )/10
  
  # Create plot
  ggplot(data = realized, mapping = aes(x = region, y = value, color = variable) ) +
    geom_point(alpha = 1, size = 4, aes(shape = variable) ) + 
    scale_color_viridis_d(begin = 0, end = 1) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, y_max),
                       breaks = seq(0, y_max, by = 0.25) ) +
    scale_shape_manual(values=c(15:20)) +
    labs(x = "Regions where World Steel reports pig iron production",
         y = "Per unit of pig iron") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5),
          legend.title = element_blank(),
          legend.position="bottom",
          text = element_text(size=12),
          panel.grid.minor = element_blank() )
  
    
  # Save plot 
  ggsave(path = path$output, 
         filename = paste0(job$year,"_Ratios_BlastFurnace.png"),
         width = 8, height = 5 )
  
  ## Write ratio data to folder for SI in paper
  
  write.xlsx( realized, file = paste0(path$output,"/SI/",job$year,"_Data for ratio realization.xlsx") )
  
  ### Basic Oxygen Furnace ###
  
  # Create data frame for storing results
  realized <- data.frame("index" = 1:num$reg,
                         "region" = base$region$Abbrev,
                         "Iron" = NA,
                         "Scrap" = NA,
                         "Total" = NA)
  
  # Read results from table 
  realized$Total <- colSums( SUT$U[,Code_sel$BOF ] ) 
  realized$Iron <- colSums( SUT$U[ Code$Z %>% filter( EntityCode == 2, SectorName == "Pig iron" ) %>%  pull(SectorIndex), Code_sel$BOF ] )
  realized$Scrap <- colSums( SUT$U[ Code$Z %>% filter( EntityCode == 2, SectorName %in% c("Steel scrap","Forming scrap" ) ) %>%  
                                                         pull(SectorIndex), Code_sel$BOF ] )
  
  # Calculate ratios i.e. intensities
  realized[,c("Iron","Scrap")] <- round( realized[,c("Iron","Scrap")] / realized$Total, digits = 3)
  
  # Select WSA regions and reorder for plot
  realized <- realized[rawdata$BOF_steel$base, c("Iron","Scrap")]
  realized <- melt(realized)
  realized$variable <- factor(realized$variable, levels = c("Iron","Scrap") )
  
  v_colors <- viridis_pal()(2)   # Select colors
  
  # Create plot
  ggplot(data = realized, mapping = aes(x = value, fill = variable) ) +
    geom_dotplot(binwidth = 0.02,
                 alpha = 0.7,
                 position = position_jitter(w = 0.005, h = 0),
                 dotsize= 0.5) + 
    scale_fill_manual(values= v_colors) +
    scale_x_continuous(breaks= seq(0,2.5,0.1) ) +
    scale_y_continuous(breaks=NULL) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 12),
          legend.position = c(0.5,0.95),
          legend.direction = "horizontal",
          legend.text=element_text(size=14,face = "bold"),
          panel.grid.minor = element_blank()) +
    labs( x="\nInput per output of liquid steel from basic oxygen furnace", y = "",fill = "") +
    expand_limits( y=c(0,2) )
  
  # Save plot 
  ggsave(path = path$output, 
         filename = paste0(job$year,"_Ratios_BasicOxygenFurnace.png"),
         width = 14, height = 8 )
  
  
  
  
  ### Electric Arc Furnace ###
  
  # Create data frame for storing results
  realized <- data.frame("index" = 1:num$reg,
                         "region" = base$region$Abbrev,
                         "Iron" = NA,
                         "Scrap" = NA,
                         "Total" = NA)
  
  # Read results from table 
  realized$Total <- colSums( SUT$U[,Code_sel$EAF ] ) 
  realized$Iron <- colSums( SUT$U[ Code$Z %>% filter( EntityCode == 2, SectorName %in% c("Pig iron","Sponge iron") ) %>%  pull(SectorIndex), Code_sel$EAF ] )
  realized$Scrap <- colSums( SUT$U[ Code$Z %>% filter( EntityCode == 2, SectorName %in% c("Steel scrap","Forming scrap" ) ) %>%  
                                      pull(SectorIndex), Code_sel$EAF ] )
  
  # Calculate ratios i.e. intensities
  realized[,c("Iron","Scrap")] <- round( realized[,c("Iron","Scrap")] / realized$Total, digits = 3)
  
  # Select WSA regions and reorder for plot
  realized <- realized[rawdata$EAF_steel$base, c("Iron","Scrap")]
  realized <- melt(realized)
  realized$variable <- factor(realized$variable, levels = c("Iron","Scrap") )
  
  v_colors <- viridis_pal()(2)   # Select colors
  
  # Create plot
  ggplot(data = realized, mapping = aes(x = value, fill = variable) ) +
    geom_dotplot(binwidth = 0.02,
                 alpha = 0.7,
                 position = position_jitter(w = 0.005, h = 0),
                 dotsize= 0.7) + 
    scale_fill_manual(values= v_colors) +
    scale_x_continuous(breaks= seq(0,1,0.1) ) +
    scale_y_continuous(breaks=NULL) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 12),
          legend.position = c(0.5,0.95),
          legend.direction = "horizontal",
          legend.text=element_text(size=14,face = "bold"),
          panel.grid.minor = element_blank()) +
    labs( x="\nInput per output of liquid steel from electric arc furnace", y = "",fill = "") +
    expand_limits( y=c(0,2) )
  
  # Save plot 
  ggsave(path = path$output, 
         filename = paste0(job$year,"_Ratios_ElectricArcFurnace.png"),
         width = 14, height = 7 )
  
  
  
    
}
