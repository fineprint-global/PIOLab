
load("./Analysis/input/EPIP/score_in_root.R")

root_root <- read.csv( str_c(path$root,"/Analysis/input/EPIP/biome_in_root.csv"), sep = ";") %>%
  select(index, GID_id, GID_0, NAME_0)

# Load region root to base aggregation matrix
R2B <- read.csv( str_c( path$root, "/ConcordanceLibrary/Region Aggregators/032_RegionAggregator.csv"), header = FALSE )
R2B <- as.matrix(R2B)

# Read extraction in root classification
extraction_in_root <- read.csv( str_c(path$root,"/Analysis/input/EPIP/Iron Ore_",job$year,"_in_roots.csv")) %>% 
  select(GID_id, value)

root_score_sel <- root_score$value[ match(extraction_in_root$GID_id ,root_score$GID_id) ]

extraction_in_root["score_mean"] <- unlist( lapply(root_score_sel, mean) )

extraction_in_root <- extraction_in_root %>% drop_na()

# Write root scores to file
result <- left_join(extraction_in_root, root_root)
result <- result[,c(4,5,6,1,2,3)]

# write.xlsx(result, file = str_c( path$output,"/BiodiversityCarbonWater_IronOre_2014_root_regions.xlsx"))


# Aggregate scores to root root

extraction_in_root["score_mean_weighted"] <- extraction_in_root$value * extraction_in_root$score_mean
extraction_score <- left_join(extraction_in_root, root_root) %>% select(GID_0, score_mean_weighted, value) %>% 
  group_by(GID_0) %>% summarise(value = sum(value), score_mean_weighted = sum(score_mean_weighted))

extraction_score$score_mean_weighted <- extraction_score$score_mean_weighted/ extraction_score$value

root_region <- root$region

E <- data.frame( matrix(0,nrow = 221, ncol = 2) )
colnames(E) <- c("extraction", "score")

index <- match(extraction_score$GID_0, root_region$RootCountryAbbreviation)

E$extraction[index] <- extraction_score$value
E$score[index] <- extraction_score$score_mean_weighted
E$score <- E$score * E$extraction
E <- as.matrix(t(E)) %*% R2B 
E <- as.data.frame(t(E))
colnames(E) <- c("extraction", "score")
E$score <- E$score / E$extraction 
rownames(E) <- base$region$Abbrev
E[is.na(E)] <- 0

global_avg_score <- sum( E$extraction * E$score ) /sum(E$extraction)

E$score[E$score ==0] <- global_avg_score

## Calculate footprints
x <- rowSums(IOT$L %*% IOT$y)  # Estimate new gross production
q <- IOT$e[, base$input$Code[base$input$Name == "Crude Ore"] ]/x

MP <- IOT$L * q  # Material multipliers

# First for all countries
FP <- MP %*% IOT$y  # Material fooptrints
FP <- Agg(FP, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
FP <- Agg(FP, Code$Z %>% filter(EntityName == "Industry") %>% pull(RegionCode), 1)

colnames(FP) <- base$region$Name

Plot_Impact_Footprint(FP)

Mat_Footprint <- colSums(FP)
FP <- colSums( E$score * FP ) / colSums(FP)

result <- data.frame("index" = 1:32,
                     "Region" = base$region$Abbrev,
                     "Extraction_Impact_Rank" = E$score,
                     "Footprint_Impact_Rank" = FP,
                     "Extraction" = E$extraction,
                     "Footprint" = Mat_Footprint)

result$Extraction_Impact_Rank[result$Extraction_Impact_Rank == global_avg_score] <- 0

# write.xlsx(result, file = str_c( path$output,"/BiodiversityCarbonWater_IronOre_2014_32_regions.xlsx"))

# Second for country groups
FP <- MP %*% IOT$y  # Material fooptrints
FP <- Agg(FP, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
FP <- Agg(FP, Code$Z %>% filter(EntityName == "Industry") %>% pull(RegionCode), 1)
FP <- Agg(FP, base$region$Region, 2)
regions <- colnames(FP)
# Mat_Footprint <- colSums(FP)
# FP <- colSums( E$score * FP ) / colSums(FP)
# result <- data.frame("index" = 1:6,
#                      "Region" = regions,
#                      "Footprint_Impact_Rank" = FP,
#                      "Footprint" = Mat_Footprint)
# 
# write.xlsx(result, file = str_c( path$output,"/BiodiversityCarbonWater_IronOre_2014_6_regions.xlsx"))

Plot_Impact_Footprint <- function(FP_data)
{
  regions <- colnames(FP_data)
  
  for(i in 1:ncol(FP_data))
  {
    dat <- data.frame("score" = E$score, 
                      "extraction" = FP_data[,i],
                      "region" = base$region$Name)
    
    dat <- dat[dat$extraction > 1,]
    dat <- dat[order(-dat$extraction),]
    dat_row <- dat[6:nrow(dat),]
    dat <- dat[1:5,]
    dat <- dat %>% add_row()
    dat$score[6] <- sum(dat_row$score * dat_row$extraction)/sum(dat_row$extraction)
    dat$extraction[6] <- sum(dat_row$extraction)
    dat$region[6] <- "RoW"
    dat$extraction <- dat$extraction/sum(dat$extraction)
    dat$extraction <- dat$extraction * 100
    
    ggplot( data = dat, aes(x = score, y = extraction, color = region ) ) +
      geom_point(alpha = 0.8, size=4, aes(shape = region) ) +
      labs( x= " Biodiversity-Carbon-Water Score",
            y = "% of Consumption Footprint") +
      theme( panel.grid.minor = element_blank(), 
             legend.position = "top",
             text = element_text(size=14),
             legend.text=element_text(size=14),
             legend.background = element_rect(fill="gray90", size=.5) ) +
      ggtitle(str_c("Iron Ore Footprint of ",regions[i]," by source region and impact score in ",job$year))
    
    # Save plot: 
    ggsave(path = path$output, 
           filename = paste0(job$year,"_Footprint_",regions[i],"_SourceRegion_ImpactScore.png"),
           width = 10, height = 10 )
  }
}
