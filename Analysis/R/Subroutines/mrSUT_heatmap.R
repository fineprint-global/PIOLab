

mrSUT_heatmap <- function(region)
{
  
  domSUT <- Check_DomesticSUT(region)
  

  # Empty matrix to put all elements of SUTs in
  M <- matrix(NA,
              nrow = nrow(domSUT$Supply) + nrow(domSUT$Use) + nrow(domSUT$In),
              ncol = ncol(domSUT$Supply) + ncol(domSUT$Use) + ncol(domSUT$Y) + nrow(domSUT$Out) )
  
  M[ ( num$ind+1 ):( nrow(M)-nrow(domSUT$In) ), 1:ncol(domSUT$Use) ] <- domSUT$Use
  M[ ( num$ind+num$pro+1 ):nrow(M), 1:num$ind ] <- domSUT$In
  M[ 1:num$ind, (num$ind+1):(num$pro+num$ind)  ] <- domSUT$Supply
  M[ 1:num$ind, (ncol(M)+1-nrow(domSUT$Out)):ncol(M)  ] <- t(domSUT$Out)
  M[ ( num$ind+1 ):( nrow(M)-nrow(domSUT$In) ), (num$ind + num$pro + 1):( ncol(M) - nrow(domSUT$Out) ) ] <- domSUT$Y
  
  # Export table for SI of paper after adding labels
  SI <- M
  
  rownames(SI) <- c( base$industry$Name,
                     base$product$Name,
                     base$input$Name )
  
  colnames(SI) <- c( base$industry$Name,
                     base$product$Name,
                     base$demand$Name[1:6],
                     "SEM-output (solid & liquid)",
                     "SEM-output (gaseous)" )
  
  write.xlsx(SI, file = paste0( path$output, "/SI/",job$year,"_Data for heatmap.xlsx"), rowNames = TRUE)
  
  
  M <- melt(M)                                   # Transform from wide to long format 
  colnames(M) <- c("y","x","value")              # Change headings
  
  quant <- quantile(M$value[ !is.na(M$value) & M$value > 1])       # Calculate quantiles
  
  M$value[M$value > 1 & M$value <= quant[2]] <- 1         # 1. quantile set to 1
  M$value[M$value > quant[2] & M$value <= quant[3]] <- 2  # 2. quantile set to 2
  M$value[M$value > quant[3] & M$value <= quant[4]] <- 3  # 3. quantile set to 3
  M$value[M$value > quant[4] & M$value <= quant[5]] <- 4  # 4. quantile set to 4
  
  quant <- round( quant/1000, digit = 0)
  
  lab <- c("0",
           paste("1.Quartil ( 0 ,",quant[2],"]"),
           paste("2.Quartil (",quant[2],",",quant[3],"]"),
           paste("3.Quartil (",quant[3],",",quant[4],"]"),
           paste("4.Quartil (",quant[4],",",quant[5],"]"),
           "NA")
  
  ggplot(M, aes(x, -y, fill= factor(value) ) ) + 
    geom_tile() +
    theme_void() +
    scale_fill_viridis_d(direction = -1,na.value = "white", name = "", labels =  lab) +
    guides(fill=guide_legend(title=NULL)) +
    theme(legend.key = element_rect(fill = "black", colour = "black"))
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0(job$year,"_",region,"_Heatmap.png"),
         width = 14, height = 8 )
  
}
