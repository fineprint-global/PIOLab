################################################################################
# Monte Carlo Simulation 

MonteCarloSimulation <- function(iterations,mode,SD_max,SD_low,distribution)
{
  LogNormal <- function(m,s)
  {
    location <- log(m^2 / sqrt(s^2 + m^2))
    shape <- sqrt(log(1 + (s^2 / m^2)))
    out <- rlnorm(n=length(m), location, shape)
    out <- matrix(out, nrow = nrow(m), ncol = ncol(m))
    
    return(out)
  }
   
  if(mode == 1)  # Assuming constant standard errors for all elements
  {
    SD <- list( "S" = SUT$S * SD_max,
                "U" = SUT$U * SD_max,
                "v" = SUT$v * SD_max,
                "y" = SUT$y * SD_max,
                "w" = SUT$w * SD_max)
  }
  
  if(mode == 2) 
  {
    SD <- Load_SUT("RsltSTDDEV") # Import standard deviations from run
    
    # Set values smaller than 1 to 1 in order to avoid negative SD and Inf values
    # SD$S[SD$S < 1] <- 1
    # SD$U[SD$U < 1] <- 1
    # SD$v[SD$v < 1] <- 1
    # SD$y[SD$y < 1] <- 1
    # SD$w[SD$w < 1] <- 1
  }
  
  # Set up storage table
  store <- list("DE" = matrix(0,nrow = iterations,ncol = ncol(SUT$y)),
                "EoL" = matrix(0,nrow = iterations,ncol = ncol(SUT$y)) )
  
  for(i in 1:iterations)
  {
    print(paste0(i,"/",iterations," iterations"))
    
    
    if(distribution == "normal")
    {
      
      # Sampling random numbes from normal distribution
      S <- matrix( rnorm( length(SD$S), mean= SUT$S, sd=SD$S) , nrow(SUT$S),ncol(SUT$S)) 
      U <- matrix( rnorm( length(SD$U), mean= SUT$U, sd=SD$U), nrow(SUT$U),ncol(SUT$U))
      v <- matrix( rnorm( length(SD$v), mean= SUT$v, sd=SD$v), nrow(SUT$v),ncol(SUT$v))
      y <- matrix( rnorm( length(SD$y), mean= SUT$y, sd=SD$y), nrow(SUT$y),ncol(SUT$y))
      w <- matrix( rnorm( length(SD$w), mean= SUT$w, sd=SD$w), nrow(SUT$w),ncol(SUT$w))
      
      #S[S == Inf] <- 1
      #U[U == Inf] <- 1
      #v[v == Inf] <- 1
      #y[y == Inf] <- 1
      #w[w == Inf] <- 1
      
    }
    
    if(distribution == "lognormal")
    {
      
      S <- LogNormal( SUT$S, SD$S )  # Function produces errors because of the off-diagonal elements
      S[is.na(S)] <- 0
      U <- LogNormal( SUT$U, SD$U )
      
      #v <- LogNormal( SUT$v, SD$v )
      v <- SUT$v
      
      y <- LogNormal( SUT$y, SD$y )
      
      #w <- LogNormal( SUT$w, SD$w )
      w <- SUT$w
      
    }
    
    
    
    # Setting negatives zero
    # S[S < 0] <- 0
    # U[U < 0] <- 0
    # v[v < 0] <- 0
    # y[y < 0] <- 0
    # w[w < 0] <- 0
    
    # Setting negatives zero
    S[which(S < 0)] <- SUT$S[which(S < 0)]
    U[which(U < 0)] <- SUT$U[which(U < 0)]
    v[which(v < 0)] <- SUT$v[which(v < 0)]
    y[which(y < 0)] <- SUT$y[which(y < 0)]
    w[which(w < 0)] <- SUT$w[which(w < 0)]
    
    
    
    
    SUT_mc <- list("S" = S,
                   "U" = U,
                   "v" = v,
                   "y" = y,
                   "w" = w)
    
    # Transform into product by product IO model
    IOT_mc <- try(Build_IOT(SUT_mc,"pxp"),silent = TRUE)
    
    if (!'try-error' %in% class(IOT_mc))
    {
      Z <- IOT_mc$Z
      L <- IOT_mc$L
      L[L < 0] <- 0
      y <- IOT_mc$y
      e <- IOT_mc$e
      q <- rowSums(L%*%y)
      
      # Domestic Extraction intensities
      intens <- e[, Code$V$index[Code$V$Entity == "Crude Ore"] ] / q
      
      # Calculate multipliers and flow matrix
      MP <- L * intens
      store$DE[i,] <- colSums(MP%*%y) 
      
      # Domestic Extraction intensities
      intens <- e[, Code$V$index[Code$V$Entity == "End-of-Life Scrap"] ] / q
      
      # Calculate multipliers and flow matrix
      MP <- L * intens
      store$EoL[i,] <- colSums(MP%*%y) 
    }
      
  }
  
  # Calculate how many permutations produced singular matrices
  iterations_real <- nrow(store$DE[rowSums(store$DE) != 0,])
  fail <- iterations - iterations_real
  
  # Remove failed iterations
  store$DE <- store$DE[ rowSums(store$DE) != 0, ]
  store$EoL <- store$EoL[ rowSums(store$EoL) != 0, ]
  
  # Aggregate final demand categories
  FP_mc <- list("DE" = Agg(store$DE, rep( Code$Y$RegionName, each = 6 ), 2),
                "EoL" = Agg(store$EoL, rep( Code$Y$RegionName, each = 6 ), 2) )
  
  colnames(FP_mc$DE) <- colnames(FP_mc$EoL) <- base$region$Abbrev
  
  # calculate MC per cap footprints
  
  FP_mc_cap <- list("DE" = t( t(FP_mc$DE)/ pop),
                    "EoL" = t( t(FP_mc$EoL)/ pop) )
  
  
  
  
  100 * round( apply(FP_mc_cap$DE, 2, sd)/colMeans(FP_mc_cap$DE), digits = 2)
  
  # Function to create plots
  Plotting <- function(df,title,Yaxis,original,ordering,y_max)
  {
    df[df < 0] <- 0
    df <- df[,order(original,decreasing = TRUE)]
    
    Comp <- melt(df)  # transform from wide to long
    Comp$Var2 <- factor(Comp$Var2, levels = colnames(df))
    
    
    boxplot <-  ggplot( data = Comp, mapping = aes(Var2, value) ) + 
      geom_boxplot(fill =  "#DCE319FF",
                   outlier.shape = NA,
                   position = "dodge") +
      stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
      scale_y_continuous(limits = c(0,y_max), expand=c(0,0)) +
      labs(title = paste(title,"\n(Note: Yellow area of boxplot marks 25th and 75th percentile,  mean is shown as red dot and median as black line)"), x="Region", y = Yaxis)
    
    
    # Plot relative distributions with ridgelines:
    
    # ridge <- ggplot( data = Comp, aes(x = value, y = Var2, fill = ..x..)) +
    #   geom_density_ridges_gradient(aes(fill = ..x..),
    #                                scale = 5,
    #                                size = 0,
    #                                rel_min_height = 0.01,
    #                                quantile_lines = TRUE,
    #                                quantiles = 0.5) +
    #   scale_fill_viridis(name = "Metric tons per capita", option = "D",direction = -1) +
    #   xlim(0, 2) +
    #   labs(title = paste(title,"(ridgeline density distributions)"),
    #        x= Yaxis,
    #        y = "Region") +
    #   theme(
    #     legend.position="none",
    #     panel.spacing = unit(4, "lines"),
    #     strip.text.x = element_text(size = 8),
    #     strip.text.y = element_text(size = 8))
   
    return(boxplot)
  }
  
  
  Plotting_error <- function(df,title,Yaxis,original,ordering,y_max)
  {
    df[df < 0] <- 0
    
    df <- data.frame("region" = base$region$Abbrev,
                     "mean" = colMeans(df),
                     "sd" = apply(df, 2, sd),
                     stringsAsFactors = FALSE)
    
    df <- df[order(original,decreasing = TRUE),]
    
    v_colors <- viridis_pal()(2)  # Define
    
    error <- ggplot(data = df) +
      geom_errorbar( aes(x = region, ymin = mean - sd, ymax = sd + sd ),
                     stat = "identity",
                     width=0.2, size=1, color= v_colors[1]) +
      geom_point( aes(x = region, y = mean),
                  size=2, shape=21, fill= v_colors[2] ) +
      labs( x="Region", y = "tons per capita")
    
    return(error)
  }
  
  # Calculate orginal footprints
  FP_piot_cap <- list("DE" = Calc_FP("Crude Ore",1),
                      "EoL" = Calc_FP("End-of-Life Scrap",1) )
  
  
  # Plot per capita results
  
  title <- paste0("Monte carlo simulation results after ",iterations_real," iterations (",fail," permutations were singular)\nPer capita iron ore footprints of steel in final consumption in the year ",job$year)
  Yaxis <- "metric tons per capita"
  ShortName <- "MC_DE"
  df <- FP_mc_cap$DE
  original <- FP_piot_cap$DE
  ordering <- 0
    
  Plotting(df,title,Yaxis,original,ordering,2.5)
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0(ShortName,".png"),
         width = 14, height = 7 )
  
  Plotting_error(df,title,Yaxis,original,ordering,2.5)
  ShortName <- "SD_DE"
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0(ShortName,".png"),
         width = 14, height = 7 )
  
  

  title <- paste0("Monte carlo simulation results after ",iterations," iterations\nPer capita End-of-Life Scrap footprints of steel in final consumption in the year ",job$year)
  Yaxis <- "metric tons per capita"
  df <- FP_mc_cap$EoL
  original <- FP_piot_cap$EoL
  ordering <- 0
  ShortName <- "MC_EoL"
  
  Plotting(df,title,Yaxis,original,ordering,1)
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0(ShortName,".png"),
         width = 14, height = 7 )
  
  Plotting_error(df,title,Yaxis,original,ordering,2.5)
  ShortName <- "SD_EoL"
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0(ShortName,".png"),
         width = 14, height = 7 )
  
  
  return(FP_mc_cap)
 }