# Load functions:

fun <- list(paste0(path$repo,"/R/Subroutines/Load_IOCodes.R"),
            paste0(path$repo,"/R/Subroutines/Load_SUT.R"),
            paste0(path$repo,"/R/Subroutines/Build_IOT.R"),
            paste0(path$root,"/Rscripts/Subroutines/Agg.R"),
            paste0(path$repo,"/R/Subroutines/Plot_HeadlineIndicators.R"),
            paste0(path$repo,"/R/Subroutines/Check_Balance.R"),
            paste0(path$repo,"/R/Subroutines/Check_domesticSUT.R"),
            paste0(path$repo,"/R/Subroutines/Compile_RegionProfile.R"),
            paste0(path$repo,"/R/Subroutines/Calc_ewMFA.R"),
            paste0(path$repo,"/R/Subroutines/Calc_EXIOfootprint.R"),
            paste0(path$repo,"/R/Subroutines/MonteCarloSimulation.R"),
            paste0(path$repo,"/R/Subroutines/Check_import.R"),
            paste0(path$repo,"/R/Subroutines/Plot_Rocket.R"),
            paste0(path$repo,"/R/Subroutines/Diagnostics.R"),
            paste0(path$repo,"/R/Subroutines/Calc_FP.R"),
            paste0(path$repo,"/R/Subroutines/Analysis_Y.R"),
            paste0(path$repo,"/R/Subroutines/Plot_Ratios.R"),
            paste0(path$repo,"/R/Subroutines/Plot_BoundaryFlows.R"),
            paste0(path$repo,"/R/Subroutines/Prepare_SankeyData.R"),
            paste0(path$repo,"/R/Subroutines/mrSUT_heatmap.R"),
            paste0(path$repo,"/R/Subroutines/Numbers2File.R"))



lapply(fun,source)

remove(fun)

base <<- list("region" = read.xlsx(paste0(path$settings,"/Base/",job$RegAgg,"_BaseRegionClassification.xlsx"),sheet = 1),
              "industry" = read.xlsx(paste0(path$settings,"/Base/",job$IEdatafeed,"_BaseSectorClassification.xlsx"),sheet = 1),
              "product" = read.xlsx(paste0(path$settings,"/Base/",job$IEdatafeed,"_BaseSectorClassification.xlsx"),sheet = 2),
              "input" = read.xlsx(paste0(path$settings,"/Base/",job$IEdatafeed,"_BaseSectorClassification.xlsx"),sheet = 4),
              "demand" = read.xlsx(paste0(path$settings,"/Base/",job$IEdatafeed,"_BaseSectorClassification.xlsx"),sheet = 3))

num <<- list("pro" = nrow(base$product),
             "ind" = nrow(base$industry),
             "reg" = nrow(base$region),
             "va" = nrow(base$input),
             "fd" = nrow(base$demand) )

# Load and aggregate population data
# pop <<- read.xlsx(paste0(path$repo,"/input/EXIOBASE population data.xlsx"),sheet = 3) %>%
#   select(EXIOcode,as.character(job$year)) 
# 
# pop <<- pop[1:49,2] # Clean pop data

