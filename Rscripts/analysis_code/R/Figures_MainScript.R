# Install packages
# install.packages("ggpubr")
#install.packages("ggExtra")
#install.packages("mime")

# Load packages
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(ggExtra)

# Define job and settings of table that is to be analysed:


# 40 regions (orginal):
job <<- list("date" = "20200723",
             "phase" = "666",
             "loop" = "331",
             "year" = 2008,
             "RegAgg" = "040",
             "IEdatafeed" = "Ind30Pro39v1")

# 40 regions (timeseries):
job <<- list("date" = "20201118",
             "phase" = "666",
             "loop" = "366",
             "year" = 2014,
             "RegAgg" = "040",
             "IEdatafeed" = "Ind30Pro39v1")


# Set path to folder where results from AISHA runs are stored:

github <- "C:/Users/hwieland/Github workspace"

path <<- list("input" = paste0(github,"/PIOLab_Analysis/input/"),
              "output" = paste0(github,"/PIOLab_Analysis/output/",job$loop),
              "run" = paste0(github,"/PIOLab_Analysis/input/AISHA_runs/",job$loop),
              "repo" = paste0(github,"/PIOLab_Analysis"),
              "root" = paste0(github,"/PIOLab"),
              "concordance" = paste0(github,"/PIOLab/ConcordanceLibrary"),
              "processed" = paste0(github,"/PIOLab/ProcessedData"),
              "subroutines" = paste0(github,"/PIOLab_Analysis/R/Subroutines"),
              "settings" = paste0(github,"/PIOLab/Settings"),
              "raw" = paste0(github,"/PIOLab/RawDataRepository"),
              "IE_processed" = paste0(github,"/PIOLab/ProcessedData","/",job$IEdatafeed,"/",job$RegAgg) )

source( paste0(path$subroutines,"/Load_Routines.R") )

Code <<- Load_IOCodes()           # Compile IO model codes
SUT <<- Load_SUT("Results")       # Create supply use tables from raw data
IOT <<- Build_IOT(SUT,"pxp")      # Compile IO model

# Create empty folder for output
if(dir.exists(path$output)) unlink(path$output,recursive = TRUE)
dir.create(path$output)
dir.create( paste0( path$output, "/SI" ) )


Plot_HeadlineIndicators()

Prepare_SankeyData()

mrSUT_heatmap("China")

Diagnostics()



Plot_Ratios()





