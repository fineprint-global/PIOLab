
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


# Set parameters to select raw data files (version and/or year) for the construction of PIOTs
job <<- list("date" = "20201218",
             "phase" = "666",
             "loop" = "666",
             "year" = 2008,
             "RegAgg" = "032",
             "IEdatafeed" = "Ind30Pro39v1")


# Set path to folder with GitHub repositories:
# github <- "C:/Users/hwieland/Github workspace"
github <- "C:/Users/polly/OneDrive/Documents/GIT"
# github <- "C:/Users/hwieland/Github workspace"

# Set paths to important folders
path <<- list("input" = paste0(github,"/PIOLab/Analysis/input/",job$RegAgg,"/"),
              "output" = paste0(github,"/PIOLab/Analysis/output/",job$loop),
              "run" = paste0(github,"/PIOLab/Analysis/input/AISHA_runs/",job$loop),
              "repo" = paste0(github,"/PIOLab/Analysis"),
              "root" = paste0(github,"/PIOLab"),
              "SI" = paste0(github,"/PIOLab/Analysis/output/",job$loop,"/SI"),
              "concordance" = paste0(github,"/PIOLab/ConcordanceLibrary"),
              "processed" = paste0(github,"/PIOLab/ProcessedData"),
              "subroutines" = paste0(github,"/PIOLab/Analysis/R/Subroutines"),
              "settings" = paste0(github,"/PIOLab/Settings"),
              "raw" = paste0(github,"/PIOLab/RawDataRepository"),
              "IE_processed" = paste0(github,"/PIOLab/ProcessedData","/",job$IEdatafeed,"/",job$RegAgg) )


# Create empty folder for model outputs of selected raw data files (i.e. loop)
# if(dir.exists(path$output)) unlink(path$output,recursive = TRUE)
# dir.create(path$output)
# dir.create( path$SI )


# Load functions into workspace
source( paste0(path$subroutines,"/Load_Routines.R") )

# Read IO model codes
Code <<- Load_IOCodes() 

# Create supply use tables from raw data
SUT <<- Load_SUT("Results")       

# Create PIOTs
IOT <<- Build_IOT(SUT,"ixi")      # Compile IO model

# View(IOT$L)   # Total Requirement Matrix (Leontief inverse)
# View(IOT$y)   # Final demand matrix
# View(IOT$e)   # Extensions (boundary input and output vectors)

# Calculate ewMFA indicators from gPIOT
# ewMFA <- Calc_ewMFA(IOT, Code)

# Calculate footprints for selected stressor (i.e. boundary input/output flow)
# FP <- Calc_FP("Crude Ore", 1)

# Read domestic variables/tables for specific region
# SUT_sel <- Check_DomesticSUT("China")

# Prepare_SankeyData()
