################################################################################
# This script loads the right WSA yearbook

# Depending on the year that is processed, the data feed either sources numbers 
# from the 2018 or the 2005 yearbook

if(year >= 2008)
{
  # Defining the page(s) where the production values are
  items <- list(list("page"=96,"name"="SpongeIron"),
                list("page"=c(90,91),"name"="PigIron"),
                list("page"=c(7,8),"name"="Ingots"),
                list("page"=20,"name"="SteelOxygenBlownConverters"),
                list("page"=c(22,23),"name"="SteelElectricFurnaces"),
                list("page"=26,"name"="SteelOpenHearthFurnaces"),
                list("page"=c(32,33),"name"="LongRolledProducts"),
                list("page"=34,"name"="FlatRolledProducts"),
                list("page"=c(30,31),"name"="HotRolledProducts"),
                list("page"=c(10,11),"name"="ContinuouslyCastSteel"),
                list("page"=14,"name"="LiquidSteelForCastings"),
                list("page"=c(1,2),"name"="TotalProductionOfCrudeSteel"),
                list("page"=35,"name"="RailwayTrackMaterial"),
                list("page"=36,"name"="HeavySections"),
                list("page"=37,"name"="LightSections"),
                list("page"=c(38,39),"name"="ConcreteReinforcingBars"),
                list("page"=40,"name"="Hot RolledBarsOtherThanConcreteReinforcingBars"),
                list("page"=c(41,42),"name"="WireRod"),
                list("page"=43,"name"="HotRolledPlate"),
                list("page"=44,"name"="HotRolledCoilSheetStrip"),
                list("page"=45,"name"="ElectricalSheetAndStrip"),
                list("page"=46,"name"="TinmillProducts"),
                list("page"=47,"name"="OtherMetalCoatedAndSheetandStrip"),
                list("page"=48,"name"="OtherNonMetalCoatedAndSheetandStrip"),
                list("page"=49,"name"="TubesAndTubeFittings"),
                list("page"=50,"name"="SeamlessTubes"),
                list("page"=51,"name"="WeldedTubes"))
  
  
                               

  
  # Loading raw RData-File containing the full yearbook
  # Note that at the moment the data feed does not encompass the transformation
  # of the original Yearbook pdf into an RData-File.
  load(file = paste0(path$Raw,"/WSA/Worldsteeldata 2018.RData"))
  # The years covered by the yearbook
  years <- 2008:2017
  
}else
{
  items <- list(list("page"=3,"name"="SpongeIron"),
                list("page"=c(1,2),"name"="PigIron"),
                list("page"=c(13,14),"name"="Ingots"),
                list("page"=c(29,30),"name"="SteelOxygenBlownConverters"),
                list("page"=c(33,34),"name"="SteelElectricFurnaces"),
                list("page"=37,"name"="SteelOpenHearthFurnaces"),
                list("page"=c(45,46),"name"="LongRolledProducts"),
                list("page"=c(47,48),"name"="FlatRolledProducts"))
  
  load(file = paste0(path$Raw,"/WSA/Worldsteeldata 2005.RData"))
  years <- 1995:2004
}

# Change name of the object (asap)
yb <- out
remove(out)

# load concordance to match Worldsteel country names with 3digitISO codes
concord <- read.xlsx(xlsxFile = paste0(path$Concordance,"/WSA/WSA_RegionConcordance.xlsx"),sheet = 1)

