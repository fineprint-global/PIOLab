# PIOLab
This repo features all R functions and codes that are required to (1) compile data feeds and the initial estimate for the reconciliation routine, (2) plot the figures included in the associated publication and (3) perform basic IO analysis (calculating consumption-based/footprint indicators) for various stressors (i.e. boundary input & output flows).

1 & 2 demand MRSUTs of EXIOBASE 3 and BACI trade data which is not part of this repository. Contact hanspeter.wieland@wu.ac.at for further information. 

The folder PIOLab/Analysis/ is the working directory for plotting the figures and performing IO analysis. 
Base tables (gPSUTs and gPIOTs) and raw data outputs (in the Tvy-format) can be downloaded from Zotero: https://zenodo.org/record/4385975 . For IO analysis, copy the raw data tables ( e.g. 20201218_Mother_AllCountries_666_T-Results_2008_666_Markup001(full).csv ; contained in the RawData-folder in the Zotero-download) to the folder /PIOLab/Analysis/input/AISHA_runs/666/ . 

See PIOLab/Analysis/R/MainScript.R for loading and running the functions.

 


  
