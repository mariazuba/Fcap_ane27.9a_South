
# Script information ------------------------------------------------------
#bootstrap/initial
#The essential inputs for the analysis include the result of the stock assessment model fit and the current set of reference points.

# Load libraries ----------------------------------------------------------
library("icesTAF")  # Load the ICES Transparent Assessment Framework (TAF) package
library("r4ss")     # Load the r4ss package for working with Stock Synthesis (SS3)
# Check the working directory ---------------------------------------------

getwd()



# Make the skeleton -------------------------------------------------------

# create initial directories and R scripts for a new TAF analysis
  
# ¦--bootstrap   
# ¦   ¦--initial 
# ¦   ¦   `--data
# ¦   ¦       `-- ane27.9aS.rda  - Input dataset
# ¦   ¦-- references.bib
# |.  |--DATA.bib
# ¦--data.R      
# ¦--model.R     
# ¦--output.R    
# °--report.R    

#taf.skeleton()

# Upload initial data -----------------------------------------------------



# Create a DATA.bib file with metadata about the data used
# The SS3 scenarios metadata
draft.data(
  originator = "WGHANSA",           # Who created the data (e.g., Working Group)
  year = 2024,                      # Year of the data
  title = "SS3 data format",         # Title for the data
  period = "1989-2024",              # Period of the data
  file = TRUE                        # Export the metadata to a .bib file
)

# Bootstrap the data from the boot/data folder
taf.bootstrap()

# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------
