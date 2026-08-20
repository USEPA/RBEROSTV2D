##############################################################
### RBEROST Preprocessing Gateway                          ###
### Date: May 6, 2021                                      ###
### Author: Cathy Chamberlin                               ###
### Purpose: Bridge between RBEROST run file and R scripts ###
##############################################################

if(exists("IncludeUncertainty")) {
  if(IncludeUncertainty == TRUE) {
  source(paste0(working_dir,"RBEROST-Pacific/R/01_Optimization_Preprocessing+Uncertainty-Pacific_[PROD].R"))
} else if(IncludeUncertainty == FALSE) {
    source(paste0(working_dir,"RBEROST-Pacific/R/01_Optimization_Preprocessing-Pacific_V2_[PROD].R"))
} else {
    print(
      "Only allowable options are 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'. You cannot have both, only one or the other."
    )
  }
} else {
  print(
    "Did you delete the line of code that says 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'?"
  )
}
