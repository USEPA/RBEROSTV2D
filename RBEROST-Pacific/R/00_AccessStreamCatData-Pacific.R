###########################################################################################
# PURPOSE: Download StreamCat data for states covered by LIS Model  ####
# BY: Hunter Parker, Sam Ennett ####
# ORIGINAL DATE: 7/20/2023 ####
###########################################################################################

# Lines of code tagged with #*# may need to be edited by the user.
# NOTE: this code only needs to be re-run if the user needs a data vintage other than 2019.

##########################################################
# 1. Setup #####
##########################################################

# install.packages("dplyr") #*# #May need to be run if packages are not installed
#install.packages("StreamCatTools") #*# #May need to be run if packages are not installed

library("tidyverse")
library("stringr")
library("StreamCatTools") # If you have trouble installing using "install.packages()" https://github.com/USEPA/StreamCatTools
options(stringsAsFactors = FALSE)

##########################################################
# 2. Read CSV files ####
##########################################################

# Working directory
setwd("./") #*# This defaults to the project directory folder if run in an RProject in RStudio

## Updates made 7/20/23 by Hunter Parker

## Download STREAMCAT API data ------------------------------------------------

year <- "2019"

StreamLake_Var <- paste0("pctow", year, ",pctice", year, ",pcturbop", year, 
  ",pcturblo", year, ",pcturbmd", year, ",pcturbhi", year, ",pctbl", year, 
  ",pctdecid", year, ",pctconif", year, ",pctmxfst", year, 
  ",pctshrb", year, ",pctgrs", year, ",pcthay", year, ",pctcrop", year, 
  ",pctwdwet", year, ",pcthbwet", year, ",pctimp", year, "")
#"*" #You can add/remove variables here as desired, the data year can be changed by amending the year variable 

states <- c("WA")
cat_api <- data.frame()
ws_api <- data.frame()

# Download catchment data
for (i in (1:length(states))) {
  StreamCat_tempcat <- sc_get_data(metric=StreamLake_Var, aoi='catchment', state=states[i], showAreaSqKm = TRUE)
  cat_api <- rbind(cat_api, StreamCat_tempcat)
}

# Download watershed data
for (i in (1:length(states))) {
  StreamCat_tempws <- sc_get_data(metric=StreamLake_Var, aoi='watershed', state=states[i], showAreaSqKm = TRUE)
  ws_api <- rbind(ws_api, StreamCat_tempws)
}

### Format API data for consistency with other files ---------------------------

# Merge watershed and catchment level data by COMID
raw_api <- merge(cat_api,ws_api,by =c("comid", "wsareasqkm", "catareasqkm"))

# Format API data for use with RBEROST code
StreamCat_api <- raw_api %>%
  select(
    COMID = comid, CatAreaSqKm = catareasqkm, WsAreaSqKm=wsareasqkm,
    PctImp2019Cat = pctimp2019cat,
    PctImp2019Ws = pctimp2019ws,
    PctOw2019Cat = pctow2019cat,
    PctIce2019Cat = pctice2019cat,
    PctUrbOp2019Cat = pcturbop2019cat,
    PctUrbLo2019Cat = pcturblo2019cat,
    PctUrbMd2019Cat = pcturbmd2019cat,
    PctUrbHi2019Cat = pcturbhi2019cat,
    PctBl2019Cat = pctbl2019cat,
    PctDecid2019Cat=pctdecid2019cat,
    PctConif2019Cat=pctconif2019cat,
    PctMxFst2019Cat=pctmxfst2019cat,
    PctShrb2019Cat=pctshrb2019cat,
    PctGrs2019Cat=pctgrs2019cat,
    PctHay2019Cat=pcthay2019cat,
    PctCrop2019Cat=pctcrop2019cat,
    PctWdWet2019Cat=pctwdwet2019cat,
    PctHbWet2019Cat=pcthbwet2019cat,
    PctOw2019Ws=pctow2019ws,
    PctIce2019Ws=pctice2019ws,
    PctUrbOp2019Ws=pcturbop2019ws,
    PctUrbLo2019Ws=pcturblo2019ws,
    PctUrbMd2019Ws=pcturbmd2019ws, 
    PctUrbHi2019Ws=pcturbhi2019ws,
    PctBl2019Ws=pctbl2019ws,
    PctDecid2019Ws=pctdecid2019ws,
    PctConif2019Ws=pctconif2019ws,
    PctMxFst2019Ws=pctmxfst2019ws,
    PctShrb2019Ws=pctshrb2019ws,
    PctGrs2019Ws=pctgrs2019ws,
    PctHay2019Ws=pcthay2019ws,
    PctCrop2019Ws=pctcrop2019ws,
    PctWdWet2019Ws=pctwdwet2019ws,
    PctHbWet2019Ws=pcthbwet2019ws
  ) # %>%
  # relocate(State,.after=comid)%>%
  # relocate(PctImp2019Cat, .after=State)%>%
  # relocate(PctImp2019Ws, .after=PctImp2019Cat)


### View and Export Final Data Set ---------------------------------------------

#Remove intermediate step data frames and view final data set format
remove(list= "cat_api","raw_api","ws_api","StreamCat_tempcat","StreamCat_tempws","i","states","StreamLake_Var")

#You can export data set as csv file. This file will be stored to current working directory
write.csv(StreamCat_api, "RBEROST-Pacific/Preprocessing/Inputs/WA_streamcat_2019_cropland.csv", row.names = F)

