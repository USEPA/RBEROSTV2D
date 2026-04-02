# 00_ExtractPSLowlandCOMID.R
# This program modifies the upstream COMIDs in 01_Preprocessing_Upstream_COMID.csv
# to only include those which overlap with the Puget Lowland Ecoregion in which
# most of the anthropogenic activities (agriculture, urbanization) are concentrated.
# N. Detenbeck
# Created 3/9/26
# Last updated 3/9/26
#####################################################################################

library(foreign)
library(dplyr)

# import list of Puget Sound COMIDs within Puget Lowlands ecoregion
PLE_COMIDs <- read.csv("C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/R10ROARFY24/GIS/Ecoregions/Catchment_alb_1711_PugetLowlandsEco.csv") %>%
  rename(catchment_comid = FEATUREID) %>% select(catchment_comid,SOURCEFC) %>% unique
# save unique comids only
PLE_COMIDs <- read.dbf("C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/R10ROARFY24/GIS/Ecoregions/Catchment_alb_1711_PugetLowlandsEco.dbf", as.is = FALSE)


# import 01_Preprocessing_Upstream_COMID.csv and edit to include only COMIDs overlapping
# with Puget Lowlands Ecoregion
PLE_UpstreamCOMIDs <- read.csv("C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Pacific/Preprocessing/Inputs/01_Preprocessing_Upstream_COMID.csv") %>%
left_join(PLE_COMIDs, by = ("catchment_comid")) %>% filter(!is.na(SOURCEFC)) %>% 
  select(catchment_comid, watershed_name) %>% unique

fileName <- "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Pacific/Preprocessing/Inputs/01_Preprocessing_Upstream_COMID_PugetLowlands.csv"
ret <- write.csv(x=PLE_UpstreamCOMIDs, file=fileName, row.names = FALSE)
print(ret)