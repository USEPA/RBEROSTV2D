###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Load in updated precipitation, run off, and land use data for each COMID as well as updated cost and efficiency data ####
# BY: Kelly-Anne Moffa
# DATE:  11/2/2023 ####
###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~


###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 1. Setup -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

packages <- c(
  "sf", "sp", "dplyr", "data.table", "ggplot2", "purrr", "openxlsx", "stringr", "tidyr", "foreign", "openxlsx"
)
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}
invisible(
  suppressPackageStartupMessages(
    lapply(packages, library, character.only = TRUE)
  )
)

### Set working directory to location of "Tier_1_Optimization-SSWR.5.3.2" folder -----

source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R")

InPath <- ("./RBEROST-Pacific/Preprocessing/Inputs/")

sf::sf_use_s2(FALSE)



###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 2. Load Data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~


### User defined list of BMPs -----
user_specs_BMPs <- fread(
  paste(InPath,"01_UserSpecs_BMPs.csv",sep=""), data.table = FALSE
) 

#Pull out urban BMPs and make into list to filter future datasets by
urban_BMP <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "urban")

relevant_BMP <- urban_BMP %>%
  as_tibble() %>%
  dplyr::select(BMP) %>%
  pull() %>%
  unique()

# Replace "rain_garden6" and "rain_garden10" with "rain_garden" if selected
if ("Rain_Garden6" %in% relevant_BMP || "Rain_Garden10" %in% relevant_BMP) {
  relevant_BMP <- c(relevant_BMP, "Rain_Garden")
  relevant_BMP <- setdiff(relevant_BMP, c("Rain_Garden6", "Rain_Garden10"))
}


### 2a. Load in updated catchment data to maintain area information -----

catchments_upd <- read_sf("./Data/Catchment_alb_1711.shp")
catchments_temp <- read.dbf("./Data/Catchment_centroid_1711wcounty_alb.dbf")

match <- catchments_temp %>% filter(FEATUREID %in% catchments_upd$FEATUREID)

catchments <- merge(catchments_upd, catchments_temp) %>% rename(county = CNTY_NAME)


### 2b. Read in land value data and map numeric land use identifiers to character identifiers -----

land_value_temp <- read.dbf("./Data/AvLandValueft2xCountyxNLCD16.dbf") %>% 
  dplyr::select(nlcd_value = RASTERVALU, county = CNTY_NAME, FREQUENCY, land_val_sqft = MEAN_LandV)

land_value_sqft <- land_value_temp %>% filter(nlcd_value %in% c("21", "22", "23", "24"))

land_use_mapping <- data.frame(
  NumericValue = c(21, 22, 23, 24),
  LandUseCode = c("URLD", "URMD", "URHD", "UIDU")
)

land_value <- land_value_sqft %>% mutate(nlcd_value = case_when(
  nlcd_value == 21 ~ "URLD",
  nlcd_value == 22 ~ "URMD",
  nlcd_value == 23 ~ "URHD",
  nlcd_value == 24 ~ "UIDU",
  TRUE ~ as.character(nlcd_value)
))

land_value_acre <- land_value %>% mutate(land_val_acre = land_val_sqft/43560) %>% select(nlcd_value, county, land_val_acre)


### 2c. Read in run off and precipitation data -----

RO2d_in_tmp <- fread("./Data/PS_1983to2018RO2d_inv3.csv") 

# Anav_in_temp <- fread("./Data/PSppt_1983to2018anav.csv")
Anav_in_NEW_tmp <- fread("./Data/PSPPT_2002to2020yrxssn.csv")

#Identify missing catchment (run off and precip are both missing the same)
missing_ro <- catchments %>%
  filter(!FEATUREID %in% RO2d_in_tmp$FEATUREID)

missing_prec <- catchments %>%
  filter(!FEATUREID %in% Anav_in_NEW_tmp$Name)

## Using nearest neighbor matching, find precip values to fill in for missing comids

missing_cent <- st_centroid(missing_prec)

catchments_no_miss <- catchments %>%
  filter(!FEATUREID %in% missing_prec$FEATUREID)

catchment_cent <- st_centroid(catchments_no_miss)

# find nearest feature, from centroid of polygon
neighbor <- st_nearest_feature(missing_cent, catchment_cent)

neighbor_comid <- catchments_no_miss$FEATUREID[neighbor]

missing_to_neighbor <- data.frame(
  missing_COMID = missing_prec$FEATUREID,
  neighbor_COMID = neighbor_comid
)

matched_prec <- as.data.frame(Anav_in_NEW_tmp) %>%
  filter(Name %in% neighbor_comid) %>%
  merge(., missing_to_neighbor, by.x = "Name", by.y = "neighbor_COMID") %>%
  select(-Name) %>%
  rename(Name = missing_COMID)

Anav_in_NEW <- bind_rows(Anav_in_NEW_tmp, matched_prec)

matched_ro2d <- RO2d_in_tmp %>%
  filter(FEATUREID %in% neighbor_comid) %>%
  merge(., missing_to_neighbor, by.x = "FEATUREID", by.y = "neighbor_COMID") %>%
  select(-FEATUREID) %>%
  rename(FEATUREID = missing_COMID)

RO2d_in <- bind_rows(RO2d_in_tmp, matched_ro2d)

### 2d. Read in land use data to get land use by COMID and HSG -----

NLCD_bycomid_byHSG_temp <- fread("./Data/PugetSound_NLCDAreabycomidbyHSG.txt") %>%
  dplyr::select(ID, contains(c("21", "22", "23", "24")))

#*# Note: filled in the missing data with a variety of input files
NLCD_bycomid_byHSG_missing <- fread("./Data/PugetSound_NLCDAreabycomidbyHSG_fillmissing.txt") %>%
  dplyr::select(ID = IDCORR, contains(c("21", "22", "23", "24"))) %>%
  mutate(VALUE_23 = 0,
         VALUE_24 = 0)

MissingHSG_domHSG <- read.xlsx("./Data/Missinghsgwdomhsg.xlsx", ) %>%
  dplyr::select(COMID, HSG) %>%
  mutate(COMID = as.integer(COMID))
MissingHSG_closeHSG <- read.xlsx("./Data/MissingHSG_071123.xlsx") %>%
  dplyr::select(COMID, HSG) %>%
  mutate(COMID = as.integer(COMID)) %>%
  rbind(., MissingHSG_domHSG) %>%
  filter(!is.na(HSG)) %>%
  rename(HSG_upd = HSG)

NLCD_bycomid_byHSG <- rbind(NLCD_bycomid_byHSG_temp, NLCD_bycomid_byHSG_missing) %>%
  distinct(ID, .keep_all=T) %>%
  mutate(COMID = as.integer(sub("_.*", "", ID)),
         HSG_temp = sub(".*_", "", ID)) %>%
  dplyr::select(-ID) %>%
  merge(., MissingHSG_closeHSG, by="COMID", all.x=T) %>%
  mutate(HSG = case_when(
    HSG_temp=="" & !is.na(HSG_upd) ~ HSG_upd,
    HSG_temp=="" & is.na(HSG_upd) ~ NA_character_,
    TRUE ~ HSG_temp)
  ) %>%
  dplyr::select(-HSG_upd, -HSG_temp)


### 2e. Load in cost and efficiency data -----

# Read in costs (Cap, OM, min cap/OM, and rain garden)
cap_costs_temp <- read.xlsx("./Data/Capcost_finalparameters_092823_clean.xlsx", sheet="#LN16126", startRow=3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Intercept, mean_capcost, min_capcost, max_capcost, cap_rmse = Root.mean.squared.error, 
                Annav_capcost_coeff = AnnavPrecipin, ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2,
                ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4, bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)

cap_costs_temp$BMP.Type <- gsub(" ", "_", cap_costs_temp$BMP.Type)
cap_costs_temp$BMP.Type <- gsub("[/]", "_", cap_costs_temp$BMP.Type)

cap_costs <- cap_costs_temp %>%
  filter(BMP.Type %in% relevant_BMP)

colnames(cap_costs) <- ifelse(
  colnames(cap_costs) %in% c("BMP.Type", "Land_Use", "HSG", "mean_capcost",
                             "min_capcost", "max_capcost", "cap_rmse",
                             "Annav_capcost_coeff", "ro2d_capcost_coeff",
                             "Annav2_capcost_coeff", "ro2d2_capcost_coeff",
                             "bestAIC_capcost", "bestmodel_capcost",
                             "PrecOrder_capcost", "Ro2dOrder_capcost"), 
  colnames(cap_costs), 
  paste0("cap_", colnames(cap_costs))
)

bmp_count <- cap_costs %>%
  group_by(BMP.Type) %>%
  tally()

om_costs_temp <- read.xlsx("./Data/OMcost_finalparameters_092823_clean.xlsx", sheet="#LN16147", startRow=3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Intercept, mean_omcost, min_omcost, max_omcost, om_rmse = Root.mean.squared.error,
                Annav_omcost_coeff = AnnavPrecipin, ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2,
                ro2d2_omcost_coeff = ro2d_in2, bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel, PrecOrder_omcost = PrecOrder, Ro2dOrder_omcost = Ro2dOrder)

om_costs_temp$BMP.Type <- gsub(" ", "_", om_costs_temp$BMP.Type)
om_costs_temp$BMP.Type <- gsub("[/]", "_", om_costs_temp$BMP.Type)

om_costs <- om_costs_temp %>%
  filter(BMP.Type %in% relevant_BMP)

colnames(om_costs) <- ifelse(
  colnames(om_costs) %in% c("BMP.Type", "Land_Use", "HSG", "mean_omcost",
                             "min_omcost", "max_omcost", "om_rmse",
                             "Annav_omcost_coeff", "ro2d_omcost_coeff",
                             "Annav2_omcost_coeff", "ro2d2_omcost_coeff",
                             "bestAIC_omcost", "bestmodel_omcost",
                             "PrecOrder_omcost", "Ro2dOrder_omcost"), 
  colnames(om_costs), 
  paste0("om_", colnames(om_costs))
)


min_capcost <- read.xlsx("./Data/Mincapcostparam_statswq.xlsx", sheet = "#LN16161", startRow = 3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Intercept, mean_capcost, min_capcost, max_capcost, 
                Annav_capcost_coeff = AnnavPrecipin, min_cap_rmse = Root.mean.squared.error, 
                ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2, ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4,
                bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)

min_capcost$BMP.Type <- gsub(" ", "_", min_capcost$BMP.Type)
min_capcost$BMP.Type <- gsub("[/]", "_", min_capcost$BMP.Type)

colnames(min_capcost) <- ifelse(
  colnames(min_capcost) %in% c("BMP.Type", "Land_Use", "HSG",
                             "min_capcost", "min_cap_rmse"), 
  colnames(min_capcost), 
  paste0("min_cap_", colnames(min_capcost))
)

min_omcost <- read.xlsx("./Data/Minomcostparam_statswq.xlsx", sheet = "#LN16164", startRow = 3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Intercept, mean_omcost, min_omcost, max_omcost,
                min_om_rmse = Root.mean.squared.error, Annav_omcost_coeff = AnnavPrecipin, 
                ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2, ro2d2_omcost_coeff = ro2d_in2, 
                bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel, PrecOrder_omcost = PrecOrder, Ro2dOrder_omcost = Ro2dOrder)

min_omcost$BMP.Type <- gsub(" ", "_", min_omcost$BMP.Type)
min_omcost$BMP.Type <- gsub("[/]", "_", min_omcost$BMP.Type)

colnames(min_omcost) <- ifelse(
  colnames(min_omcost) %in% c("BMP.Type", "Land_Use", "HSG",
                               "min_omcost", "min_cap_rmse"), 
  colnames(min_omcost), 
  paste0("min_om_", colnames(min_omcost))
)


###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 3. Make matrix of comid x  HSG x Landcover types -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

rel_comids <- catchments %>%
  as_tibble() %>%
  dplyr::select(comid = FEATUREID) %>%
  pull() %>%
  unique()

relevantcomids <- catchments %>%
  dplyr::select(comid = FEATUREID, county)

all_HRUs_temp <- merge(NLCD_bycomid_byHSG, relevantcomids, by.x = "COMID", by.y = "comid")

all_qa <- all_HRUs_temp %>% as_tibble() %>%
  dplyr::select(COMID) %>%
  pull() %>%
  unique()

missing_hru <- catchments %>%
  filter(!FEATUREID %in% all_qa)
#*# Identified 15 catchments that drop out of the data, however they are all over the Canadian border. 

all_HRUs_prelim <- all_HRUs_temp %>%
  rename(URLD = VALUE_21,
         URMD = VALUE_22,
         URHD = VALUE_23,
         UIDU = VALUE_24
  ) %>%
  gather(., Land_Use, Area, URLD:UIDU) %>%
  filter(Area > 0)

all_HRUs <- merge(all_HRUs_prelim, land_value_acre, by.x = c("county", "Land_Use"), by.y = c("county", "nlcd_value"))

all_HRUs_with_precip_temp <- all_HRUs %>%
  merge(., RO2d_in %>% rename(ro2d_in = ro2d_in), by.x="COMID", by.y="FEATUREID", all.x=T) %>%
  merge(., Anav_in_NEW %>% rename(Ppt_in = pptyrxssn_in), by.x="COMID", by.y="Name", all.x=T) %>%
  dplyr::select(COMID:HSG, Area, land_val_acre, Year, season, ro2d_in, Ppt_in) 

length(unique(all_HRUs_with_precip_temp$COMID))

all_HRUs_with_precip <- all_HRUs_with_precip_temp %>%
  group_by(COMID, Land_Use, HSG, Year) %>%
  summarize(
    AnPpt_in = sum(Ppt_in, na.rm = TRUE),
    ro2d_in = mean(ro2d_in, na.rm = T),
    land_val_acre = mean(land_val_acre, na.rm = T),
    Area = sum(Area)
  )

length(unique(all_HRUs_with_precip$COMID))

# QA - how many comids do not have precip data? None.
all_HRUs_precip_QA <- all_HRUs_with_precip %>%
  filter(is.na(ro2d_in) | is.na(AnPpt_in) | is.na(land_val_acre) | is.na(Area))




###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 4. Make matrix of costs -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

Cost_matrix <- merge(cap_costs, om_costs, by=c("BMP.Type", "Land_Use", "HSG"), all.x=T)
min_cost_matrix <- merge(min_capcost, min_omcost, by=c("BMP.Type", "Land_Use", "HSG"), all.x=T)


all_HRUs_withcosts <- merge(all_HRUs_with_precip, Cost_matrix, by=c("Land_Use", "HSG"), all.x=T)

final_all_HRUs_withcosts <- merge(all_HRUs_withcosts, min_cost_matrix, by = c("BMP.Type", "Land_Use", "HSG"), all.x = T)


final_all_HRUs_withcosts <- final_all_HRUs_withcosts %>%
  mutate(across(
    .cols = !c(BMP.Type, Land_Use, HSG, contains("ModelLabel")),
    .fns = as.numeric
  ))



write.csv(final_all_HRUs_withcosts, paste0(InPath, "allHRUs_withcosts.csv"), row.names = F)

#*# The last .csv printed will be used in the next R file ("00_CostandEffic_Calc.R") to actually calculate costs #*#