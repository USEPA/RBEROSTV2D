### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# PURPOSE: Develop AMPL files for WMOST scaled-up optimization model screening tool 
# BY: Cathy Chamberlin, Kelly-Anne Moffa (ICF), Hunter Parker (ICF), Sam Ennett (ICF)
# DATE: 03/14/2022
# Updated DATE: November 2024
# Updated DATE: February 2026
# Naomi Detenbeck and Craig Connolly (EPA)
# Added septic, removed double-counting of riparian loads
# Referencing updated cost data for ag bmps and septic
# Updated WWTP upgrade costs from 2021USD to 2025USD
# 5/31/26 Added filter to only consider septic conversions w conversion efficiency > 0 (those close to WWTP)
# Fixed septic variable names
# ND 6/2/26 Redefined septicpoint_reduction_term to replace septic_reduction_term
# ND 6/3/26 Fixed frac variable lower boundaries and removed unnecessary :0s
# 01_Optimization_Preprocessing-Pacifi_V2_[PROD]_midloadobj.R
# ND 6/3/26 Added alternative objective to calculate minimum possible seasonal load
# This should be uncommented and cost objective should be commented to get min_load
# ND 6/9/26 Fixed transfer coefficient again to only subtract point not storage from total in denominator
# ND 6/15/26 Fixed transfer coefficient to use _ND variables in calculation
# ND 6/18/26 Added calculation and printing of total_load season x year to assist in 
# comparison with load_lim and min_load
# ND 6/19/26 Restricted riparian removal potential to account for prior removal by upland ag bmps
# ND 7/3/2026 Commented out alternative objective function
# ND 7/11/2026 Changed sorting order for .dat files to year,season instead of season,year
# so that storage calculations would be done correctly, referring correctly to the previous season
# ND 7/20/2026 Added zeros for totalbanklength and available buffer lengths for notrib catchments
# ND 7/20/2026 ND added road_removal_cap_N and _P
# ND 7/21/2026 addded if/then to cover cases of zero denominators
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Based on: RBEROST-Northeast preprocessor, initially created by ICF under contract to EPA
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# Notes, Introduction, and Set up -----

# General Notes:
# This Preprocessor is sourced through the RunRBEROST-Pacific.Rmd file.
# Installation of necessary packages is done through this RunRBEROST-Pacific.Rmd file as well.
# If more advanced R users would like to assess different locations/data, they would need to search for and update lines of code that end in "#*#" 


### ### ### ### ### ### ### ### ### 
# PART 1: USER SPECIFICATIONS ------
### ### ### ### ### ### ### ### ###

### I. Read in user specifications ------

# BMPS specifications: If BMP_Selection field = X, retain BMP for optimization. Otherwise ignore. 

user_specs_BMPs <- fread(
  paste(InPath, "01_UserSpecs_BMPs_wseptic.csv", sep = ""), data.table = FALSE,
  colClasses = list(numeric = c("capital_WA", "operations_WA"))
) #*# Updated costs to 2026USD for ag bmps, 2025USD for other management options

### II. Read in data ------
#base target data
# adjusted_targets <- read.csv(paste0(InPath,"01_UserSpecs_loadingtargets.csv")) %>% #*# 
adjusted_targets <- read.csv(paste0(InPath,"01_UserSpecs_loadingtargets_updatedwet.csv")) %>%
  select(-watershed_ID)

if (MODE=="All") {
  target_selection <- as.vector(adjusted_targets$watershed_name)
} else if (MODE=="Select") {
  target_selection <- watershed_choices
}

use_threshold <- FALSE #*# Carried over from LIS model to help calculate transfer coefficients
threshold <- 1e2

#terminal catchments/comids
terminal_comid <- read.csv(paste0(InPath, "01_Preprocessing_Terminal_COMID.csv")) %>% 
  dplyr::rename("terminal_comid"="comid") %>%
  mutate(
    conc=paste0(watershed_name, terminal_comid))

#all upstream/terminal catchments/comids
upstream_comid <- read.csv(paste0(InPath, "01_Preprocessing_Upstream_COMID.csv")) %>% 
  select(watershed_name, catchment_comid) %>%
  mutate(
    conc=paste0(watershed_name, catchment_comid))

#final dataframe used to pull data for the watersheds that the user wants
final_target_list <- upstream_comid %>%
  mutate(
    TerminalFlag=ifelse(conc %in% terminal_comid$conc, "X", NA)
  ) %>%
  select(-conc) %>%
  merge(., adjusted_targets, by="watershed_name", all.x=T) %>%
  filter(watershed_name %in% target_selection)

### III. Adjust loading targets based on user specified selection ------

user_specs_loadingtargets <- adjusted_targets %>%
  filter(watershed_name %in% target_selection)

#### CTC edited: 1/26/2026 & 2/10/2026 ####

user_specs_loadingtargets_sel <-
  adjusted_targets %>%
  filter(watershed_name %in% target_selection) %>%
  mutate(Number = match(watershed_name, unique(watershed_name)))

# write.csv(user_specs_loadingtargets_sel, paste0(InPath,"01_UserSpecs_loadingtargets_selected.csv"), row.names = F)
write.csv(user_specs_loadingtargets_sel, paste0(OutPath,"01_UserSpecs_loadingtargets_selected.csv"), row.names = F)

#### end edits ####

### IV. Establish load reduction goal ------
### Specify loading target goal. This percent reduction will be applied to total baseline loadings at specified pore point.
# Note to user: If TMDL is associated with a particular load value, calculate percent reduction required to meet target at pore point.
# load_perc_reduc is now specified in user_specs_loadingtargets

load_target_tn <- foreach(i = 1:length(target_selection)) %do% {
  temp_target <- user_specs_loadingtargets %>% 
    filter(TN_or_TP == "TN" & watershed_name == target_selection[i]) %>%
    summarize(Percent_Reduction = mean(TN_perc_reduction_target, na.rm = TRUE))
  
  temp_target$Percent_Reduction
}

load_target_tp <- foreach(i = 1:length(target_selection)) %do% {
  temp_target <- user_specs_loadingtargets %>% 
    filter(TN_or_TP == "TP" & watershed_name == target_selection[i]) %>%
    summarize(target_reduction = mean(adjusted_annual_target_kgperyr, na.rm = TRUE))
  
  temp_target$target_reduction
}

### ### ### ### ### ### ### ### ### 
# PART 2: PREPROCESS DATA INPUTS ------
### ### ### ### ### ### ### ### ### 

### I. Read raw data inputs ------

# NEW USGS SPARROW DATA (DYNAMIC) https://www.sciencebase.gov/catalog/item/65860a06d34eff134d437200
sparrow_in_DYN <- fread(paste(InPath,"data1_puget.csv",sep="")) #*#
sparrow_in_DYN$HUC_12_Rev <- str_pad(sparrow_in_DYN$HUC12, width=12, pad="0")  #*#
sparrow_in_DYN <- sparrow_in_DYN  %>%
  filter(year %in% year_xwalk$year) %>%
  rename(season = quarter)
# ND 7/19/26 addition to flag terminal catchments with no trib regardless of N/P selection
sparrow_notrib <- sparrow_in_DYN %>% select(comid,TermFlag,IncAreaKm2,CumAreaKm2) %>% 
  mutate(
    notrib = if_else(IncAreaKm2 == CumAreaKm2 & TermFlag == 0,1,0))  %>%
  select(comid,notrib)

# USGS Pacific SPARROW Model Output Data, 2020: https://www.sciencebase.gov/catalog/item/5d407318e4b01d82ce8d9b3c
message("Reading in SPARROW TN data...")
sparrow_cons_out_tn_pre_DYN <- fread(  #AK: modified to call tables "_pre" to preserve original
  paste(InPath, "predict_puget_tn.csv",sep=""), data.table = FALSE
) %>%
  rename(season = quarter) %>%
# replace DEL_FRAC of zero (coastal catchments w/out flowlines) with 1 to avoid zeroing out loads
  mutate(DEL_FRAC = if_else(DEL_FRAC == 0, 1, DEL_FRAC)) #*#
    
# QA Check: Do all years/seasons have the same number of COMIDs? -- yes
sparrow_cons_out_tn_pre_DYN %>%
  group_by(year) %>%
  tally()

message("Reading in SPARROW TP data...")
sparrow_cons_out_tp_pre_DYN <- fread(
  paste(InPath, "predict_puget_tp.csv",sep=""), data.table = FALSE
) %>%
  rename(season = quarter)

# QA Check: Do all years/seasons have the same number of COMIDs? -- yes
sparrow_cons_out_tp_pre_DYN %>%
  group_by(year) %>%
  tally()

# 60 COMIDS are updated by PARIS - old files are retained for all other COMIDS
paris_fed <- fread(
  paste(InPath, "MUN_FED_ALL_USGS.csv",sep=""), data.table = FALSE
)  %>% 
  mutate_all(as.character) #*#
paris_state <- fread(
  paste(InPath,"MUN_STATE_ALL_USGS.csv", sep=""), data.table = FALSE
)  %>% 
  mutate_all(as.character) #*#

paris <- paris_state %>% ## KM: updated this data to aggregate to seasons across all years provided (2005-2020 same time frame as SPARROW data)
  bind_rows(paris_fed) %>%
  mutate(season = case_when(
    month %in% c("1","2","3") ~ 1,
    month %in% c("4","5","6") ~ 2,
    month %in% c("7","8","9") ~ 3,
    month %in% c("10","11","12") ~ 4
  )) %>%
  filter(COMID != "None",
         TN_load_kg_month != "None") %>%
  rename(comid = COMID) %>%
  select(comid, year, season, TP_load_kg_month, TN_load_kg_month) %>%
  mutate_all(as.numeric) %>%
  group_by(comid, year, season) %>%
  summarize_all(sum, na.rm=T) %>%
  ungroup() %>%
  rename(TP_load_kg_season = TP_load_kg_month, TN_load_kg_season = TN_load_kg_month)

sparrow_cons_out_tn <- sparrow_cons_out_tn_pre_DYN %>%
  left_join(paris, by = c("comid", "year", "season")) %>%
  mutate(PLOAD_INC_POINT1_KG = ifelse(!is.na(TN_load_kg_season), TN_load_kg_season, PLOAD_INC_POINT1_KG)) %>%
  select(-TP_load_kg_season, -TN_load_kg_season)  %>%
  filter(year %in% year_xwalk$year)

# ND 6/5/26: Corrected ICF estimates of transfer coefficient by using previous season load in denominator
# and not including either point sources or storage load in the previous load term
# ND 6/15/26: Per commn with Noah Schmadel, we need to use the "_ND" version of these
# variables to avoid confounding storage calculations with effects of decay in reach except
# for coastal catchments w/out tribs which have no decay

# ### Separate by season
sparrow_cons_out_tn_s <- sparrow_cons_out_tn %>%
  arrange(comid, year, season) %>%
#  filter(comid == 23989375) %>%
  select(comid, year, season, PLOAD_INC_TOTAL_ND, PLOAD_INC_TOTAL, PLOAD_INC_STORAGE, PLOAD_INC_POINT1_KG,incarea,cumarea,termflag) %>%
  mutate(decay = PLOAD_INC_TOTAL/PLOAD_INC_TOTAL_ND,
         PLOAD_INC_STORAGE_ND = PLOAD_INC_STORAGE/decay,
         PLOAD_INC_POINT1_KG_ND = PLOAD_INC_POINT1_KG/decay,
         PLOAD_INC_TOTALMINUSPOINT_ND = PLOAD_INC_TOTAL_ND - PLOAD_INC_POINT1_KG_ND,
         PLOAD_INC_TOTALMINUSPOINT = PLOAD_INC_TOTAL - PLOAD_INC_POINT1_KG)
 
### Compute transfer coefficients
# ND Updated 6/4/26
# ND 6/4/26 transfer coefficient to lead to current storage is based on last season's load
# transfer coefficient for current season is applied to last season's load to get current storage
# Use season 4 as proxy for prior season for calculating coeff for first year season 1  %>%
# for coastal catchments w/out trib, no decay so don't use _ND versions
# drop lag for coastal catchments to avoid transfer coefficients > 1
sm_N <- sparrow_cons_out_tn_s %>%
  mutate(
    notrib = if_else(
      incarea == cumarea & termflag == 0, 1,0)
    ) %>%
  arrange(comid, year, season) %>%
  group_by(comid) %>%
  select(comid, year, season, PLOAD_INC_TOTAL_ND, PLOAD_INC_TOTALMINUSPOINT_ND, PLOAD_INC_STORAGE_ND,PLOAD_INC_STORAGE, PLOAD_INC_TOTALMINUSPOINT, notrib) %>%
  mutate(
    is_first = case_when(
      row_number() == 1 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
  mutate(
    coeff = if_else(notrib == 0 & is_first == TRUE,
                    PLOAD_INC_STORAGE_ND/lead(PLOAD_INC_TOTALMINUSPOINT_ND,3),
                    PLOAD_INC_STORAGE_ND/lag(PLOAD_INC_TOTALMINUSPOINT_ND,n=1))) %>%
  mutate(coeff = if_else(notrib == 1, PLOAD_INC_STORAGE/PLOAD_INC_TOTALMINUSPOINT, coeff, missing = NA)) %>%
  select(comid, year, season, coeff, notrib) # assuming constant fraction for storage on nontrib coastal catchments

### Overwrite year 20 transfer coefficients, no Y21S1 to compare to - set to 0
### ND: Overwrite year 1 transfer coefficients for season 1 no Y0S4 to compare to
### Assume previous winter load equal to current winter load
### Consistent with Schmadel et al. 2024 Supplemental Text 1

sm_N$coeff <- ifelse(sm_N$year == 2005 & sm_N$season == 1,
                     PLOAD_INC_STORAGE_ND / lead(PLOAD_INC_TOTALMINUSPOINT_ND,n=3), 
                     sm_N$coeff)

sm_N$coeff <- ifelse(sm_N$year == 2005 & sm_N$season == 1 & sm_N$notrib == 1,
                     PLOAD_INC_STORAGE/PLOAD_INC_TOTALMINUSPOINT, 
                     sm_N$coeff)

## Prep TP Data by Season ---------
sparrow_cons_out_tp <- sparrow_cons_out_tp_pre_DYN %>%
  left_join(paris, by = c("comid", "year", "season")) %>%
  mutate(PLOAD_INC_POINT1_KG = ifelse(!is.na(TP_load_kg_season), TP_load_kg_season, PLOAD_INC_POINT1_KG)) %>% # left join replaces old with new for 60 COMIDs that have new data
  select(-TP_load_kg_season, -TN_load_kg_season) %>%
  filter(year %in% year_xwalk$year)

# ND 6/5/26: Corrected ICF estimates of transfer coefficient by using previous season load in denominator
# and not including either point sources or storage load in the previous load term

sparrow_cons_out_tp_s <- sparrow_cons_out_tp %>% 
  arrange(comid, year, season) %>%
  select(comid, year, season, PLOAD_INC_TOTAL_ND, PLOAD_INC_TOTAL, PLOAD_INC_STORAGE, PLOAD_INC_POINT1_KG,incarea,cumarea,termflag) %>%
  mutate(decay = PLOAD_INC_TOTAL/PLOAD_INC_TOTAL_ND,
         PLOAD_INC_STORAGE_ND = PLOAD_INC_STORAGE/decay,
         PLOAD_INC_POINT1_KG_ND = PLOAD_INC_POINT1_KG/decay,
         PLOAD_INC_TOTALMINUSPOINT_ND = PLOAD_INC_TOTAL_ND - PLOAD_INC_POINT1_KG_ND,
         PLOAD_INC_TOTALMINUSPOINT = PLOAD_INC_TOTAL - PLOAD_INC_POINT1_KG)

sm_P <- sparrow_cons_out_tp_s  %>%
  mutate(
    notrib = if_else(
      incarea == cumarea & termflag == 0, 1,0)
  ) %>%
  arrange(comid, year, season) %>%
  group_by(comid) %>%
  select(comid, year, season, PLOAD_INC_TOTAL_ND, PLOAD_INC_TOTALMINUSPOINT_ND, PLOAD_INC_STORAGE_ND, PLOAD_INC_TOTALMINUSPOINT, PLOAD_INC_STORAGE,notrib) %>%
  mutate(
    is_first = case_when(
      row_number() == 1 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
  mutate(
    coeff = case_when(
      # if the total load is zero then the transferred storage will be zero * coeff of 1
      lag(PLOAD_INC_TOTALMINUSPOINT_ND) == 0 ~ 0,
      #      lead(PLOAD_INC_TOTAL, default = 0) == 0 ~ PLOAD_INC_STORAGE / 1,
      # ND:  if the next total load is zero then the transfer coefficient from the current total load = 0
      #      lead(PLOAD_INC_TOTAL, default = 0) == 0 ~ 0,
      #      TRUE ~ PLOAD_INC_STORAGE / PLOAD_INC_TOTAL
      # Use season 4 as proxy for calculating coeff for first year season 1
      is_first == TRUE ~ PLOAD_INC_STORAGE_ND/lead(PLOAD_INC_TOTALMINUSPOINT_ND,3),  
      # ND 6/4/26 transfer coefficient to lead to current storage is based on last season's load
      # transfer coefficient for current season is applied to last season's load to get current storage
      # for coastal catchments w/out trib, no decay so don't use _ND versions
      notrib == 1 ~ PLOAD_INC_STORAGE/PLOAD_INC_TOTALMINUSPOINT,
      TRUE ~ PLOAD_INC_STORAGE_ND/lag(PLOAD_INC_TOTALMINUSPOINT_ND,n=1)
    )
  ) %>%
  select(comid, year, season, coeff, notrib)

### Overwrite year 20 transfer coefficients, no Y21S1 to compare to - set to 0
### ND: Overwrite year 1 transfer coefficients for season 1 no Y0S4 to compare to
### Assume previous winter load equal to current winter load
### Consistent with Schmadel et al. 2024 Supplemental Text 1

sm_P$coeff <- ifelse(sm_P$year == 2005 & sm_P$season == 1,
                     PLOAD_INC_STORAGE_ND / lead(PLOAD_INC_TOTALMINUSPOINT_ND,n=3), 
                     sm_P$coeff)

sm_P$coeff <- ifelse(sm_P$year == 2005 & sm_P$season == 1 & sm_P$notrib == 1,
                     PLOAD_INC_STORAGE/PLOAD_INC_TOTALMINUSPOINT, 
                     sm_P$coeff)

# # Transfer coefficient threshold
if (use_threshold) {
  sm_N$coeff[sm_N$coeff > threshold] <- 0
  sm_P$coeff[sm_P$coeff > threshold] <- 0
}

# StreamCat Land Use Data, 2019: https://www.epa.gov/national-aquatic-resource-surveys/streamcat 
# Percent Cropland Data (NLCD2019)
croplandfiles <- lapply(
  str_subset(list.files(path = InPath, full.names = TRUE), "2019_cropland"), fread
)
names(croplandfiles) <- str_subset(list.files(path = InPath), "2019_cropland")

# Percent Imperviousness Data (ImperviousSurfaces2019)
impervfiles <- lapply(
  str_subset(list.files(path = InPath, full.names = TRUE), "imperv"), fread
)
names(impervfiles) <- str_subset(list.files(path = InPath), "imperv")

# Preprocessed Riparian Data
# ND 6/23/26 Use updated PS season x year urban riparian loadings
riparian.loadings <- fread(paste0(InPath, "RiparianLoadings_update.csv")) %>%
  rename(season = quarter)
riparian.existingbuffer <- fread(paste0(InPath, "LengthinBuffer_2016_update.csv"))
riparian.efficiencies <- fread(paste0(InPath, "RiparianEfficiencies_update.csv"))

# Updated in 2025 dynamic RBEROST update to use 2019 StreamCat data
cropland <- imap_dfr(
  croplandfiles, ~{return(.x %>% mutate(StateAbbrev = str_sub(.y, 1, 2)))}
) %>% arrange(COMID, StateAbbrev) %>%
  drop_na()

## already includes 2019 imperv data - dataset not updated in 2025 RBEROST update
imperv <- imap_dfr(
  impervfiles, ~{return(.x %>% mutate(StateAbbrev = str_sub(.y, 1, 2)))}
) %>% arrange(COMID, StateAbbrev) %>%
  drop_na()

# Perform data modifications for all reaches

# Change all COMID columns to lower case
cropland <- cropland %>%
  rename(comid = COMID)

imperv <- imperv %>%
  rename(comid = COMID)

### Specify incremental loads as six categories: Point sources, urban, roads, agricultural (which is cropland), grazing, and "other"
# Note to user: if making adjustment to SPARROW region, must identify column names that reflect agricultural, point, road, grazing, urban, and "other" 
# nutrient sources based on SPARROW regional model metadata. For example, the northeast model used the USGS metadata from the Northeast SPARROW model: 
# https://www.sciencebase.gov/catalog/item/5d4192aee4b01d82ce8da477 

### II. Specify N loads ------

temp_inc_tn <- sparrow_cons_out_tn %>% 
  select(c("comid", "year", "season", "PLOAD_INC_TOTAL":"PLOAD_INC_STORAGE")) %>%
  filter(year %in% year_filter)

tn_cols <- colnames(temp_inc_tn)
tn_cols <- tn_cols[grepl("PLOAD", tn_cols)]
se_tn_cols <- paste0("SE_", tn_cols)

temp_inc_tn$in_ag <- with(temp_inc_tn, PLOAD_INC_AG_KG + PLOAD_INC_CAFOS) #*#
temp_inc_tn$in_urb <- with(temp_inc_tn, PLOAD_INC_URB_KM2)
temp_inc_tn$in_road <- with(temp_inc_tn, PLOAD_INC_ATM_KG) #*#
temp_inc_tn$in_poin <- with(temp_inc_tn, PLOAD_INC_POINT1_KG)
temp_inc_tn$in_graz <- 0 #*# KM: Maintaining so structure is present if loads become significant in future SPARROW models
temp_inc_tn$in_septic <- with(temp_inc_tn, PLOAD_INC_SEPTIC) # CTC edited: 2/5/2026

temp_inc_tn_rev <- temp_inc_tn

temp_inc_tn_rev$in_road <- with(temp_inc_tn_rev, PLOAD_INC_ATM_KG)

# temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, PLOAD_INC_FOREIGN + PLOAD_INC_SEPTIC + PLOAD_INC_NFIX_M2) #*#
temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, PLOAD_INC_FOREIGN + PLOAD_INC_NFIX_M2) # CTC edited: 2/5/2026

temp_inc_tn_rev$in_storage <- with(temp_inc_tn_rev, PLOAD_INC_STORAGE) #*#

inc_tn <- temp_inc_tn_rev %>% 
  # select(c("comid", "year", "season", "in_poin", "in_urb", "in_road", "in_ag", "in_graz", "in_storage", "in_other")) #*#
  select(c("comid", "year", "season", "in_poin", "in_urb", "in_road", "in_ag", "in_graz", "in_septic", "in_storage", "in_other")) # CTC edited: 2/5/2026

### III. Specify P loads ------

temp_inc_tp <- sparrow_cons_out_tp %>%
  select(c("comid", "year", "season", "PLOAD_INC_TOTAL":"PLOAD_INC_STORAGE")) %>%
  filter(year %in% year_filter)

tp_cols <- colnames(temp_inc_tp)
tp_cols <- tp_cols[grepl("PLOAD", tp_cols)]
se_tp_cols <- paste0("SE_", tp_cols)

temp_inc_tp$ip_ag <- with(temp_inc_tp, PLOAD_INC_AG_KG + PLOAD_INC_CAFOS) #*#
temp_inc_tp$ip_urb <- with(temp_inc_tp, PLOAD_INC_URB_KM2)
temp_inc_tp$ip_poin <- with(temp_inc_tp, PLOAD_INC_POINT1_KG)
temp_inc_tp$ip_other <- with(temp_inc_tp, PLOAD_INC_FOREIGN+PLOAD_INC_NATP_MT)
temp_inc_tp$ip_storage <- with(temp_inc_tp, PLOAD_INC_STORAGE)
temp_inc_tp$ip_road <- 0 #*# KM: Maintaining, but setting to 0 for consistency with TN model
temp_inc_tp$ip_graz <- 0 #*# KM: Maintaining so structure is present if loads become significant in future SPARROW models
# temp_inc_tp$in_septic <- with(temp_inc_tp, PLOAD_INC_SEPTIC) # CTC edited: 2/5/2026
temp_inc_tp$ip_septic <- with(temp_inc_tp, PLOAD_INC_SEPTIC) # ND edited: 8/3/2026

inc_tp <- temp_inc_tp %>% 
  # select(c("comid", "year", "season", "ip_poin", "ip_urb", "ip_road", "ip_ag", "ip_graz", "ip_other", "ip_storage")) 
  # select(c("comid", "year", "season", "ip_poin", "ip_urb", "ip_road", "ip_ag", "ip_graz", "in_septic","ip_other", "ip_storage")) 
  select(c("comid", "year", "season", "ip_poin", "ip_urb", "ip_road", "ip_ag", "ip_graz", "ip_septic","ip_other", "ip_storage")) # ND 8/3/26

### IV. N point source loading ------
inc_tn_rev <- inc_tn

### V. P point source loading ------
inc_tp_rev <- inc_tp

### VI. Specify urban, road, and agricultural areas, change units from km2 to acres ------

# Select urban area and incremental area from SPARROW input data
temp_sparrow_area <- sparrow_in_DYN %>% 
  select(comid, IncAreaKm2, urb) %>%
  distinct() %>%
  mutate(urb = ifelse(is.na(urb), 0, urb),
         urban_km2 = IncAreaKm2 * (urb / 100)) %>%
  mutate(IncAreaKm2 = set_units(IncAreaKm2, "km^2"),
         urban_km2 = set_units(urban_km2, "km^2"))

# Select percentage of incremental area that is cropland
streamcat_crop <- cropland %>% select(c("comid", "PctCrop2019Cat", "PctHay2019Cat")) #*#
length(unique(streamcat_crop$comid)) #57,313 comids in streamcat

temp_sparrow_area <- temp_sparrow_area %>%
  mutate(urban_ac = urban_km2 %>% set_units("acres"),
         inc_ac = IncAreaKm2 %>% set_units("acres"))

temp_area <- merge(temp_sparrow_area, streamcat_crop, by = c("comid")) 
temp_area$ag_ac <- with(temp_area, (PctCrop2019Cat/100)*inc_ac) #*#
temp_area$graz_ac <- with(temp_area, (PctHay2019Cat/100)*inc_ac)

setDT(temp_area)

distinct_area <- temp_area[, .(
  urban_ac = mean(urban_ac),
  ag_ac = mean(ag_ac),
  graz_ac = mean(graz_ac)
), by = .(comid)]

# Read in the Ag baseline BMP %
ag_bmp_baseline_perc   <- fread(paste(InPath, "USGS_AgBMP_PugetSound.csv", sep = "")) #*#

# Calculate Ag baseline BMP Area
area <- distinct_area %>%
  merge(., ag_bmp_baseline_perc, by = "comid") %>%
  mutate(ag_ac_bmpadj = ag_ac - ((TOT_IMPV12_PERC / 100) * ag_ac)) %>%
  select(-TOT_IMPV12_PERC, -ag_ac) %>%
  relocate(ag_ac_bmpadj, .after = urban_ac)

distinct_area <- distinct(area) # Removes duplicates

distinct_area <- as.data.frame(distinct_area)

# Calculate road area (percent impervious)
road.area <- fread(
  paste0(InPath, "road_areas.csv"), data.table = FALSE
) %>%
  filter(!is.na(HSG))  # Filter out NA HSG values

road_port <- road.area %>%
  select(comid, HSG, area_port) %>%
  distinct()

road_area_temp <- imperv %>%
  select(c("comid","CatAreaSqKm","PctImp2019Cat")) %>% 
  merge(., road_port, by = "comid")

road_area <- road_area_temp %>%
  mutate(road_sqkm = CatAreaSqKm * (PctImp2019Cat/100) * area_port)

road_area$road_sqkm <- set_units(road_area$road_sqkm, km2)
road_area$road_ac <- set_units(road_area$road_sqkm, acre)
road_area <- road_area %>%
  select(comid, HSG, road_ac) %>%
  distinct()

#### CTC edited: 2/5/2026 - brought in from LIS dynamic RBEROST ####

## Septic Upgrades and Sewer Conversions Data -----

### Eventually want to convert this to a standalone input included
# class0ups <- readxl::read_xlsx(paste0(InPath, "Unclassified_Septic_Parcels_LIS.xlsx"))%>%
class0ups <- fread(paste0(InPath, "Unclassified_Septic_Parcels.csv"))%>%
  
  mutate(
    Upgrade_Efficiency = 0
  )%>%
  mutate(
    comid = as.character(comid),
    SepticUpgradeClass = as.integer(SepticUpgradeClass),
    SepticUpgradeParcels = as.integer(SepticUpgradeParcels),
    Upgrade_Efficiency = as.integer(Upgrade_Efficiency)
  )

### Number of COMID
length(unique(class0ups$comid))

### Read in septic upgrade data
septic.upgrade_raw <- fread(paste0(InPath, "01_RevSepticUpgrde_ParcelEff.csv")) %>%
  mutate(
    comid = as.character(comid)
  ) %>%
  mutate(SepticUpgradeClass = SeptUpgrCl)
length(unique(septic.upgrade_raw$comid))

### Merge in the number of class 0 parcels per COMID
septic.upgrade_raw <- bind_rows(class0ups%>%filter(comid %in% final_target_list$catchment_comid), septic.upgrade_raw%>%filter(comid %in% final_target_list$catchment_comid))
length(unique(septic.upgrade_raw$comid))

### Format septic upgrades data
septic.upgrade <- septic.upgrade_raw%>%
  rename(parcels = SepticUpgradeParcels, efficiency_per_parcel = Upgrade_Efficiency) %>%
  mutate(efficiency_per_parcel = efficiency_per_parcel/100)# Revise efficiency to be a decimal not a whole number

### Determine total parcels per COMID
septic.upgrade_totals <- septic.upgrade%>%
  group_by(comid)%>%
  summarise(
    total_parcels = sum(nparcels) # ND Fixed variable name
  )%>%
  ungroup()

### Remove class 0
septic.upgrade <- septic.upgrade %>%
  filter(SeptUpgrCl != 0) # ND fixed variable name

### Check to make sure there are no NAs in the data
septic_qa <- septic.upgrade_totals%>%
  summarise(across(everything(), ~sum(is.na(.)))) # All 0
rm(septic_qa) # Remove QA check

### Read in sewer conversion data
septic.conversion_raw <- fread(paste0(InPath, "01_RevSepticConv_ParcelEff.csv"))%>%
  filter(comid %in% final_target_list$catchment_comid)
septic.conversion_totals <- septic.conversion_raw%>%
  group_by(comid)%>%
  summarise(
    total_conv_parcels = sum(SewerConversionParcels),
  )
septic.conversion <- septic.conversion_raw%>%
  filter(SewerConversionClass != 0)%>%
  # ND: only consider parcels close enough to WWTP to have conversion efficiency ne zero;
  filter(Convert_Efficiency != 0)%>%  
  group_by(comid)%>%
  summarise(
    parcels = sum(SewerConversionParcels),
    efficiency_per_parcel = last(Convert_Efficiency)
  ) %>%
  # Revise efficiency to be a decimal not a whole number
  mutate(efficiency_per_parcel = efficiency_per_parcel/100)

total_septic_totals_temp <- merge(septic.upgrade_totals, septic.conversion_totals, by = "comid", all.x = TRUE, all.y = TRUE)

total_septic_totals_temp[is.na(total_septic_totals_temp)] <- 0

total_septic_totals <- total_septic_totals_temp%>%
  mutate(
    final_total = total_parcels + total_conv_parcels)%>%
  select(comid, final_total)
rm(total_septic_totals_temp)

#### end edits ####

### VII. Specify the length of stream bank already buffered ------
RiparianBuffer_BMPs <- with(
  user_specs_BMPs, BMP[which(BMP_Category == "ripbuf"& BMP_Selection == "X")]
) 

RiparianBuffer_Widths <- with(
  user_specs_BMPs, UserSpec_width_ft[
    which(BMP_Category == "ripbuf"& BMP_Selection == "X")
  ]
) 

UserSpecs_bufferwidth_nearest <- custom.round(
  x = RiparianBuffer_Widths, breaks = c(20, 40, 60, 80, 100)
)

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_maximp <- foreach(
    i = 1:length(RiparianBuffer_BMPs), .combine = "merge"
  ) %do% {
    RiparianBuffer_BMPs_tmp <- RiparianBuffer_BMPs[i]
    
    riparian.existingbuffer %>%
      select(
        comid, 
        totalbanklength_ft, 
        contains(as.character(UserSpecs_bufferwidth_nearest[i]))
      ) %>%
      rename_with(
        .fn = ~gsub(
          paste0("_", UserSpecs_bufferwidth_nearest[i], "ft_ft"), '', .
        ), 
        .col = contains(as.character(UserSpecs_bufferwidth_nearest[i]))
      ) %>%
      mutate(
        maximp = case_when(
          RiparianBuffer_BMPs_tmp == "Forested_Buffer" ~ 
            totalbanklength_ft - Forest_buffer,
          RiparianBuffer_BMPs_tmp == "Grassed_Buffer" ~ 
            totalbanklength_ft - Forest_buffer - Grass_buffer
        )
      ) %>%
      mutate(maximp = case_when(maximp < 0 ~ 0, maximp >= 0 ~ maximp)) %>%
      select(comid, totalbanklength_ft, maximp) %>%
      rename_with(.fn = ~paste(RiparianBuffer_BMPs_tmp), .cols = "maximp")
  }
}

### VIII. Calculate runoff coefficient for urban area ------

# Specify column for impervious dataset
temp_runoffcoeff <- imperv %>% select(c("comid","PctImp2019Cat")) #*#
temp_runoffcoeff$runoffcoeff <- with(
  temp_runoffcoeff, 0.05 + 0.009 * PctImp2019Cat
) #*#

runoffcoeff <- temp_runoffcoeff %>% select(c("comid", "runoffcoeff"))
runoffcoeff <- as.data.frame(distinct(runoffcoeff)) # Removes duplicates

# Remove temporary datasets
rm(list = ls(pattern = "^temp"))

### IX. Limit dataframe to the COMIDs within the specified watershed ------
reaches_all <- sparrow_in_DYN %>% 
  filter(
    comid %in% final_target_list$catchment_comid
  )

reaches_TN_target <- final_target_list %>%
  filter(TN_or_TP == "TN")

reaches_TP_target <- final_target_list %>%
  filter(TN_or_TP == "TP")

# # Add a error if the COMIDs in the final target list are not within the SPARROW model
if (all(!reaches_TN_target$catchment_comid %in% sparrow_cons_out_tn$comid) & nrow(reaches_TN_target) > 0) {
  missing_comids_tn <- reaches_TN_target$catchment_comid[!reaches_TN_target$catchment_comid %in% sparrow_cons_out_tn$comid]
  stop(paste0(
    "\n\nERROR: Some COMIDs in the TN target watersheds are not present in the SPARROW TN model data.\n",
    "Missing COMIDs: ", paste(unique(missing_comids_tn), collapse = ", "), "\n",
    "Total missing COMIDs: ", length(unique(missing_comids_tn)), "\n\n",
    "SOLUTION: Please verify that:\n",
    "1. The watershed selection includes only COMIDs present in the SPARROW model\n",
    "2. The SPARROW input files contain data for all selected watersheds\n",
    "3. The ComID values in the loading targets file match those in SPARROW data\n"
  ))
}

if (exists("reaches_TP_target") && nrow(reaches_TP_target) > 0) {
  if (all(!reaches_TP_target$catchment_comid %in% sparrow_cons_out_tp$comid)) {
    missing_comids_tp <- reaches_TP_target$catchment_comid[!reaches_TP_target$catchment_comid %in% sparrow_cons_out_tp$comid]
    stop(paste0(
      "\n\nERROR: Some COMIDs in the TP target watersheds are not present in the SPARROW TP model data.\n",
      "Missing COMIDs: ", paste(unique(missing_comids_tp), collapse = ", "), "\n",
      "Total missing COMIDs: ", length(unique(missing_comids_tp)), "\n\n",
      "SOLUTION: Please verify that:\n",
      "1. The watershed selection includes only COMIDs present in the SPARROW model\n",
      "2. The SPARROW input files contain data for all selected watersheds\n",
      "3. The ComID values in the loading targets file match those in SPARROW data\n"
    ))
  }
}

# Message regarding SPARROW/StreamCat mismatch

streamcat_subset_all <- as.data.frame(runoffcoeff)[
  runoffcoeff$comid %in% 
    unique(final_target_list$catchment_comid), 
] %>%
  select(comid) %>%
  arrange(comid)

streamcat_subset_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  streamcat_subset_tn_tmp <- as.data.frame(runoffcoeff)[
    runoffcoeff$comid %in% watershed_comid$catchment_comid, 
  ] %>%
    select(comid) %>%
    arrange(comid)
  
  streamcat_subset_tn_tmp
  
}

streamcat_subset_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  streamcat_subset_tp_tmp <- as.data.frame(runoffcoeff)[
    runoffcoeff$comid %in% watershed_comid$catchment_comid, 
  ] %>%
    select(comid) %>%
    arrange(comid)
  
  streamcat_subset_tp_tmp
  
}

Message.tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  if(
    length(unique(streamcat_subset_tn[[i]]$comid)) > 
    length(unique(watershed_comid$catchment_comid))
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TN target, there are more reaches in the provided StreamCat"
      ),
      "datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization.Loads from the remaining reaches will be included in", 
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else if(
    length(unique(streamcat_subset_tn[[i]]$comid)) < 
    length(unique(watershed_comid$catchment_comid))
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TN target, there are fewer reaches in the provided"
      ),
      "StreamCat datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(i == 1) {"st"} else if(i == 2) {"nd"} else {"rd"}, 
        " TN target, there are the same number of reaches in SPARROW and Streamcat"
      ),
      "subsetted datasets.", 
      "All reaches available for BMP optimization", 
      sep = "\n"
    )
  }
  
  Message.tmp
}
if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  invisible(lapply(Message.tn, cat, sep = "\n"))
}

Message.tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  if(
    length(unique(streamcat_subset_tp[[i]]$comid)) > 
    length(unique(watershed_comid$catchment_comid))
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are more reaches in the provided StreamCat"
      ),
      "datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization.Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else if(
    length(unique(streamcat_subset_tp[[i]]$comid)) < 
    length(unique(watershed_comid$catchment_comid))
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are fewer reaches in the provided"
      ),
      "StreamCat datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are the same number of reaches in SPARROW and Streamcat"
      ),
      "subsetted datasets.", 
      "All reaches available for BMP optimization", 
      sep = "\n"
    )
  }
  
  Message.tmp
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  invisible(lapply(Message.tp, cat, sep = "\n"))
}

### X. Determine riparian loadings ------

riparian.loadings_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  riparian.loadings %>% 
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, year, season, N_riparian_kgyr) %>%
    right_join(
      ., 
      sparrow_cons_out_tn %>% 
        filter(comid %in% watershed_comid$catchment_comid) %>% 
        select(comid, year, season, PLOAD_INC_TOTAL), 
      by = c("comid", "year", "season")
    ) %>%
    mutate(
      N_riparian_kgyr = case_when(
        N_riparian_kgyr > PLOAD_INC_TOTAL ~ PLOAD_INC_TOTAL, 
        is.na(N_riparian_kgyr) ~ 0, 
        TRUE ~ N_riparian_kgyr
      )
    ) %>%
    select(-PLOAD_INC_TOTAL)
}

riparian.loadings_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  riparian.loadings %>% 
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, year, season, P_riparian_kgyr) %>%
    right_join(
      ., 
      sparrow_cons_out_tp %>% 
        filter(comid %in% watershed_comid$catchment_comid) %>% 
        select(comid, year, season, PLOAD_INC_TOTAL), 
      by = c("comid", "year", "season")
    ) %>%
    mutate(
      P_riparian_kgyr = case_when(
        P_riparian_kgyr > PLOAD_INC_TOTAL ~ PLOAD_INC_TOTAL, 
        is.na(P_riparian_kgyr) ~ 0, 
        TRUE ~ P_riparian_kgyr
      )
    )  %>%
    select(-PLOAD_INC_TOTAL)
}

### XI. Adjust delivery fraction to pore point ------
# Specify maximum delivery fraction per watershed
max_delfrac_tn <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    if (nrow(watershed_comid) > 0) {
      max_val <- sparrow_cons_out_tn %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        summarise(max(DEL_FRAC, na.rm = TRUE)) %>%
        pull()
      
      # Ensure max_val is valid (not 0, NA, or Inf)
      # Use extremely small value (1) instead of 0 to avoid division by zero
      if (is.na(max_val) || is.infinite(max_val) || max_val == 0) {
        warning(paste0("Invalid max DEL_FRAC for watershed ", target_selection[i], " (TN). Using 1 to avoid division by zero."))
        1
      } else {
        max_val
      }
    } else { 1 }
    
  }
)

temp_delfrac_rev_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% watershed_comid$catchment_comid)  %>% 
    select(c("comid", "year", "season", "DEL_FRAC"))
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
    
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], DEL_FRAC / max_delfrac_tn[i]
    )
    
    # Validate delfrac_rev values:
    # - If Inf/NaN (from division by 0 or invalid max), use original DEL_FRAC
    # - If negative (shouldn't happen), use 0 for safety
    # - Otherwise use the normalized value
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- ifelse(
      is.infinite(temp_delfrac_rev_tn[[i]]$delfrac_rev) | 
      is.nan(temp_delfrac_rev_tn[[i]]$delfrac_rev),
      temp_delfrac_rev_tn[[i]]$DEL_FRAC,  # Fall back to original DEL_FRAC
      ifelse(
        temp_delfrac_rev_tn[[i]]$delfrac_rev < 0,
        0,  # Use 0 for negative values (safety check)
        temp_delfrac_rev_tn[[i]]$delfrac_rev  # Use normalized value
      )
    )
    
  }
)

delfrac_rev_tn <- foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
  
  temp_delfrac_rev_tn[[i]] %>%
    mutate(delfrac_rev = case_when(is.na(delfrac_rev) ~ DEL_FRAC, T ~ delfrac_rev)) %>%
    select(c("comid", "year", "season", "delfrac_rev"))
  
}

max_delfrac_tp <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TP_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    if (nrow(watershed_comid) > 0) {
      
      max_val <- sparrow_cons_out_tp %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        summarise(max(DEL_FRAC, na.rm = TRUE)) %>%
        pull()
      
      # Ensure max_val is valid (not 0, NA, or Inf)
      # Use extremely small value (1) instead of 0 to avoid division by zero
      if (is.na(max_val) || is.infinite(max_val) || max_val == 0) {
        warning(paste0("Invalid max DEL_FRAC for watershed ", target_selection[i], " (TP). Using 1 to avoid division by zero."))
        1
      } else {
        max_val
      }
      
    } else { 1 } 
    
  }
)

temp_delfrac_rev_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tp %>% filter(comid %in% watershed_comid$catchment_comid)  %>% 
    select(c("comid", "year", "season", "DEL_FRAC"))
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
    
    temp_delfrac_rev_tp[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], DEL_FRAC / max_delfrac_tp[i]
    )
    
    # Validate delfrac_rev values:
    # - If Inf/NaN (from division by 0 or invalid max), use original DEL_FRAC
    # - If negative (shouldn't happen), use 0 for safety
    # - Otherwise use the normalized value
    temp_delfrac_rev_tp[[i]]$delfrac_rev <- ifelse(
      is.infinite(temp_delfrac_rev_tp[[i]]$delfrac_rev) | 
      is.nan(temp_delfrac_rev_tp[[i]]$delfrac_rev),
      temp_delfrac_rev_tp[[i]]$DEL_FRAC,  # Fall back to original DEL_FRAC
      ifelse(
        temp_delfrac_rev_tp[[i]]$delfrac_rev < 0,
        0,  # Use 0 for negative values (safety check)
        temp_delfrac_rev_tp[[i]]$delfrac_rev  # Use normalized value
      )
    )
    
  }
)

delfrac_rev_tp <- foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
  
  temp_delfrac_rev_tp[[i]] %>%
    mutate(delfrac_rev = case_when(is.na(delfrac_rev) ~ DEL_FRAC, T ~ delfrac_rev)) %>%
    select(c("comid", "year", "season", "delfrac_rev"))
  
}

### XII. Define costs ------

ft2_to_ac <- measurements::conv_unit(1, "acre", "ft2")
yd2_to_ac <- measurements::conv_unit(1, "acre", "yd2")
km2_to_ac <- measurements::conv_unit(1, "acre", "km2") 

temp_bmp_costs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X") %>%
  select(
    BMP_Category, BMP, contains(c("capital", "operations")), UserSpec_width_ft
  )

### XIII. Grazing, ag, urban, road & riparian buffer costs ------

##### a. Convert area-specific costs to costs per acre for ag and grazing BMPs and to costs per linear foot for Riparian BMPs ------
# ND Modified calculations for riparian buffers - NRCS scenario is based on 50 ft buffer
temp_bmp_costs_ag_rev <- temp_bmp_costs %>%
  filter(BMP_Category != "point" & BMP_Category != "urban" & BMP_Category != "road") %>%
  mutate(
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~as.numeric(.)
    ),
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~replace_na(., NA_real_)
    ),
    across(
      .cols = c(contains(c("capital_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          capital_units == "ft2" ~ . * ft2_to_ac,
          capital_units == "km2" ~ . * km2_to_ac,
          capital_units == "yd2" ~ . * yd2_to_ac,
          capital_units == "ac" ~ .
        ),
        BMP_Category == "ripbuf" ~ case_when(
          capital_units == "ft2" ~ . * UserSpec_width_ft,
          capital_units == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units == "ac" ~ . * UserSpec_width_ft / 50
        ),
        BMP_Category == "graz" ~ case_when(
          capital_units == "ft2" ~ . * ft2_to_ac,
          capital_units == "km2" ~ . * km2_to_ac,
          capital_units == "yd2" ~ . * yd2_to_ac,
          capital_units == "ac" ~ .
        ),
      ), 
      .names = "{.col}_rev"
    ),
    across(
      .cols = c(contains(c("operations_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          operations_units == "ft2" ~ . * ft2_to_ac,
          operations_units == "km2" ~ . * km2_to_ac,
          operations_units == "yd2" ~ . * yd2_to_ac,
          operations_units == "ac" ~ .
        ),
        BMP_Category == "ripbuf" ~ case_when(
          operations_units == "ft2" ~ . * UserSpec_width_ft,
          operations_units == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_width_ft,
          operations_units == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_width_ft,
          operations_units == "ac" ~ .  * UserSpec_width_ft / 50
        ),
        BMP_Category == "graz" ~ case_when(
          operations_units == "ft2" ~ . * ft2_to_ac,
          operations_units == "km2" ~ . * km2_to_ac,
          operations_units == "yd2" ~ . * yd2_to_ac,
          operations_units == "ac" ~ .
        ),
      ), 
      .names = "{.col}_rev"
    )
  )

bmp_nonptsrc_costs <- temp_bmp_costs_ag_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev")
    )
  ) %>%
  rename(
    category = BMP_Category,
    bmp = BMP
  ) %>%
  rename_at(
    vars(contains(c("capital_", "operations_"))), list( ~ gsub("_rev", "", .))
  ) %>%
  # Annualize capital costs based on planning horizon and interest rate
  mutate(
    across(
      .cols = contains("capital_"), 
      .fns = ~(
        . * (
          interest_rate * ((1 + interest_rate) ^ horizon) / (
            ((1 + interest_rate) ^ horizon) - 1
          )
        )
      )
    )
  ) 

##### b. Specify point source costs by COMID ------

if(
  "point" %in% 
  (user_specs_BMPs %>% filter(BMP_Selection == "X") %>% .$BMP_Category)
) {
  point_comid <- fread(paste(InPath, "WWTP_COMIDs.csv", sep="")) #*#
  # Note to user: Can update this file if more WWTPs desired for analysis; must also update WWTP_Costs.csv with costs
  
  bmp_costs_ptsrc <- fread(paste(InPath, "WWTP_Costs_2025.csv", sep=""))
  
  temp_bmp_costs_point <- merge(
    bmp_costs_ptsrc, 
    point_comid,
    by = "COMID",
    all.x = TRUE
  ) %>%
    select(-contains("_se")) %>%
    pivot_longer(
      cols = -c(COMID, State, Plant_Name, NPDES_ID), 
      names_to = c("BMP", "CostType", "Costunits"), 
      names_pattern = "(.*)_(Capital|OM)_Cost_([[:digit:]]{4}dollars)",
      values_to = "Cost"
    )
  
  UserSelected_pointBMPs <- user_specs_BMPs %>%
    filter(BMP_Category == "point") %>%
    filter(BMP_Selection == "X") %>%
    mutate(
      BMP = case_when(
        BMP == "Fourstage_Bardenpho" ~ "FOURBDP",
        BMP == "Chem_add" ~ "C",
        BMP == "Denitrification_filters" ~ "DNF",
        BMP == "Tertiary_filters" ~ "F",
        BMP == "MeOH_addition" ~ "M",
        BMP == "Membrane_bioreactors" ~ "MBR",
        BMP == "Modified_Ludzack_Ettinger" ~ "MLE",
        BMP == "Sequencing_batch_reactor" ~ "SBR"
      )
    )
  
  temp_bmp_costs_point_selected <- temp_bmp_costs_point %>% 
    slice(
      which(
        unlist(
          map(
            .x = str_split(temp_bmp_costs_point$BMP, "_"), 
            .f = ~(all(.x %in% UserSelected_pointBMPs$BMP)))
        )
      )
    ) %>% # Update from 2021USD to 2025USD and annualize capital costs based on planning horizon and interest rate
    mutate(
      Cost = case_when(
        CostType == "Capital" ~ (
          1.1 * Cost * (
            interest_rate * ((1 + interest_rate) ^ horizon) / 
              (((1 + interest_rate) ^ horizon) - 1)
          )
        ),
        CostType == "OM" ~ 1.1 * Cost
      )
    )
  
  Point_BMPs <- unique(temp_bmp_costs_point_selected$BMP)
  
  if(length(Point_BMPs) > 0) {
    print(
      paste0(
        "Based on User Selections, the following combinations of BMPs are ",
        "considered for WWTP upgrades: ", 
        paste0(Point_BMPs, collapse = ", "),
        ". FOURBDP = Fourstage_Bardenpho, C = Chem_add, ",
        "DNF = Denitrification_filters, F = Tertiary_filters, M = MeOH_addition, ",
        "MBR = Membrane_bioreactors, MLE = Modified_Ludzack_Ettinger, and ",
        "SBR = Sequencing_batch_reactor."
      )
    )
    
    bmp_costs_point <- temp_bmp_costs_point_selected
  } else {
    print(
      paste0(
        "Based on User Selections, no WWTP upgrade BMPs will be considered. ",
        "The BMPs the User has selected may only be implemented in conjunction with unselected BMPs."
      )
    )
    
    bmp_costs_point <- data.frame(
      COMID = NA, State = NA, Plant_Name = NA, NPDES_ID = NA, BMP = NA, 
      CostType = NA, Costunits = NA, Cost = NA
    )
  }
  
} else {
  bmp_costs_point <- data.frame(
    COMID = NA, State = NA, Plant_Name = NA, NPDES_ID = NA, BMP = NA, 
    CostType = NA, Costunits = NA, Cost = NA
  )
  
  Point_BMPs <- user_specs_BMPs %>%
    filter(BMP_Selection == "X", BMP_Category == "point") %>%
    .$BMP
} 

##### c. Specify urban source costs by COMID ------
temp_Urban_BMPs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "urban")

Urban_BMPs <- paste0(temp_Urban_BMPs$BMP)

if(
  length(Urban_BMPs) > 0
) {
  
  urban.dat <- fread(
    paste0(InPath, "/UrbanBMPData_2025USD.csv"), data.table = FALSE
  ) #*# Updated costs to $2025
  
  #Transform urban areas from m^2 to acres 
  urban.dat$Area <- set_units(urban.dat$Area, m^2)
  urban.dat$acre_area <- set_units(urban.dat$Area, acres)
  
  # Create proportions of urban area that are of a certain land use and HSG combo
  urban.proportions.temp <- urban.dat %>%
    select(Land_Use, HSG, comid, acre_area) %>%
    distinct() %>%
    group_by(comid) %>%
    mutate(urban_area = sum(acre_area)) %>%
    ungroup()
  
  urban.proportions <- urban.proportions.temp %>%
    mutate(area_port = acre_area/urban_area) %>%
    unique()
 
  # Generate urban areas using proportions and streamcat data
  urban_areas_tmp <- distinct_area %>%
    select(comid, urban_ac) %>%
    merge(., urban.proportions)
  
  urban_areas <- urban_areas_tmp %>%
    mutate(urban_ac = urban_ac*area_port) %>%
    select(comid, Land_Use, HSG, urban_ac) 
  
  urban_areas <- urban_areas %>%
    mutate(urban_ac = drop_units(urban_ac))
  
  #Format urban costs
  temp_bmp_costs_urban <- urban.dat %>%
    select(
      comid, Land_Use, HSG, bmp = BMP, capital = capital_2025USD, operations = operations_2025USD
    ) %>% 
    drop_na() %>%
    filter(!is.na(HSG) & !is.na(Land_Use)) %>%  # Remove entries with NA HSG or Land_Use values
    filter(comid %in% c(unlist(streamcat_subset_all, use.names = FALSE)))
  
  
  bmp_costs_urban <- temp_bmp_costs_urban %>% 
    filter(bmp %in% Urban_BMPs) %>%
    # Annualize capital costs based on planning horizon and interest rate
    mutate(
      capital = (as.numeric(capital) * (
        interest_rate * ((1 + interest_rate) ^ horizon) / 
          (((1 + interest_rate) ^ horizon) - 1)
      )
      ),
      operations = as.numeric(operations)
    )
  
} else {
  bmp_costs_urban <- data.frame(
    comid = NA, bmp = NA, capital = NA, operations = NA
  )
} 

##### d. Specify road source costs by COMID ------
temp_road_BMPs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "road")

road_BMPs <- paste0(temp_road_BMPs$BMP)

if(
  length(road_BMPs) > 0
) {
  
  road.dat <- fread(
    paste0(InPath, "/road_bmps.csv"), data.table = FALSE
  )
  
  #Formatting road costs
  temp_bmp_costs_road <- road.dat %>%
    select(
      comid, bmp = BMP, HSG, capital = capital_2025USD, operations = operations_2025USD
    ) %>%
    drop_na() %>%
    filter(comid %in% c(unlist(streamcat_subset_all, use.names = FALSE)))
  
  
  bmp_costs_road <- temp_bmp_costs_road %>% 
    filter(bmp %in% road_BMPs) %>%
    # Annualize capital costs based on planning horizon and interest rate
    mutate(
      capital = (as.numeric(capital) * (
        interest_rate * ((1 + interest_rate) ^ horizon) / 
          (((1 + interest_rate) ^ horizon) - 1)
      )
      ),
      operations = as.numeric(operations)
    )
  
  if (length(bmp_costs_road$comid) == 0) {
    bmp_costs_road <- data.frame(
      comid = NA, bmp = NA, capital = NA, operations = NA
    )
  }
  
} else {
  bmp_costs_road <- data.frame(
    comid = NA, bmp = NA, capital = NA, operations = NA
  )
} 

##### e. Define the fraction of agricultural costs that reflect base payment versus actual agricultural BMP costs ------
agcost_frac <- 100/75

# Remove temporary datasets
rm(list = ls(pattern = "^temp"))


### XIV. Define efficiencies ------

##### a. Agricultural BMPs ------

# Read in efficiency data for Fert_20 and Manure_Injection BMPs
temp_ag_effic_fert_man <- fread(
  paste(InPath, "AgBMPEffic_nonACRE.csv", sep = "")
) #*#
temp_ag_effic_fert_man_cast_tn <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "N_Efficiency"
) #*#
temp_ag_effic_fert_man_cast_tp <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "P_Efficiency"
) #*#

# Read in efficiency data for ACRE database BMPs
temp_acre <- if(AgBMPcomparison == "No Practice") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareNoPractice.csv"))
} else if(AgBMPcomparison == "Baseline") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareBaseline.csv"))
} else {
  stop(
    'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
  )
}#*#
temp_acre$bmp <- with(
  temp_acre,
  ifelse(
    Scenario=="CONSERVATION",
    "Conservation",
    ifelse(
      Scenario=="Contour Farming",
      "Contour_Farming",
      ifelse(
        Scenario=="Fert 75%",
        "Fert_75",
        ifelse(
          Scenario=="Fert 90%",
          "Fert_90",
          ifelse(
            Scenario == "Terraces and Waterway",
            "Terrace_Waterway",
            ifelse(
              Scenario == "Terraces Only",
              "Terrace_Only",
              ifelse(
                Scenario=="Waterway Only", 
                "Waterway_Only", 
                as.character(Scenario)
              )
            )
          )
        )
      )
    )
  )
) #*#


temp_acre$Scenario <- NULL
temp_acre$HUC12_Rev <- str_pad(temp_acre$HUC12, width=12, pad="0")
temp_acre$HUC10_Rev <- str_pad(temp_acre$HUC10, width=10, pad="0")
temp_acre$HUC8_Rev <- str_pad(temp_acre$HUC8, width=8, pad="0")

temp_acre_cast_tn <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTN_Effic"
) %>% 
  {
    if(AgBMPcomparison == "Baseline") {select(., -"No Practice")} else if(
      AgBMPcomparison == "No Practice"
    ) {select(., -"Baseline")} else {
      stop(
        'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
      )
    } 
  }%>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tn_HUC8 <- temp_acre_cast_tn %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tn_HUC10 <- temp_acre_cast_tn %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tn_HUC12 <- temp_acre_cast_tn %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

non_bmp <- c(names(temp_acre_cast_tn %>% select(starts_with("HUC"))))

ACRE_bmps_tmp <- temp_acre_cast_tn %>% select(-c(all_of(non_bmp)))
ACRE_BMPs <- names(ACRE_bmps_tmp)

reaches_huc12_tn <- sparrow_in_DYN %>% 
  filter(comid %in% reaches_TN_target$catchment_comid) %>% 
  select(comid, year, season, HUC12 = HUC_12_Rev) %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tn <- merge(
  reaches_huc12_tn[, c("comid", "year", "season", "HUC12")],
  temp_acre_cast_tn_HUC12[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tn <- merge(
  reaches_huc12_tn[, c("comid", "year", "season", "HUC10")],
  temp_acre_cast_tn_HUC10[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tn <- merge(
  reaches_huc12_tn[, c("comid", "year", "season", "HUC8")],
  temp_acre_cast_tn_HUC8[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tn <- merge(
  merge(
    merge(
      reaches_huc12_tn, 
      temp_acre_reaches_HUC12_tn, 
      by = c("comid", "year", "season", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tn,
    by = c("comid", "year", "season", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tn,
  by = c("comid", "year", "season", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tn[ , ACRE_BMPs] <- NA

acre_reaches_tn <- temp_acre_reaches_tn %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, year, season, all_of(ACRE_BMPs))

temp_acre_cast_tp <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTP_Effic"
) %>% 
  {
    if(AgBMPcomparison == "Baseline") {select(., -"No Practice")} else if(
      AgBMPcomparison == "No Practice"
    ) {select(., -"Baseline")} else {
      stop(
        'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
      )
    } 
  }%>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tp_HUC8 <- temp_acre_cast_tp %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tp_HUC10 <- temp_acre_cast_tp %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tp_HUC12 <- temp_acre_cast_tp %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

reaches_huc12_tp <- sparrow_in_DYN %>% 
  filter(comid %in% reaches_TP_target$catchment_comid) %>% 
  select(comid, year, season, HUC12 = HUC_12_Rev)  %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tp <- merge(
  reaches_huc12_tp[, c("comid", "year", "season", "HUC12")],
  temp_acre_cast_tp_HUC12[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tp <- merge(
  reaches_huc12_tp[, c("comid", "year", "season", "HUC10")],
  temp_acre_cast_tp_HUC10[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tp <- merge(
  reaches_huc12_tp[, c("comid", "year", "season", "HUC8")],
  temp_acre_cast_tp_HUC8[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tp <- merge(
  merge(
    merge(
      reaches_huc12_tp, 
      temp_acre_reaches_HUC12_tp, 
      by = c("comid", "year", "season", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tp,
    by = c("comid", "year", "season", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tp,
  by = c("comid", "year", "season", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tp[ , ACRE_BMPs] <- NA

acre_reaches_tp <- temp_acre_reaches_tp %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, year, season, all_of(ACRE_BMPs))

##### b. Combine ACRE BMPs with non-ACRE BMPs ------
temp_Ag_BMPs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X" & BMP_Category == "ag")

Ag_BMPs <- paste0(temp_Ag_BMPs$BMP)

TileAg_BMPs <- user_specs_BMPs %>% 
  filter(
    BMP_Selection == "X" & BMP_Category == "ag" & TileDrainRestricted == "X"
  ) %>%
  .$BMP

CustomBMPmissingdata <- temp_ag_effic_fert_man %>% 
  filter(grepl("CustomAgBMP", BMP) & BMP %in% Ag_BMPs) %>%
  mutate(
    datacheck = map2(.x = N_Efficiency, .y = P_Efficiency, .f = ~sum(.x, .y))
  ) %>%
  filter(is.na(datacheck))

if(nrow(CustomBMPmissingdata) != 0) {
  stop(
    paste0(
      paste0(CustomBMPmissingdata$BMP, collapse = ", "), " is/are included by the user, ",
      "however there are missing efficiency data in the file AgBMPEffic_nonACRE.csv. ",
      "Please enter the relevant TN and/or TP removal efficiencies to this file. ",
      "If only one nutrient is being analyzed, the efficiency data for the other ",
      "may be set to 0."
    )
  )
}

ag_effic_bycomid_tn <- add_column(
  acre_reaches_tn, 
  temp_ag_effic_fert_man_cast_tn %>%
    filter(Category == "ag") %>%
    select(any_of(Ag_BMPs))
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  unique() %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.

ag_effic_bycomid_tp <- add_column(
  acre_reaches_tp, 
  temp_ag_effic_fert_man_cast_tp %>%
    filter(Category == "ag") %>%
    select(any_of(Ag_BMPs))
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  unique() %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.

##### c. Read tiled rained cropland ------
pct_tiled <- fread(paste0(InPath, "/PctCroplandTileDrained_ICF25.csv"))

##### d. Grazing BMPs ------
#*# KM: The costs and efficiencies for grazing BMPs are maintained in the data, however there will be no grazing loads that
#*# they will be applied to as SPARROW dropped them from load estimates. 
Graz_BMPs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "graz") %>%
  .$BMP

if(length(Graz_BMPs) > 0) {
  graz_effic_bycomid_tn <- data.frame(
    comid = sparrow_in_DYN %>% 
      filter(comid %in% reaches_TN_target$catchment_comid) %>% 
      .$comid
  ) %>%
    add_column(
      temp_ag_effic_fert_man_cast_tn %>%
        filter(Category == "graz") %>%
        select(any_of(Graz_BMPs))
    )
  
  graz_effic_bycomid_tp <- data.frame(
    comid = sparrow_in_DYN %>% 
      filter(comid %in% reaches_TP_target$catchment_comid) %>% 
      .$comid
  ) %>%
    add_column(
      temp_ag_effic_fert_man_cast_tp %>%
        filter(Category == "graz") %>%
        select(any_of(Graz_BMPs))
    )
}


##### e. Point source BMPs ------
if(length(Point_BMPs) > 0) {
  temp_point_effic <- fread(
    paste(InPath, "WWTP_RemovalEffic.csv", sep = "")
  ) #*#
  
  temp_bmp_effic_point <- merge(
    temp_point_effic, 
    point_comid,
    by = "COMID",
    all.x = TRUE
  ) %>%
    select(-contains("_se")) %>%
    pivot_longer(
      cols = -c(COMID, State, Plant_Name, NPDES_ID), 
      names_to = c("BMP", "Nutrient"), 
      names_pattern = "(.*)_(TN|TP)_Efficiency",
      values_to = "Efficiency"
    )
  
  temp_bmp_effic_point_selected <- temp_bmp_effic_point %>% 
    slice(
      which(
        unlist(
          map(
            .x = str_split(temp_bmp_effic_point$BMP, "_"), 
            .f = ~(all(.x %in% UserSelected_pointBMPs$BMP)))
        )
      )
    ) 
  
  point_effic <- temp_bmp_effic_point_selected %>%
    mutate(across(contains("Efficiency"), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.
  
  point_effic_bycomid_tn <- point_effic %>% 
    filter(Nutrient == "TN") %>%
    select(comid = COMID, BMP, effic = Efficiency)
  
  point_effic_bycomid_tp <- point_effic %>% 
    filter(Nutrient == "TP") %>%
    select(comid = COMID, BMP, effic = Efficiency)
  
} else {
  point_effic_bycomid_tn <- data.frame(
    comid = NA, BMP = NA, effic = NA
  )
  point_effic_bycomid_tp <- data.frame(
    comid = NA, BMP = NA, effic = NA
  )
}


##### f. Urban BMPs ------

if(length(Urban_BMPs) > 0) {
  urban.dat.effic <- fread(
    paste0(InPath, "/UrbanBMP_Effic_Data.csv"), data.table = FALSE
  )
  
  urban_comid <- urban.dat %>%
    select(comid, BMP, Land_Use, HSG) %>%
    unique()
  urban.effic.comid <- urban.dat.effic %>%
    merge(., urban_comid, by = c("BMP", "Land_Use", "HSG")) %>%
    rename(season = quarter)
  
  urban_effic.n <- merge(
    data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)),
    urban.effic.comid,
    by = "comid"
  )
  
  urban_effic_bycomid_tn <- urban_effic.n %>%
    select(comid, year, season, Land_Use, HSG, BMP, effic_N) %>%
    filter(BMP %in% Urban_BMPs,
           year %in% year_filter) %>%
    mutate(category = "urban") %>%
    select(
      category, comid, year, season, Land_Use, HSG, bmp = BMP, effic = effic_N
    ) %>%
    unique() %>%
    pivot_wider(
      id_cols = c(comid, year, season, Land_Use, HSG), names_from = bmp, values_from = effic
    ) %>%
    fill(any_of(Urban_BMPs)) %>% unnest(cols = 1:ncol(.)) %>%
    unique() %>%
    mutate(across(-c(comid, year, season, Land_Use, HSG), ~replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.
  
  urban_effic.p <- merge(
    data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)),
    urban.effic.comid,
    by = "comid"
  )
  
  if (length(urban_effic.p$comid > 0)) {
    urban_effic_bycomid_tp <- urban_effic.p %>%
      select(comid, year, season, Land_Use, HSG, BMP, effic_P) %>%
      filter(BMP %in% Urban_BMPs,
             year %in% year_filter) %>%
      mutate(category = "urban") %>%
      select(
        category, comid, year, season, Land_Use, HSG, bmp = BMP, effic = effic_P
      ) %>%
      unique() %>%
      pivot_wider(
        id_cols = c(comid, year, season, Land_Use, HSG), names_from = bmp, values_from = effic
      ) %>%
      fill(any_of(Urban_BMPs)) %>%
      unnest(cols = 1:ncol(.)) %>%
      unique() %>%
      mutate(across(-c(comid, year, season, Land_Use, HSG), ~replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.
  } else {
    urban_effic_bycomid_tp <- urban_effic.p %>%
      select(comid, year, season, Land_Use, HSG)
  }
}

##### g. Road BMPs ------

if(length(road_BMPs) > 0) {
  
  road.dat.effic <- fread(
    paste0(InPath, "/road_bmps_effic.csv"), data.table = FALSE
  )
  
  road_comid <- road.dat %>%
    select(comid, BMP, Land_Use, HSG) %>%
    unique()
  
  road.effic.comid <- road.dat.effic %>%
    merge(., road_comid, by = c("BMP", "Land_Use", "HSG")) %>%
    rename(season = quarter)
  
  # Create complete grid for TN efficiencies with ALL comid × year × season × HSG × BMP combinations
  complete_road_effic_tn <- data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)) %>%
    crossing(year = year_filter) %>%
    crossing(season = 1:4) %>%
    crossing(HSG = c("A", "B", "C", "D")) %>%
    crossing(bmp = road_BMPs)
  
  road_effic.n <- road.effic.comid %>%
    select(comid, year, season, HSG, BMP, effic_N) %>%
    filter(BMP %in% road_BMPs,
           year %in% year_filter) %>%
    mutate(category = "road") %>%
    select(
      category, comid, year, season, HSG, bmp = BMP, effic = effic_N
    )
  
  # Merge with complete grid and fill missing values with 0
  road_effic_bycomid_tn <- complete_road_effic_tn %>%
    left_join(road_effic.n, by = c("comid", "year", "season", "HSG", "bmp")) %>%
    select(-category) %>%
    pivot_wider(
      id_cols = c(comid, year, season, HSG), names_from = bmp, values_from = effic
    ) %>%
    mutate(across(any_of(road_BMPs), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data.
    unique()
  
  # Create complete grid for TP efficiencies with ALL comid × year × season × HSG × BMP combinations
  complete_road_effic_tp <- data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)) %>%
    crossing(year = year_filter) %>%
    crossing(season = 1:4) %>%
    crossing(HSG = c("A", "B", "C", "D")) %>%
    crossing(bmp = road_BMPs)
  
  road_effic.p <- road.effic.comid %>%
    select(comid, year, season, HSG, BMP, effic_P) %>%
    filter(BMP %in% road_BMPs,
           year %in% year_filter) %>%
    mutate(category = "road") %>%
    select(
      category, comid, year, season, HSG, bmp = BMP, effic = effic_P
    )
  
  # Merge with complete grid and fill missing values with 0
  road_effic_bycomid_tp <- complete_road_effic_tp %>%
    left_join(road_effic.p, by = c("comid", "year", "season", "HSG", "bmp")) %>%
    select(-category) %>%
    pivot_wider(
      id_cols = c(comid, year, season, HSG), names_from = bmp, values_from = effic
    ) %>%
    mutate(across(any_of(road_BMPs), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data.
    unique()
}

# Remove temporary datasets 
rm(list = ls(pattern = "^temp"))

### XV. Riparian buffer removals ------

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_efficiencies_N <- foreach(
    i = 1: length(streamcat_subset_tn)
  ) %do% {
    riparian_buffer_efficiencies_N_tmp <- foreach(
      j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
    ) %do% {
      
      short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
        "Grass"
      } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
      
      riparian.efficiencies.tmp <- riparian.efficiencies %>% 
        select(
          -contains("P"), 
          -V1, 
          -!contains(short.form.bmp.name), 
          -!contains(as.character(UserSpecs_bufferwidth_nearest[j])), 
          -contains("uncertainty"),
          comid
        ) %>%
        rename_with(.fn = ~"Curve.Form", .cols = contains("N")) %>%
        mutate(
          x = RiparianBuffer_Widths[j], 
          expression = gsub("y ~ ", '', Curve.Form)
        ) %>% rowwise() %>%
        mutate(iter = 1, effic = eval(parse(text = expression))) %>%
        select(comid, effic) %>%
        rename_with(.fn = ~paste(RiparianBuffer_BMPs[j]), .cols = "effic")
    }
    
    riparian_buffer_efficiencies_N_tmp %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .))
        
      )
  }
}

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_efficiencies_P <- foreach(
    i = 1: length(streamcat_subset_tp)
  ) %do% {
    riparian_buffer_efficiencies_P_tmp <- foreach(
      j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
    ) %do% {
      
      short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
        "Grass"
      } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
      
      riparian.efficiencies.tmp <- riparian.efficiencies %>% 
        select(
          -contains("N"), 
          -V1, 
          -!contains(short.form.bmp.name), 
          -!contains(as.character(UserSpecs_bufferwidth_nearest[j])), 
          -contains("uncertainty"),
          comid
        ) %>%
        rename_with(.fn = ~"Curve.Form", .cols = contains("P")) %>%
        mutate(
          x = RiparianBuffer_Widths[j], 
          expression = gsub("y ~ ", '', Curve.Form)
        ) %>% rowwise() %>%
        mutate(iter = 1, effic = eval(parse(text = expression))) %>%
        select(comid, effic) %>%
        rename_with(.fn = ~paste(RiparianBuffer_BMPs[j]), .cols = "effic")
    }
    
    riparian_buffer_efficiencies_P_tmp %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .))
      )
  }
  
  riparian_buffer_removal_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
    tmp <- left_join(
      riparian.loadings_tn[[i]], riparian.existingbuffer, by = "comid"
    ) %>%
      select(comid, year, season, N_riparian_kgyr, totalbanklength_ft) %>%
      mutate(
        loading_per_bankft_kg_ftyr = N_riparian_kgyr / totalbanklength_ft
      ) %>%
      left_join(., riparian_buffer_efficiencies_N[[i]], by = "comid") %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(
            . == -999 ~ 1e30,
            . > 0 ~ (. * loading_per_bankft_kg_ftyr)
          )
        )
      ) %>%
      select(comid, year, season, any_of(RiparianBuffer_BMPs))
    
  }
  
  riparian_buffer_removal_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
    left_join(
      riparian.loadings_tp[[i]], riparian.existingbuffer, by = "comid"
    ) %>%
      select(comid, year, season, P_riparian_kgyr, totalbanklength_ft) %>%
      mutate(
        loading_per_bankft_kg_ftyr = P_riparian_kgyr / totalbanklength_ft
      ) %>%
      left_join(., riparian_buffer_efficiencies_P[[i]], by = "comid") %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(
            . == -999 ~ 1e30, . > 0 ~ (. * loading_per_bankft_kg_ftyr)
          )
        )
      ) %>%
      select(comid, year, season, any_of(RiparianBuffer_BMPs))
    
  }
}

### XVI. Create dummy variables if modules aren't used ------

  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  urban_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))


  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    road_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    road_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  road_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))



  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    ripbuf_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    ripbuf_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  ripbuf_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}


  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    ag_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    ag_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  ag_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))



  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    point_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    point_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  point_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))



  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    graz_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    graz_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  graz_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))

#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  ## Specify septic BMPS ------------------------------------------------------
  temp_septic_BMPs <- user_specs_BMPs[
    user_specs_BMPs$BMP_Selection == "X" & (user_specs_BMPs$BMP_Category == "septic"),
  ]
  Septic_BMPs <- paste0(temp_septic_BMPs$BMP)
  
  # Set a flag to check if septic data is available
  septic_data_available <- FALSE
  
  # Check if septic BMPs are selected but no septic data exists
  if(length(Septic_BMPs) > 0) {
    
    # Check if septic data exists and has implementation potential
    total_septic_parcels <- 0
    
    # Check septic upgrade data
    if(exists("septic.upgrade") && nrow(septic.upgrade) > 0) {
      total_septic_parcels <- total_septic_parcels + sum(septic.upgrade$parcels, na.rm = TRUE)
      if(sum(septic.upgrade$parcels, na.rm = TRUE) > 0) {
        septic_data_available <- TRUE
      }
    }
    
    # Check septic conversion data
    if(exists("septic.conversion") && nrow(septic.conversion) > 0) {
      total_septic_parcels <- total_septic_parcels + sum(septic.conversion$parcels, na.rm = TRUE)
      if(sum(septic.conversion$parcels, na.rm = TRUE) > 0) {
        septic_data_available <- TRUE
      }
    }
    
    # Stop execution if septic BMPs are selected but no data is available
    if(!septic_data_available || total_septic_parcels == 0) {
      stop(paste0(
        "\n\nERROR: Septic BMPs are selected in user specifications but no septic implementation data is available in the watershed.\n",
        "Selected septic BMPs: ", paste(Septic_BMPs, collapse = ", "), "\n",
        "Total septic parcels available: ", total_septic_parcels, "\n\n",
        "SOLUTION: Please remove septic BMPs from your user specifications file (set BMP_Selection to blank instead of 'X' for septic BMPs) and re-run the analysis.\n",
        "Alternatively, verify that septic data files contain valid data for your watershed.\n"
      ))
    }
    
    # If we get here, septic data is available, proceed normally
    message(paste0("Septic data verified: ", total_septic_parcels, " septic parcels available for implementation."))
  }
  
  if(length(Septic_BMPs) == 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      septic_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
        comids <- unique(streamcat_subset_tn[[i]]$comid)
        if(length(comids) > 0) {
          data.frame(comid = comids, none = 0) %>%
            mutate(comid_form = paste0("'", comid, "'"))
        } else {
          data.frame(comid_form = character(0), none = numeric(0))
        }
      }
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      septic_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
        comids <- unique(streamcat_subset_tp[[i]]$comid)
        if(length(comids) > 0) {
          data.frame(comid = comids, none = 0) %>%
            mutate(comid_form = paste0("'", comid, "'"))
        } else {
          data.frame(comid_form = character(0), none = numeric(0))
        }
      }
    }
    septic_bmp_dummy <- data.frame(
      comid = unique(streamcat_subset_all), none = 0
    ) %>%
      mutate(comid_form = paste0("'", comid, "'"))
  }
  
#### end edits ####

### ### ### ### ### ### ### ### ### 
# PART 3: WRITE AMPL FILES ------
### ### ### ### ### ### ### ### ### 
if(
  length(Ag_BMPs) == 0 & length(Urban_BMPs) == 0 & length(Point_BMPs) == 0 & 
  length(RiparianBuffer_BMPs) == 0 & length(Graz_BMPs) == 0
) {
  stop(
    paste0(
      "No BMPs have been selected. ",
      "Please select at least one BMP with an 'X' in the third column in the ",
      "file 01_UserSpecs_BMPs.csv."
    )
  )
} else if(length(load_target_tn) == 0 & length(load_target_tp) == 0) {
  stop(
    paste0(
      "No Loading Targets have been selected. ",
      "Please enter at least one TN or TP target into the file ",
      "01_UserSpecs_loadingtargets.csv"
    )
  )
} else {


  # II. Develop data file ------
  
  ##### a. Write urban, land use x HSG, road, and agricultural BMPs as vectors ------

  #Urban BMP vectors
  bmp_urban_user <- user_specs_BMPs[
    user_specs_BMPs$BMP_Selection == "X" & 
      (user_specs_BMPs$BMP_Category == "urban"),
  ]
  bmp_urban_user$BMP <- gsub("/", "", bmp_urban_user$BMP)
  
  bmp_urban_direct <- unique(bmp_urban_user$BMP)
  
  bmp_urban <- gsub("/", "", bmp_urban_direct)
  bmp_urban_vec_comma <- paste0("'", bmp_urban, "'")
  bmp_urban_vec_comma[-length(bmp_urban_vec_comma)] <- paste0(
    bmp_urban_vec_comma[-length(bmp_urban_vec_comma)], ','
  )
  
  bmp_urban_vec <- as.character(gsub(",", "  ", bmp_urban_vec_comma))
  bmp_urban_vec_direct <- bmp_urban
  
  pervbmp_urban_vec_comma <- paste0(
    "'", with(bmp_urban_user, BMP[which(grepl("Porous_Pavement_w_UD", BMP))]), "'"
  )
  pervbmp_urban_vec_comma[-length(pervbmp_urban_vec_comma)] <- paste0(
    pervbmp_urban_vec_comma[-length(pervbmp_urban_vec_comma)], ','
  )
  pervbmp_urban_vec <- as.character(gsub(",", "  ", pervbmp_urban_vec_comma))

  # road HSG possibilities as vectors
  hsg <- c("A", "B", "C", "D")
  
  hsg_vec_comma <- paste0("'", hsg, "'")
  hsg_vec_comma[-length(hsg_vec_comma)] <- paste0(
    hsg_vec_comma[-length(hsg_vec_comma)], ','
  )
  
  hsg_vec <- as.character(gsub(",", "  ", hsg_vec_comma))
  hsg_vec_direct <- hsg
  
  # Land use as vectors
  land_use <- urban_areas %>% select(Land_Use) %>% unique()
  land_use <- land_use$Land_Use
  
  land_use_vec_comma <- paste0("'", land_use, "'")
  land_use_vec_comma[-length(land_use_vec_comma)] <- paste0(
    land_use_vec_comma[-length(land_use_vec_comma)], ','
  )
  
  land_use_vec <- as.character(gsub(",", "  ", land_use_vec_comma))
  land_use_vec_direct <- land_use
  
  # Road BMP vectors
  bmp_road_user <- user_specs_BMPs[
    user_specs_BMPs$BMP_Selection == "X" & 
      (user_specs_BMPs$BMP_Category == "road"),
  ]
  bmp_road_user$BMP <- gsub("/", "", bmp_road_user$BMP)
  
  bmp_road_direct <- unique(bmp_road_user$BMP)
  
  bmp_road <- gsub("/", "", bmp_road_direct)
  bmp_road_vec_comma <- paste0("'", bmp_road, "'")
  bmp_road_vec_comma[-length(bmp_road_vec_comma)] <- paste0(
    bmp_road_vec_comma[-length(bmp_road_vec_comma)], ','
  )
  
  bmp_road_vec <- as.character(gsub(",", "  ", bmp_road_vec_comma))
  bmp_road_vec_direct <- bmp_road
  
  # Ag BMP vectors
  bmp_ag <- user_specs_BMPs[
    user_specs_BMPs$BMP_Selection == "X" & (user_specs_BMPs$BMP_Category == "ag"),
  ]
  bmp_ag$BMP <- gsub("/", "", bmp_ag$BMP)
  bmp_ag_vec_comma <- paste0("'", bmp_ag$BMP, "'")
  bmp_ag_vec_comma[-length(bmp_ag_vec_comma)] <- paste0(
    bmp_ag_vec_comma[-length(bmp_ag_vec_comma)], ','
  )
  
  bmp_ag_vec <- as.character(gsub(",","  ", bmp_ag_vec_comma))
  bmp_ag_vec_direct <- bmp_ag$BMP
  
  
  if (is_empty(TileAg_BMPs)) {
    tilebmp_ag_vec_comma <- vector()
    tilebmp_ag_vec <- vector()
  } else{
    tilebmp_ag_vec_comma <- paste0(
      "'", with(bmp_ag, BMP[which(TileDrainRestricted == "X")]), "'"
    )
    tilebmp_ag_vec_comma[-length(tilebmp_ag_vec_comma)] <- paste0(
      tilebmp_ag_vec_comma[-length(tilebmp_ag_vec_comma)], ','
    )
    tilebmp_ag_vec <- as.character(gsub(",", "  ", tilebmp_ag_vec_comma))
  }
  
  # Riparian buffer BMP vectors
  bmp_ripbuf_vec_comma <- paste0("'", gsub("/", "",RiparianBuffer_BMPs), "'")
  bmp_ripbuf_vec_comma[-length(bmp_ripbuf_vec_comma)] <- paste0(
    bmp_ripbuf_vec_comma[-length(bmp_ripbuf_vec_comma)], ','
  )
  bmp_ripbuf_vec <- as.character(gsub(",","  ", bmp_ripbuf_vec_comma))
  
  # Point BMP vectors
  bmp_point_vec_comma <- paste0("'", gsub("/", "",Point_BMPs), "'")
  bmp_point_vec_comma[-length(bmp_point_vec_comma)] <- paste0(
    bmp_point_vec_comma[-length(bmp_point_vec_comma)], ','
  )
  bmp_point_vec <- as.character(gsub(",","  ", bmp_point_vec_comma))
  
  #Grazing BMP vectors
  bmp_graz_vec_comma <- paste0("'", gsub("/", "", Graz_BMPs), "'")
  bmp_graz_vec_comma[-length(bmp_graz_vec_comma)] <- paste0(
    bmp_graz_vec_comma[-length(bmp_graz_vec_comma)], ','
  )
  bmp_graz_vec <- as.character(gsub(",","  ", bmp_graz_vec_comma))
  
#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  #*# HP Septic Addition: 5-2-2025
  # Filter Septic_BMPs to only those available in the septic data
  available_septic_bmps <- character(0)
  
  # Add upgrade BMPs that are available in the septic.upgrade data
  if(exists("septic.upgrade") && nrow(septic.upgrade) > 0) {
    available_upgrade_classes <- unique(septic.upgrade$SepticUpgradeClass)
    available_upgrade_bmps <- paste0("class_", available_upgrade_classes, "_upgrade")
    available_septic_bmps <- c(available_septic_bmps, available_upgrade_bmps)
  }
  
  # Add sewer conversion BMP if available in the septic.conversion data
  if(exists("septic.conversion") && nrow(septic.conversion) > 0) {
    available_septic_bmps <- c(available_septic_bmps, "Sewer_convert")
  }
  
  # Filter Septic_BMPs to only include those available in the data and update the main variable
  Septic_BMPs <- Septic_BMPs[Septic_BMPs %in% available_septic_bmps]
  
  # Only proceed if we have valid septic BMPs after filtering
  if(length(Septic_BMPs) == 0) {
    warning("No septic BMPs are available in the data. Septic functionality will be disabled.")
  }
  
  bmp_septic_vec_comma <- paste0("'", Septic_BMPs, "'")
  if(length(Septic_BMPs) > 1) {
    bmp_septic_vec_comma[-length(bmp_septic_vec_comma)] <- paste0(
      bmp_septic_vec_comma[-length(bmp_septic_vec_comma)], ','
    )
  }
  bmp_septic_vec_direct <- Septic_BMPs
  bmp_septic_vec <- as.character(trimws(gsub(",","  ", bmp_septic_vec_comma)))
  
  #### end edits ####
  
  ##### b. Subset dataframes to reaches in the analysis ------
  
  # Create a complete list of all COMIDs that will be in the model
  all_model_comids <- data.frame(comid = unique(reaches_all$comid), stringsAsFactors = FALSE)
  all_model_comids$comid <- as.character(all_model_comids$comid)
  distinct_area$comid <- as.character(distinct_area$comid)
  
  temp_area_dat <- all_model_comids %>%
    left_join(distinct_area, by = "comid") %>%
    mutate(
      urban_ac = ifelse(is.na(urban_ac), 0, urban_ac),
      ag_ac_bmpadj = ifelse(is.na(ag_ac_bmpadj), 0, ag_ac_bmpadj),
      graz_ac = ifelse(is.na(graz_ac), 0, graz_ac)
    ) %>%
    arrange(comid)
  
  # Report COMIDs that were added with zero areas
  missing_area_comids <- temp_area_dat %>%
    anti_join(distinct_area, by = "comid") %>%
    pull(comid)
  
  if(length(missing_area_comids) > 0) {
    cat("\nNote: The following", length(missing_area_comids), "COMIDs are in the model but lack StreamCat land use data.\n")
    cat("Setting urban, agricultural, and grazing areas to 0 for these COMIDs:\n")
    cat(paste(head(missing_area_comids, 20), collapse = ", "))
    if(length(missing_area_comids) > 20) {
      cat(paste0(", ... and ", length(missing_area_comids) - 20, " more"))
    }
    cat("\n\n")
  }
  
  temp_area_dat$comid_form <- paste0("'", temp_area_dat$comid, "'")
  area_dat <- temp_area_dat %>% select(c("comid_form", "urban_ac", "ag_ac_bmpadj", "graz_ac"))
  
  # Filter area_dat to only include COMIDs that will be in streamcat_subset_all (comid_all set)
  # This prevents AMPL warnings about dropped subscripts for COMIDs not in the model
  area_dat <- area_dat %>%
    filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
  
  # Get complete list of urban land uses and HSGs from ALL available data (not just filtered by reaches_all)
  # This ensures that COMIDs in reaches_all but without urban data still get all combinations
  urban_land_use <- unique(urban_areas$Land_Use)
  urban_hsg <- unique(urban_areas$HSG)
  
  temp_urban_area <- urban_areas[urban_areas$comid %in% reaches_all$comid,]
  
  seasons <- unique(reaches_all$season)
  
  years <- as.numeric(unique(year_xwalk$model_year))
  
  complete_urban <- data.frame(
    comid = unique(reaches_all$comid)
  ) %>%
    crossing(., urban_land_use) %>%
    crossing(., urban_hsg)
  
    complete_urban_N <- data.frame(
      comid = unique(reaches_TN_target$catchment_comid)
    ) %>%
      crossing(., urban_land_use) %>%
      crossing(., urban_hsg) %>%
      crossing(., years) %>%
      crossing(., seasons) %>%
      rename(year = years, season = seasons)
  
    complete_urban_P <- data.frame(
      comid = unique(reaches_TP_target$catchment_comid)
    ) %>%
      crossing(., urban_land_use) %>%
      crossing(., urban_hsg) %>%
      crossing(., years) %>%
      crossing(., seasons) %>%
      rename(year = years, season = seasons)
  
  temp_urban_full <- temp_urban_area %>%
    merge(., complete_urban, by.x = c("comid", "Land_Use", "HSG"), by.y = c("comid", "urban_land_use", "urban_hsg"), all = T) %>%
    mutate(urban_ac = case_when(is.na(urban_ac) ~ 0, T ~ urban_ac))
# ND temp_urban_area has Land_Use and HSG surrounded by single quotes so will never
  # match up with temp_urban_full which doesn't have quoted Land_Use and HSG
  # Report COMIDs that were added with zero urban area
  missing_urban_comids <- temp_urban_full %>%
    anti_join(temp_urban_area, by = c("comid", "Land_Use", "HSG")) %>%
    pull(comid) %>%
    unique()
  
  if(length(missing_urban_comids) > 0) {
    cat("\nNote: The following", length(missing_urban_comids), "COMIDs are in the model but lack urban land use data.\n")
    cat("Setting urban areas to 0 for these COMIDs:\n")
    cat(paste(head(missing_urban_comids, 20), collapse = ", "))
    if(length(missing_urban_comids) > 20) {
      cat(paste0(", ... and ", length(missing_urban_comids) - 20, " more"))
    }
    cat("\n\n")
  }
  
  
  temp_urban_area <- temp_urban_full[order(temp_urban_full$comid, temp_urban_full$Land_Use, temp_urban_full$HSG),]
  
  
  temp_urban_area <- temp_urban_area %>% mutate(comid_form = paste0("'", comid, "'"), HSG = paste0("'", HSG, "'"), Land_Use = paste0("'", Land_Use, "'"))

  urban_area_dat <- temp_urban_area %>% select(comid_form, Land_Use, HSG, urban_ac)
  
  # Filter urban_area_dat to only include COMIDs that will be in streamcat_subset_all (comid_all set)
  # This prevents AMPL warnings about dropped subscripts for COMIDs not in the model
  urban_area_dat <- urban_area_dat %>%
    filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
  
  #urban_crosswalk <- temp_urban_area %>% select(combo_form, comid_form) %>% unique()
  
  # Get complete list of road HSGs - always include A, B, C, D regardless of what's in road_area
  # This ensures all COMIDs get all four HSG combinations, with missing data handled by high costs/zero efficiency
  road_hsg <- c("A", "B", "C", "D")
  
  temp_road_area <- road_area[road_area$comid %in% reaches_all$comid,]
  
  complete_road <- data.frame(
    comid = unique(reaches_all$comid)
  ) %>%
    crossing(., road_hsg)
  
    complete_road_N <- data.frame(
      comid = unique(reaches_TN_target$catchment_comid)
    ) %>%
      crossing(., road_hsg) %>%
      crossing(., years) %>%
      crossing(., seasons) %>%
      rename(year = years, season = seasons)
  
    complete_road_P <- data.frame(
      comid = unique(reaches_TP_target$catchment_comid)
    ) %>%
      crossing(., road_hsg) %>%
      crossing(., years) %>%
      crossing(., seasons) %>%
      rename(year = years, season = seasons)
  
  temp_road_full <- temp_road_area  %>%
    mutate(road_ac = drop_units(road_ac)) %>%
    merge(., complete_road, by.x = c("comid", "HSG"), by.y = c("comid", "road_hsg"), all = T) %>%
    mutate(road_ac = case_when(is.na(road_ac) ~ 0, T ~ road_ac))
  
  # Report COMIDs that were added with zero road area
  missing_road_comids <- temp_road_full %>%
    filter(road_ac == 0) %>%
    pull(comid) %>%
    unique()
  
  original_road_comids <- unique(temp_road_area$comid)
  new_road_comids <- setdiff(missing_road_comids, original_road_comids)
  
  if(length(new_road_comids) > 0) {
    cat("\nNote: The following", length(new_road_comids), "COMIDs are in the model but lack road area data.\n")
    cat("Setting road areas to 0 for these COMIDs:\n")
    cat(paste(head(new_road_comids, 20), collapse = ", "))
    if(length(new_road_comids) > 20) {
      cat(paste0(", ... and ", length(new_road_comids) - 20, " more"))
    }
    cat("\n\n")
  }
  
  temp_road_area <- temp_road_full[order(temp_road_full$comid, temp_road_full$HSG),]
  
  if (length(temp_road_area$comid) == 0) {
    temp_road_area <- data.frame(
      comid = unique(reaches_all$comid),
      HSG = NA,
      road_ac = 0
    )
    
    road_hsg <- unique(temp_road_area$HSG)
  }
  
  
  temp_road_area$comid_form <- paste0("'", temp_road_area$comid, "'")
  temp_road_area$HSG <- paste0("'", temp_road_area$HSG, "'")
  road_area_dat <- temp_road_area %>% select(comid_form, HSG, road_ac)
  
  # Filter road_area_dat to only include COMIDs that will be in streamcat_subset_all (comid_all set)
  # This prevents AMPL warnings about dropped subscripts for COMIDs not in the model
  road_area_dat <- road_area_dat %>%
    filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
  # ND 7/20/26 force notrib unbuffered and total bank lengths to be zero 
  if(length(RiparianBuffer_BMPs) > 0) {
    streambanklength_total_dat <- riparian_buffer_maximp %>% 
      full_join(., streamcat_subset_all, by = "comid") %>%
      filter(comid %in% streamcat_subset_all$comid) %>%
      arrange(comid) %>%
      mutate(
        comid_form = paste0("'", comid, "'"), 
        totalbanklength_ft = case_when(
          is.na(totalbanklength_ft) ~ 0, 
          !is.na(totalbanklength_ft) ~ totalbanklength_ft
        )
      ) %>% 
      left_join(sparrow_notrib,by = "comid") %>% #
      mutate(totalbanklength_ft = if_else(notrib == 1,0,totalbanklength_ft)) %>%      
      mutate(Grassed_Buffer = if_else(notrib == 1,0,Grassed_Buffer)) %>%
      mutate(Forested_Buffer = if_else(notrib == 1,0,Forested_Buffer)) %>%
      select(-notrib,-comid) %>%
      select(comid_form, totalbanklength_ft)
# ND 7/29/26 force notrib unbuffered lengths to be zero    
    streambanklength_available_dat <-  riparian_buffer_maximp %>% 
      full_join(., streamcat_subset_all, by = "comid") %>%
      filter(comid %in% streamcat_subset_all$comid) %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fns = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .)
        )
      ) %>% 
      select(comid,comid_form, any_of(RiparianBuffer_BMPs)) %>%
      left_join(sparrow_notrib,by = "comid") %>% #
      mutate(Grassed_Buffer = if_else(notrib == 1,0,Grassed_Buffer)) %>%
      mutate(Forested_Buffer = if_else(notrib == 1,0,Forested_Buffer)) %>%
      select(-notrib,-comid)
  }

  # ND 7/20/26 Create riparian length entries of zero for watersheds with no tribs
  if(nrow(streambanklength_available_dat) == 0) {
   streambanklength_available_dat <- COMID_State %>%
   mutate(comid_form = paste0("'", comid, "'")) %>%
   mutate(Grassed_Buffer = 0.0, Forested_Buffer = 0.0) %>%
     arrange(comid) %>%
     select(-State,-comid)
  }
  if(nrow(streambanklength_total_dat) == 0) {
    streambanklength_total_dat <- COMID_State %>%
      mutate(totalbanklength_ft = 0.0, Grassed_Buffer = 0.0, Forested_Buffer = 0.0) %>%
      arrange(comid) %>%
      select(-State)
  }
  
  temp_runoffcoeff_dat <- as.data.frame(runoffcoeff)[
    runoffcoeff$comid %in% streamcat_subset_all$comid,
  ]
  temp_runoffcoeff_dat <- temp_runoffcoeff_dat[order(temp_runoffcoeff_dat$comid),]
  temp_runoffcoeff_dat$comid_form <- paste0("'", temp_runoffcoeff_dat$comid, "'")
  runoffcoeff_dat <- temp_runoffcoeff_dat %>% 
    select(c("comid_form", "runoffcoeff"))
  

  ##### c. Specify state COMIDs ------

  COMID_State <- distinct(
    imperv[
      imperv$comid %in% streamcat_subset_all$comid, c("comid", "StateAbbrev")
    ]
  ) %>% 
    group_by(comid) %>%
    arrange(StateAbbrev) %>%
    summarize(State = StateAbbrev[1]) #*#
  

  ##### d. Multiply all loads by revised del_frac ------
  
  temp_inc_tn_dat <- foreach(i = 1:length(delfrac_rev_tn)) %do% {
    
    temp_inc_tn_dat_tmp <- merge(
      inc_tn_rev[inc_tn_rev$comid %in% delfrac_rev_tn[[i]]$comid,],
      delfrac_rev_tn[[i]],
      by = c("comid", "year", "season"),
      all.x = TRUE
    ) %>%
      unique() # Remove duplicates
    
    temp_inc_tn_dat_tmp[-c(temp_inc_tn_dat_tmp$comid, temp_inc_tn_dat_tmp$season, temp_inc_tn_dat_tmp$year, temp_inc_tn_dat_tmp$delfrac_rev)] <- temp_inc_tn_dat_tmp[-c(temp_inc_tn_dat_tmp$comid, temp_inc_tn_dat_tmp$season, temp_inc_tn_dat_tmp$year, temp_inc_tn_dat_tmp$delfrac_rev)] * 
      temp_inc_tn_dat_tmp[["delfrac_rev"]]
    
    
    temp_inc_tn_dat_tmp <- temp_inc_tn_dat_tmp[
      order(temp_inc_tn_dat_tmp$comid), 
    ]
    
  }
  
  temp_inc_tp_dat <- foreach(i = 1:length(delfrac_rev_tp)) %do% {
    
    temp_inc_tp_dat_tmp <- merge(
      inc_tp[inc_tp$comid %in% delfrac_rev_tp[[i]]$comid,],
      delfrac_rev_tp[[i]],
      by = c("comid", "year", "season"),
      all.x = TRUE
    ) %>%
      unique() # Remove duplicates
    
    temp_inc_tp_dat_tmp[-c(temp_inc_tp_dat_tmp$comid, temp_inc_tp_dat_tmp$season, temp_inc_tp_dat_tmp$year, temp_inc_tp_dat_tmp$delfrac_rev)] <- temp_inc_tp_dat_tmp[-c(temp_inc_tp_dat_tmp$comid, temp_inc_tp_dat_tmp$season, temp_inc_tp_dat_tmp$year, temp_inc_tp_dat_tmp$delfrac_rev)] * 
      temp_inc_tp_dat_tmp[["delfrac_rev"]]
    
    
    temp_inc_tp_dat_tmp <- temp_inc_tp_dat_tmp[
      order(temp_inc_tp_dat_tmp$comid), 
    ]
    
  }
  
  temp_sm_N_dat <- foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i])
    
    temp_sm_N_dat_tmp <- sm_N[sm_N$comid %in% watershed_comid$catchment_comid,] %>%
      arrange(comid)
    
  }
  
  temp_sm_P_dat <- foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TP_target %>%
      filter(watershed_name == target_selection[i])
    
    temp_sm_P_dat_tmp <- sm_P[sm_P$comid %in% watershed_comid$catchment_comid,] %>%
      arrange(comid)
  }
  
  
  if(length(RiparianBuffer_BMPs) > 0) {
    
    temp_riparian_tn_dat <- foreach(i = 1:length(riparian.loadings_tn)) %do% {
      
      temp_riparian_tn_dat_tmp <- merge(
        riparian.loadings_tn[[i]],
        delfrac_rev_tn[[i]],
        by = c("comid", "year", "season"),
        all = TRUE
      ) %>%
        mutate(in_riparian = N_riparian_kgyr * delfrac_rev) %>%
        arrange(comid) %>%
        select(comid, year, season, in_riparian) %>% unique()
      
    }
    
    temp_riparian_tp_dat <- foreach(i = 1:length(riparian.loadings_tp)) %do% {
      
      temp_riparian_tp_dat_tmp <- merge(
        riparian.loadings_tp[[i]],
        delfrac_rev_tp[[i]],
        by = c("comid", "year", "season"),
        all = TRUE
      ) %>%
        mutate(ip_riparian = P_riparian_kgyr * delfrac_rev) %>%
        arrange(comid) %>%
        select(comid, year, season, ip_riparian) %>% unique()
      
    }
    
    temp_riparian_tn_removal <- foreach(i = 1:length(riparian_buffer_removal_N)) %do% {
      
      temp_riparian_tn_removal_tmp <- merge(
        riparian_buffer_removal_N[[i]],
        delfrac_rev_tn[[i]],
        by = c("comid", "year", "season"),
        all = TRUE
      ) %>%
        mutate(
          across(
            .col = any_of(RiparianBuffer_BMPs), 
            .fn =  ~case_when(is.na(.) ~ 0, !is.na(.) ~ . * delfrac_rev) # Set to 0 for missing values
          )
        ) %>%
        arrange(comid) %>%
        select(comid, year, season, any_of(RiparianBuffer_BMPs))
      
    }
    
    temp_riparian_tp_removal <- foreach(i = 1:length(riparian_buffer_removal_P)) %do% {
      
      temp_riparian_tp_removal_tmp <- merge(
        riparian_buffer_removal_P[[i]],
        delfrac_rev_tp[[i]],
        by = c("comid", "year", "season"),
        all = TRUE
      ) %>%
        mutate(
          across(
            .col = any_of(RiparianBuffer_BMPs), 
            .fn =  ~case_when(is.na(.) ~ 0, !is.na(.) ~ . * delfrac_rev)
          )
        ) %>%
        arrange(comid) %>%
        select(comid, year, season, any_of(RiparianBuffer_BMPs))
      
    }
  }
  
  ##### e. Specify data to COMIDs within/ not within StreamCat Dataset ------

  temp_inc_tn_dat_SC <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
    temp_inc_tn_dat[[i]][
      temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid, 
    ]
  }
  
  temp_inc_tn_dat_other <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
    temp_inc_tn_dat[[i]][
      !(temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid), 
    ]
  }
  
  temp_inc_tp_dat_SC <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
    temp_inc_tp_dat[[i]][
      temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid, 
    ]
  }
  
  temp_inc_tp_dat_other <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
    temp_inc_tp_dat[[i]][
      !(temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid), 
    ]
  }
  
  # (SC for StreamCat)
  temp_sm_N_dat_SC <- foreach(i = 1:length(temp_sm_N_dat)) %do% {
    temp_sm_N_dat[[i]][
      temp_sm_N_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid,
    ]
  }
  
  temp_sm_N_dat_other <- foreach(i = 1:length(temp_sm_N_dat)) %do% {
    temp_sm_N_dat[[i]][
      !(temp_sm_N_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid),
    ]
  }
  
  # (SC for StreamCat)
  temp_sm_P_dat_SC <- foreach(i = 1:length(temp_sm_P_dat)) %do% {
    temp_sm_P_dat[[i]][
      temp_sm_P_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid,
    ]
  }
  
  temp_sm_P_dat_other <- foreach(i = 1:length(temp_sm_P_dat)) %do% {
    temp_sm_P_dat[[i]][
      !(temp_sm_P_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid),
    ]
  }
}  
##### f. Calculate sums of loads for optimization ------
  ## Calculate sums of loads for optimization ----- 
# ND removed in_riparian to avoid double-counting  
  if(actionable_load == TRUE){
    invisible(
      if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
        param_loads_lim_tn <-
          foreach(i = 1:length(temp_inc_tn_dat)) %do% {
            
#            riparian_load <- temp_riparian_tn_dat[[i]]
#ND 6/18/26 save total load season x year            
            load_lim_temp <- temp_inc_tn_dat[[i]] %>% 
#              merge(riparian_load, by = c("comid", "season", "year")) %>%
              group_by(year, season) %>%
              
              
              summarize(total_load = sum(in_poin, na.rm = TRUE) + sum(in_urb, na.rm = TRUE) + 
                          sum(in_septic, na.rm = TRUE) + sum(in_ag, na.rm = TRUE) + sum(in_graz, na.rm = TRUE) + 
                          sum(in_road, na.rm = TRUE) + sum(in_storage, na.rm = TRUE),
                        # + sum(in_riparian),#+ sum(in_other) # CTC edited: 2/5/2026 ND edited 2/23/26
                        load_lim = (1 - as.numeric(load_target_tn[i])) * (sum(in_poin, na.rm = TRUE) + 
                          sum(in_urb, na.rm = TRUE) + sum(in_septic, na.rm = TRUE) + sum(in_ag, na.rm = TRUE) + 
                            sum(in_graz, na.rm = TRUE) + sum(in_road, na.rm = TRUE) + sum(in_storage, na.rm = TRUE)),
                        # + sum(in_riparian)),# + sum(in_other) # CTC edited: 2/5/2026 ND 2/23/26
                        .groups = "drop"
              )
            load_lim <- load_lim_temp %>%
              select(season, year, load_lim) %>%
              merge(., year_xwalk, by = "year") %>%
              select(-year) %>%
              rename(year = model_year)
          }

        param_total_loads_tn <-
          foreach(i = 1:length(temp_inc_tn_dat)) %do% {
            
#            riparian_load <- temp_riparian_tn_dat[[i]]
            
            load_lim_temp <- temp_inc_tn_dat[[i]] %>% 
#              merge(riparian_load, by = c("comid", "season", "year")) %>%
              group_by(year, season) %>%
              
              summarize(total_load = sum(in_poin, na.rm = TRUE) + sum(in_urb, na.rm = TRUE) + 
                          sum(in_septic, na.rm = TRUE) + sum(in_ag, na.rm = TRUE) + sum(in_graz, na.rm = TRUE) + 
                          sum(in_road, na.rm = TRUE) + sum(in_storage, na.rm = TRUE),
                        # + sum(in_riparian),#+ sum(in_other) # CTC edited: 2/5/2026 ND edited 2/23/26
                        .groups = "drop"
              )

            total_load <- load_lim_temp %>%
              select(season, year, total_load) %>%
              merge(., year_xwalk, by = "year") %>%
              select(-year) %>%
              rename(year = model_year)
            # ND 6/18/26 end edits
            
          }        
        
      }
    )
    print("Generating TN param load limit based only on actionable load components")
  } else {
    print("Moving to generate TN param load limit based on all loads")
  }
  
  if(actionable_load == FALSE){
    invisible(
      if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
        param_loads_lim_tn <-
          foreach(i = 1:length(temp_inc_tn_dat)) %do% {
            
#            riparian_load <- temp_riparian_tn_dat[[i]]
            
            load_lim_temp <- temp_inc_tn_dat[[i]] %>% 
              group_by(year, season) %>%
              summarize(total_load = sum(in_poin, na.rm = TRUE) + sum(in_urb, na.rm = TRUE) +
                sum(in_septic, na.rm = TRUE) + sum(in_road, na.rm = TRUE) + sum(in_ag, na.rm = TRUE) +
                sum(in_graz, na.rm = TRUE) + sum(in_storage, na.rm = TRUE)+ sum(in_other, na.rm = TRUE),
                # + sum(in_riparian),# # CTC edited: 2/5/2026
                load_lim = (1 - as.numeric(load_target_tn[i])) * (sum(in_poin, na.rm = TRUE) + 
                  sum(in_urb, na.rm = TRUE) + sum(in_septic, na.rm = TRUE) + sum(in_ag, na.rm = TRUE) +
                  sum(in_graz, na.rm = TRUE) + sum(in_road, na.rm = TRUE) + sum(in_storage, na.rm = TRUE) +
                  sum(in_other, na.rm = TRUE)),# + sum(in_riparian)),# # CTC edited: 2/5/2026 ND 2/23/26
                        .groups = "drop"
              )
            
            load_lim <- load_lim_temp %>%
              select(season, year, load_lim) %>%
              merge(., year_xwalk, by = "year") %>%
              select(-year) %>%
              rename(year = model_year)
          }
        param_total_loads_tn <-
          foreach(i = 1:length(temp_inc_tn_dat)) %do% {
            
#            riparian_load <- temp_riparian_tn_dat[[i]]
            
            load_lim_temp <- temp_inc_tn_dat[[i]] %>% 
              group_by(year, season) %>%
              summarize(total_load = sum(in_poin, na.rm = TRUE) + sum(in_urb, na.rm = TRUE) +
                          sum(in_septic, na.rm = TRUE) + sum(in_road, na.rm = TRUE) + sum(in_ag, na.rm = TRUE) +
                          sum(in_graz, na.rm = TRUE) + sum(in_storage, na.rm = TRUE)+ sum(in_other, na.rm = TRUE),
                        # + sum(in_riparian),# # CTC edited: 2/5/2026
                        load_lim = (1 - as.numeric(load_target_tn[i])) * (sum(in_poin, na.rm = TRUE) + 
                                                                            sum(in_urb, na.rm = TRUE) + sum(in_septic, na.rm = TRUE) + sum(in_ag, na.rm = TRUE) +
                                                                            sum(in_graz, na.rm = TRUE) + sum(in_road, na.rm = TRUE) + sum(in_storage, na.rm = TRUE) +
                                                                            sum(in_other, na.rm = TRUE)),# + sum(in_riparian)),# # CTC edited: 2/5/2026 ND 2/23/26
                        .groups = "drop"
              )
            
            # ND 6/18/26 start edits            
            total_load <- load_lim_temp %>%
              select(season, year, total_load) %>%
              merge(., year_xwalk, by = "year") %>%
              select(-year) %>%
              rename(year = model_year) 
            # ND 6/18/26 end edits                    
        
          }
      }
    )
    print("Generating TN param load limit by target percentage based on all load components")
  }else{
    print("Generated TN param load limit by target percentage based only on actionable load components")
  }
 # ND 8/3/26 corrected code and added missing septic
  if(actionable_load == TRUE){
    invisible(
      if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
        param_loads_lim_tp <-
          foreach(i = 1:length(temp_inc_tp_dat)) %do% {
            
            i <- 1

            limit <- user_specs_loadingtargets[i, "adjusted_annual_target_kgperyr"]

#            riparian_load <- temp_riparian_tp_dat[[i]]

            load_lim_temp <- temp_inc_tp_dat[[i]] %>%
#              merge(riparian_load, by = c("comid", "season", "year")) %>%
              group_by(year, season) %>%
              summarize(total_load = sum(ip_poin, na.rm = TRUE) + sum(ip_urb, na.rm = TRUE) +
                sum(ip_ag, na.rm = TRUE) + sum(ip_graz, na.rm = TRUE) + sum(ip_septic, na.rm = TRUE) +
                sum(ip_road, na.rm = TRUE) + sum(ip_storage, na.rm = TRUE),# + sum(ip_riparian) + sum(ip_other)
                        .groups = "drop"
              ) %>%
              group_by(year) %>%
              mutate(season_share = total_load/sum(total_load)) %>%
              ungroup() %>%
              mutate(load_lim = limit * season_share)

            load_lim <- load_lim_temp %>%
              select(season, year, load_lim) %>%
              merge(., year_xwalk, by = "year") %>%
              select(-year) %>%
              rename(year = model_year)
          }

        param_total_loads_tp <-
          foreach(i = 1:length(temp_inc_tp_dat)) %do% {

            load_lim_temp <- temp_inc_tp_dat[[i]] %>%
#              merge(riparian_load, by = c("comid", "season", "year")) %>%
              group_by(year, season) %>%
              summarize(total_load = sum(ip_poin, na.rm = TRUE) + sum(ip_urb, na.rm = TRUE) +
                          sum(ip_ag, na.rm = TRUE) + sum(ip_graz, na.rm = TRUE) + sum(ip_septic, na.rm = TRUE) +
                          sum(ip_road, na.rm = TRUE) + sum(ip_storage, na.rm = TRUE),# + sum(ip_riparian) + sum(ip_other)
                        .groups = "drop"
              ) 

            total_load <- load_lim_temp %>%
              select(season, year, total_load) %>%
              merge(., year_xwalk, by = "year") %>%
              select(-year) %>%
              rename(year = model_year)
            # ND 6/18/26 end edits
          }
      }
    )
    print("Generating TP param load limit based only on actionable load components") # CTC edited: 2/5/2026
  }else{
    print("Moving to generate TP param load limit based on all loads") # CTC edited: 2/5/2026
  }
  if(actionable_load == FALSE){
    invisible(
      if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
        param_loads_lim_tp <-
          foreach(i = 1:length(temp_inc_tp_dat)) %do% {
            
            limit <- user_specs_loadingtargets[i, "adjusted_annual_target_kgperyr"]
            
#            riparian_load <- temp_riparian_tp_dat[[i]]
            
            load_lim_temp <- temp_inc_tp_dat[[i]] %>% # ND 8/4/26 added missing septic
#              merge(riparian_load, by = c("comid", "season", "year")) %>% 
              group_by(year, season) %>%
              summarize(total_load = sum(ip_poin, na.rm = TRUE) + sum(ip_urb, na.rm = TRUE) +
                  sum(ip_ag, na.rm = TRUE) + sum(ip_graz, na.rm = TRUE) + sum(ip_road, na.rm = TRUE) +
                  sum(ip_septic, na.rm = TRUE) + sum(ip_storage, na.rm = TRUE) + sum(ip_other, na.rm = TRUE), 
                  # + sum(ip_riparian, na.rm = TRUE),
                        .groups = "drop"
              ) %>%
              group_by(year) %>%
              mutate(season_share = total_load/sum(total_load)) %>%
              ungroup() %>%
              mutate(load_lim = limit * season_share)
            
            load_lim <- load_lim_temp %>%
              select(season, year, load_lim) %>%
              merge(., year_xwalk, by = "year") %>%
              select(-year) %>%
              rename(year = model_year)
            
# ND 6/18/26 start edits            
        param_total_loads_tp <-
          foreach(i = 1:length(temp_inc_tp_dat)) %do% {
            load_lim_temp <- temp_inc_tp_dat[[i]] %>%
#              merge(riparian_load, by = c("comid", "season", "year")) %>% 
              group_by(year, season) %>% #ND 8/4/26 added missing septic
              summarize(total_load = sum(ip_poin, na.rm = TRUE) + sum(ip_urb, na.rm = TRUE) +
                  sum(ip_ag, na.rm = TRUE) + sum(ip_graz, na.rm = TRUE) + sum(ip_road, na.rm = TRUE) +
                  sum(ip_septic, na.rm = TRUE), sum(ip_storage, na.rm = TRUE) + sum(ip_other, na.rm = TRUE), 
                  # + sum(ip_riparian, na.rm = TRUE),
                        .groups = "drop"
              ) %>%

            total_load <- load_lim_temp %>%
              select(season, year, total_load) %>%
              merge(., year_xwalk, by = "year") %>%
              select(-year) %>%
              rename(year = model_year)
# ND 6/18/26 end edits            
                                                      }
          }        
      }
    )
  }else{
    print("Generated TP param load limit by target percentage based only on actionable load components") # CTC edited: 2/5/2026
  }


  ##### g. Format data  ------
  
    # Format loadings data 
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    inc_tn_dat <- foreach(i = 1:length(temp_inc_tn_dat_SC)) %do% {
      temp_inc_tn_dat_opt <- temp_inc_tn_dat_SC[[i]]
      temp_inc_tn_dat_opt <- temp_inc_tn_dat_opt %>%
        mutate(comid_form = paste0("'", temp_inc_tn_dat_opt$comid, "'")) %>% 
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(c("comid_form", "season", "year", "in_poin", "in_urb", "in_road", "in_ag", "in_septic", "in_graz", "in_storage", "in_other")) # CTC edited: 2/5/2026
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    inc_tp_dat <- foreach(i = 1:length(temp_inc_tp_dat_SC)) %do% {
      temp_inc_tp_dat_opt <- temp_inc_tp_dat_SC[[i]]
      temp_inc_tp_dat_opt <- temp_inc_tp_dat_opt %>%
        mutate(comid_form = paste0("'", temp_inc_tp_dat_opt$comid, "'")) %>% 
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>% # ND 8/4/26 added missing septic
        select(c("comid_form", "season", "year", "ip_poin", "ip_urb", "ip_road", "ip_ag", "ip_septic", "ip_graz", "ip_storage", "ip_other"))
    }
  }
  
  if(length(RiparianBuffer_BMPs) > 0) {
    riparian_tn_dat <- foreach(i = 1:length(temp_riparian_tn_dat)) %do% {
      
      temp_riparian_tn_dat[[i]] %>%
        filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(comid_form, season, year, in_riparian)
      
    }
    
    riparian_tp_dat <- foreach(i = 1:length(temp_riparian_tp_dat)) %do% {
      
      temp_riparian_tp_dat[[i]] %>%
        filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(comid_form, season, year, ip_riparian)
      
    }
    
    riparian_tn_removal <- foreach(i = 1:length(temp_riparian_tn_removal)) %do% {
      
      temp_riparian_tn_removal[[i]] %>%
        filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(comid_form, season, year, any_of(RiparianBuffer_BMPs))
      
    }
    
    riparian_tp_removal <- foreach(i = 1:length(temp_riparian_tp_removal)) %do% {
      
      temp_riparian_tp_removal[[i]] %>%
        filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(comid_form, season, year, any_of(RiparianBuffer_BMPs))
      
    }
  }
  
  ## Format transfer coefficients -----
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    sm_N_dat <- foreach(i = 1:length(temp_sm_N_dat_SC)) %do% {
      if (!is.null(temp_sm_N_dat_SC[[i]]) && nrow(temp_sm_N_dat_SC[[i]]) > 0) {
        temp_sm_N_dat_opt <- temp_sm_N_dat_SC[[i]]
        temp_sm_N_dat_opt$comid_form <- paste0(
          "'", temp_sm_N_dat_opt$comid, "'"
        )
        temp_sm_N_dat_opt$year_form <- paste0(
          "'", temp_sm_N_dat_opt$year, "'"
        )
        temp_sm_N_dat_opt %>%
          merge(., year_xwalk, by = "year") %>%
          select(-year) %>%
          rename(year = model_year) %>%
          select(c('comid_form', season, year, 'coeff', 'notrib')) %>%
          arrange(comid_form, year, season)
      } else {
        NULL
      }
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    sm_P_dat <- foreach(i = 1:length(temp_sm_P_dat_SC)) %do% {
      if (!is.null(temp_inc_tp_dat_SC[[i]]) && nrow(temp_inc_tp_dat_SC[[i]]) > 0) {
        temp_sm_P_dat_opt <- temp_sm_P_dat_SC[[i]]
        temp_sm_P_dat_opt$comid_form <- paste0(
          "'", temp_sm_P_dat_opt$comid, "'"
        )
        temp_sm_P_dat_opt %>%
          merge(., year_xwalk, by = "year") %>%
          select(-year) %>%
          rename(year = model_year) %>%
          select(c('comid_form', season, year, 'coeff','notrib')) %>%
          arrange(comid_form, year, season)
      } else {
        NULL  # Skip empty data frames
      }
    }
  }

  # Format urban BMP costs
  
  if(length(Urban_BMPs) > 0) {
    
    print(
      paste0(
        "Urban costs are assigned based on landuse and hydrologic soil group in each comid. They are calculated using specifications that meet design standards. Urban BMPs with no cost information available are displayed as (1 x e12) to deter the NEOS server from selecting BMPs with missing data."
      )
    )
    
    # First, merge bmp_costs_urban with complete_urban to get ALL comid × land_use × HSG combinations
    # This ensures that COMIDs without cost data still get entries (filled with NA, then replaced with high cost)
    urban_costs_dat <- complete_urban %>%
      left_join(bmp_costs_urban, by = c("comid" = "comid", "urban_land_use" = "Land_Use", "urban_hsg" = "HSG")) %>%
      rename(Land_Use = urban_land_use, HSG = urban_hsg) %>%
      unique()
    
    urban_costs_dat$comid_form  <- paste0("'", urban_costs_dat$comid, "'") 
    urban_costs_dat$Land_Use  <- paste0("'", urban_costs_dat$Land_Use, "'") 
    urban_costs_dat$HSG  <- paste0("'", urban_costs_dat$HSG, "'")
    
    # Calculate average urban capital and operations costs (only from rows with actual data)
    mean_urban_bmp_capital_cost <- urban_costs_dat %>%
      filter(!is.na(bmp)) %>%
      group_by(bmp) %>%
      summarise(mean_cap_cost = mean(capital, na.rm = TRUE)) %>%
      pivot_wider(names_from = bmp, values_from = c(mean_cap_cost)) %>%
      rename_with(~paste0(., "_mean"))
    
    mean_urban_bmp_operations_cost <- urban_costs_dat %>%
      filter(!is.na(bmp)) %>%
      group_by(bmp) %>%
      summarise(mean_op_cost = mean(operations, na.rm = TRUE)) %>%
      pivot_wider(names_from = bmp, values_from = c(mean_op_cost)) %>%
      rename_with(~paste0(., "_mean"))
    
    urban_costs_capital_dat <- urban_costs_dat %>% 
      pivot_wider(id_cols = c(comid_form, Land_Use, HSG), names_from = bmp, values_from = c(capital)) %>%
      cbind(., mean_urban_bmp_capital_cost) %>%
      # Replace NA values with the average cost within the model to avoid AMPL errors
      mutate(across(any_of(Urban_BMPs), ~map2(., mean_urban_bmp_capital_cost[grepl(cur_column(), names(mean_urban_bmp_capital_cost))], ~replace_na(.x, .y)))) %>%
      unique() %>%
      unnest(cols = 1:ncol(.)) %>%
      select(comid_form, Land_Use, HSG, any_of(bmp_urban_direct))
    
    #Fill in all BMPs if some are dropped due to no data
    bmp_cols <- urban_costs_capital_dat %>%
      select(!comid_form)
    
    missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
    
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        if (nrow(urban_costs_capital_dat) > 0) {
          urban_costs_capital_dat[[col]] <- 1e30
        } else {
          urban_costs_capital_dat[[col]] <- vector("numeric", length = 0)
        }
      }
    }
    
    urban_costs_capital_dat <- urban_costs_capital_dat %>%
      mutate(across(any_of(Urban_BMPs), ~replace_na(., 1e30))) %>%
      unique()
    
    # Filter urban cost data to only include COMIDs in streamcat_subset_all (comid_all set)
    # This prevents AMPL warnings about dropped subscripts for COMIDs not in the model
    urban_costs_capital_dat <- urban_costs_capital_dat %>%
      filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
    
    urban_costs_operations_dat <- urban_costs_dat %>%
      pivot_wider(id_cols = c(comid_form, Land_Use, HSG), names_from = bmp, values_from = operations) %>%
      cbind(., mean_urban_bmp_operations_cost) %>%
      unnest(cols = 1:ncol(.)) %>%
      # Replace NA values with the average cost within the model to avoid AMPL errors
      mutate(across(any_of(Urban_BMPs), ~map2(., mean_urban_bmp_operations_cost[grepl(cur_column(), names(mean_urban_bmp_operations_cost))], ~replace_na(.x, .y)))) %>%
      # mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%  # Replace NA values with 0, as no NA value available in AMPL
      unique() %>%
      select(comid_form, Land_Use, HSG, any_of(bmp_urban_direct))
    
    #Fill in all BMPs if some are dropped due to no data
    bmp_cols <- urban_costs_operations_dat %>%
      select(!comid_form)
    
    missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
    
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        if (nrow(urban_costs_operations_dat) > 0) {
          urban_costs_operations_dat[[col]] <- 1e30
        } else {
          urban_costs_operations_dat[[col]] <- vector("numeric", length = 0)
        }
      }
    }
    
    urban_costs_operations_dat <- urban_costs_operations_dat %>%
      mutate(across(any_of(Urban_BMPs), ~replace_na(., 1e30))) %>%
      unique()
    
    # Filter urban operations cost data to only include COMIDs in streamcat_subset_all (comid_all set)
    # This prevents AMPL warnings about dropped subscripts for COMIDs not in the model
    urban_costs_operations_dat <- urban_costs_operations_dat %>%
      filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
  }
  
  # Format road BMP costs
  
  if(length(road_BMPs) > 0) {
    
    if (!is.na(any(bmp_costs_road$comid))) {
      # Create complete grid of ALL comid × HSG × BMP combinations
      # This ensures every combination gets a value, even if data is missing
      complete_road_bmps <- complete_road %>%
        crossing(bmp = road_BMPs)
      
      # Merge with actual cost data - missing combinations will have NA values
      road_costs_dat <- complete_road_bmps %>%
        left_join(bmp_costs_road, by = c("comid" = "comid", "road_hsg" = "HSG", "bmp" = "bmp")) %>%
        rename(HSG = road_hsg) %>%
        unique()
    
      road_costs_dat$comid_form  <- paste0("'", road_costs_dat$comid, "'")
      road_costs_dat$HSG <- paste0("'", road_costs_dat$HSG, "'")
    } else {
      # If no road cost data at all, create grid with zero costs
      road_costs_dat <- data.frame(
        comid = unique(reaches_all$comid)
      ) %>%
        crossing(HSG = c("A", "B", "C", "D")) %>%
        crossing(bmp = road_BMPs) %>%
        mutate(capital = 1e30,
               operations = 1e30)
      
      road_costs_dat$comid_form  <- paste0("'", road_costs_dat$comid, "'")
      road_costs_dat$HSG <- paste0("'", road_costs_dat$HSG, "'")
    }
    
    # Calculate average road capital and operations costs (only from rows with actual data)
    if (length(road_costs_dat$comid) > 0) {
      mean_road_bmp_capital_cost <- road_costs_dat %>%
        filter(!is.na(bmp)) %>%
        group_by(bmp) %>%
        summarise(mean_cap_cost = mean(capital, na.rm = TRUE)) %>%
        pivot_wider(names_from = bmp, values_from = c(mean_cap_cost)) %>%
        rename_with(~paste0(., "_mean"))
      
      mean_road_bmp_operations_cost <- road_costs_dat %>%
        filter(!is.na(bmp)) %>%
        group_by(bmp) %>%
        summarise(mean_op_cost = mean(operations, na.rm = TRUE)) %>%
        pivot_wider(names_from = bmp, values_from = c(mean_op_cost)) %>%
        rename_with(~paste0(., "_mean"))
      
      road_costs_capital_dat <- road_costs_dat %>% 
        pivot_wider(id_cols = c(comid_form, HSG), names_from = bmp, values_from = c(capital)) %>%
        cbind(., mean_road_bmp_capital_cost) %>%
        # Replace NA values with the average cost within the model to avoid AMPL errors
        mutate(across(any_of(road_BMPs), ~map2(., mean_road_bmp_capital_cost[grepl(cur_column(), names(mean_road_bmp_capital_cost))], ~replace_na(.x, .y)))) %>%
        unique() %>%
        unnest(cols = 1:ncol(.)) %>%
        select(comid_form, HSG, any_of(road_BMPs))
      
      #Fill in all BMPs if some are dropped due to no data
      bmp_cols <- road_costs_capital_dat %>%
        select(!c(comid_form, HSG))
      
      missing_columns <- setdiff(road_BMPs, names(bmp_cols))
      
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          if (nrow(road_costs_capital_dat) > 0) {
            road_costs_capital_dat[[col]] <- 1e30
          } else {
            road_costs_capital_dat[[col]] <- vector("numeric", length = 0)
          }
        }
      }
      
      road_costs_capital_dat <- road_costs_capital_dat %>%
        mutate(across(any_of(road_BMPs), ~replace_na(., 1e30))) %>%
        unique()
      
      # Filter road capital cost data to only include COMIDs in streamcat_subset_all (comid_all set)
      # This prevents AMPL warnings about dropped subscripts for COMIDs not in the model
      road_costs_capital_dat <- road_costs_capital_dat %>%
        filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
      
      road_costs_operations_dat <- road_costs_dat %>%
        pivot_wider(id_cols = c(comid_form, HSG), names_from = bmp, values_from = operations) %>%
        cbind(., mean_road_bmp_operations_cost) %>%
        unnest(cols = 1:ncol(.)) %>%
        # Replace NA values with the average cost within the model to avoid AMPL errors
        mutate(across(any_of(road_BMPs), ~map2(., mean_road_bmp_operations_cost[grepl(cur_column(), names(mean_road_bmp_operations_cost))], ~replace_na(.x, .y)))) %>%
        # mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%  # Replace NA values with 0, as no NA value available in AMPL
        unique() %>%
        select(comid_form, HSG, any_of(road_BMPs)) %>%
        mutate(across(any_of(road_BMPs), as.numeric))
      
      #Fill in all BMPs if some are dropped due to no data
      bmp_cols <- road_costs_operations_dat %>%
        select(!c(comid_form, HSG))
      
      missing_columns <- setdiff(road_BMPs, names(bmp_cols))
      
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          if (nrow(road_costs_operations_dat) > 0) {
            road_costs_operations_dat[[col]] <- 1e30
          } else {
            road_costs_operations_dat[[col]] <- vector("numeric", length = 0)
          }
        }
      }
      
      road_costs_operations_dat <- road_costs_operations_dat %>%
        mutate(across(any_of(road_BMPs), ~replace_na(., 1e30))) %>%
        unique()
      
      # Filter road operations cost data to only include COMIDs in streamcat_subset_all (comid_all set)
      # This prevents AMPL warnings about dropped subscripts for COMIDs not in the model
      road_costs_operations_dat <- road_costs_operations_dat %>%
        filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
      
    } else {
      road_costs_capital_dat <- road_costs_dat %>%
        select(combo_form)
      for (bmp in road_BMPs) {
        road_costs_capital_dat[[bmp]] <- vector("numeric", length = 0)
      }
      
      road_costs_operations_dat <- road_costs_dat %>%
        select(combo_form)
      for (bmp in road_BMPs) {
        road_costs_operations_dat[[bmp]] <- vector("numeric", length = 0)
      }
    }
  }
  
  
  # Separate parameters for ag_capital and ag_operations and format costs data
  
  if(length(Ag_BMPs) > 0) {
    
    temp_bmp_costs_ag <- merge(
      bmp_nonptsrc_costs %>%
        filter(category == "ag") %>%
        select(
          bmp, contains(c("capital", "operations"))
        ),
      COMID_State
    ) %>%
      mutate(
        capital = my_key_fun(., "State", ~paste0("capital_", .x)),
        operations = my_key_fun(., "State", ~paste0("operations_", .x))
      ) %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    if(
      any(is.na(temp_bmp_costs_ag$capital)) | 
      any(is.na(temp_bmp_costs_ag$operations))
    ) {
      stop(
        paste0(
          "Ag ",
          if(
            any(is.na(temp_bmp_costs_ag$capital)) & 
            any(is.na(temp_bmp_costs_ag$operations))
          ) {paste("capital and operations ")} else if (
            any(is.na(temp_bmp_costs_ag$capital))
          ) {paste("capital ")} else {paste("operations ")},
          "costs and/or units for ", 
          paste(
            temp_bmp_costs_ag %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(bmp) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " in ",
          paste(
            temp_bmp_costs_ag %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(State) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`, and that units are one of: ft2, km2, yd2, ac."
        )
      )
    }
    
    bmp_costs_ag_capital <- reshape2::dcast(
      temp_bmp_costs_ag[, c("comid", "comid_form", "bmp", "capital")], 
      comid_form + comid ~ bmp,
      value.var = "capital"
    )
    bmp_costs_ag_operations <- reshape2::dcast(
      temp_bmp_costs_ag[, c("comid", "comid_form", "bmp", "operations")],
      comid_form + comid ~ bmp,
      value.var = "operations"
    )
    
    bmp_costs_ag_capital_rev <- bmp_costs_ag_capital[
      order(bmp_costs_ag_capital$comid),
    ]
    ag_costs_cap_dat <- bmp_costs_ag_capital_rev[
      ,names(bmp_costs_ag_capital_rev) != "comid"
    ]
    ag_costs_cap_dat <- ag_costs_cap_dat %>% select(comid_form, everything())
    
    bmp_costs_ag_operations_rev <- bmp_costs_ag_operations[
      order(bmp_costs_ag_operations$comid),
    ]
    ag_costs_op_dat <- bmp_costs_ag_operations_rev[ 
      ,names(bmp_costs_ag_operations_rev) != "comid"
    ]
    ag_costs_op_dat <- ag_costs_op_dat %>% select(comid_form, everything())
  }
  
#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
#### ND edited: 2/26/26 - changed from VT to WA  
  ## Format Septic BMP Costs ----
  if(length(Septic_BMPs) > 0){
    
    septic_bmp_costs_temp <- user_specs_BMPs %>%
      filter(BMP_Selection == "X" & BMP_Category == "septic")%>%
      select(BMP_Category, BMP, capital_WA:operations_WA)%>%
      unique()%>%
      group_by(BMP_Category, BMP) %>%
      summarise(
        cap_cost = mean(capital_WA, na.rm = TRUE),
        op_cost = mean(operations_WA, na.rm = TRUE),
        .groups = "drop"
      )%>%
      merge(., user_specs_BMPs%>%
              filter(BMP_Selection == "X" & BMP_Category == "septic")%>%
              select(BMP_Category, BMP, capital_WA, operations_WA), by = c("BMP_Category", "BMP")
      )%>%
      mutate(
        cap_check = ifelse(cap_cost == capital_WA, 0, 1),
        op_check = ifelse(op_cost == operations_WA, 0, 1)
      )%>%
      select(-capital_WA, -operations_WA, -cap_check, -op_check)
    
    sep_convert_bmp_costs_temp <- septic.conversion%>%
      select(-parcels, -efficiency_per_parcel)%>%
      mutate(
        BMP = "Sewer_convert"
      )%>%
      merge(., septic_bmp_costs_temp%>%select(BMP, cap_cost, op_cost), by = c("BMP")
      )%>%
      select(comid, everything())
    
    sep_upgrade_bmp_costs_temp <- septic.upgrade%>%
      rename(BMP = SeptUpgrCl)%>% #ND fixed variable name
      mutate(
        BMP = paste0("class_", BMP, "_upgrade")
      )%>%
      select(-parcels, -nparcels, -efficiency_per_parcel)%>% #ND added nparcels to list
      merge(., septic_bmp_costs_temp%>%select(BMP, cap_cost, op_cost), by = c("BMP")
      )%>%
      select(comid, everything())
    
    septic_costs_temp <- rbind(sep_convert_bmp_costs_temp, 
                               select(sep_upgrade_bmp_costs_temp,-SepticUpgradeClass,-Convert_Efficiency0))
    # ND removed extraneous columns
    
    septic_cap_costs <- septic_costs_temp %>%
      select(comid, BMP, cap_cost) %>%
      mutate(comid = as.character(comid)) %>% #ND edit 2/26/26 to facilitate matching
      pivot_wider(
        id_cols = comid,
        names_from = BMP, 
        values_from = cap_cost
      ) %>%
      right_join(
        data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
        by = "comid"
      ) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, contains("convert"), contains("class"))
    
    septic_op_costs <- septic_costs_temp%>%
      select(comid, BMP, op_cost)%>%
      mutate(comid = as.character(comid))%>% # ND edit for matching types 2/26/26
      pivot_wider(
        id_cols = comid,
        names_from = BMP, 
        values_from = op_cost
      ) %>%
      right_join(
        data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
        by = "comid"
      ) %>%
      mutate(comid_form = paste0("'", comid, "'"))%>%
      select(comid_form, contains("convert"), contains("class"))
    
    # replace NAs in the septic_bmp_costs dataframes with 1e30
    septic_cap_costs[is.na(septic_cap_costs)] <- 1e30
    septic_op_costs[is.na(septic_op_costs)] <- 1e30
  }
  
#### end edits ####
  
  # Format riparian BMP costs
  
  if(length(RiparianBuffer_BMPs) > 0) {
    temp_bmp_costs_ripbuf <- merge(
      bmp_nonptsrc_costs %>%
        filter(category == "ripbuf") %>%
        select(
          bmp, contains(c("capital", "operations"))
        ),
      COMID_State
    ) %>%
      mutate(
        capital = my_key_fun(., "State", ~paste0("capital_", .x)),
        operations = my_key_fun(., "State", ~paste0("operations_", .x))
      ) %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    if(
      any(is.na(temp_bmp_costs_ripbuf$capital)) | 
      any(is.na(temp_bmp_costs_ripbuf$operations))
    ) {
      stop(
        paste0(
          "Riparian Buffer ",
          if(
            any(is.na(temp_bmp_costs_ripbuf$capital)) & 
            any(is.na(temp_bmp_costs_ripbuf$operations))
          ) {paste("capital and operations ")} else if (
            any(is.na(temp_bmp_costs_ripbuf$capital))
          ) {paste("capital ")} else {paste("operations ")},
          "costs and/or units for ", 
          paste(
            temp_bmp_costs_ripbuf %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(bmp) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " in ",
          paste(
            temp_bmp_costs_ripbuf %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(State) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`, and that units are one of: ft2, km2, yd2, ac."
        )
      )
    }
    
    bmp_costs_ripbuf_capital <- reshape2::dcast(
      temp_bmp_costs_ripbuf[, c("comid", "comid_form", "bmp", "capital")], 
      comid_form + comid ~ bmp,
      value.var = "capital"
    )
    
    bmp_costs_ripbuf_operations <- reshape2::dcast(
      temp_bmp_costs_ripbuf[, c("comid", "comid_form", "bmp", "operations")],
      comid_form + comid ~ bmp,
      value.var = "operations"
    )
    
    bmp_costs_ripbuf_capital_rev <- bmp_costs_ripbuf_capital[
      order(bmp_costs_ripbuf_capital$comid),
    ]
    ripbuf_costs_cap_dat <- bmp_costs_ripbuf_capital_rev[
      ,names(bmp_costs_ripbuf_capital_rev) != "comid"
    ]
    ripbuf_costs_cap_dat <- ripbuf_costs_cap_dat %>% 
      select(comid_form, everything())
    
    bmp_costs_ripbuf_operations_rev <- bmp_costs_ripbuf_operations[
      order(bmp_costs_ripbuf_operations$comid),
    ]
    ripbuf_costs_op_dat <- bmp_costs_ripbuf_operations_rev[ 
      ,names(bmp_costs_ripbuf_operations_rev) != "comid"
    ]
    ripbuf_costs_op_dat <- ripbuf_costs_op_dat %>% 
      select(comid_form, everything())
    
  }
  
  
  # Format point source BMP efficiency data
  
  if(length(Point_BMPs) > 0) {
    
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      temp_point_effic_dat_tn <- point_effic_bycomid_tn %>%
        filter(comid %in% unlist(streamcat_subset_tn)) %>% 
        pivot_wider(id_cols = comid, names_from = BMP, values_from = effic) %>%
        select(comid, any_of(Point_BMPs)) # forces the order
      
      temp_point_effic_other_tn <- data.frame(
        comid = unlist(
          streamcat_subset_tn, use.names = FALSE
        )[
          !(
            unlist(streamcat_subset_tn, use.names = FALSE) %in% 
              temp_point_effic_dat_tn$comid
          )
        ]
      )
      
      temp_point_effic_other_tn[2:(1+length(Point_BMPs))] <- 0
      names(temp_point_effic_other_tn) <- c("comid", Point_BMPs)
      
      if(nrow(temp_point_effic_dat_tn) > 0) {
        temp_point_effic_dat_rev_tn <- rbind(
          temp_point_effic_dat_tn, temp_point_effic_other_tn
        ) %>% 
          arrange(comid) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      } else {
        temp_point_effic_dat_rev_tn <- temp_point_effic_other_tn %>% 
          arrange(comid) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      }
      
      point_effic_dat_tn <- temp_point_effic_dat_rev_tn %>% 
        select(comid_form, all_of(Point_BMPs)) %>%
        unique() # function used to ensure duplicates don't creep in
    }
    
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      
      temp_point_effic_dat_tp <- point_effic_bycomid_tp %>%
        filter(comid %in% unlist(streamcat_subset_tp)) %>% 
        pivot_wider(id_cols = comid, names_from = BMP, values_from = effic) %>%
        select(comid, any_of(Point_BMPs)) # forces the order
      
      temp_point_effic_other_tp <- data.frame(
        comid = unlist(
          streamcat_subset_tp, use.names = FALSE
        )[
          !(
            unlist(streamcat_subset_tp, use.names = FALSE) %in% 
              temp_point_effic_dat_tp$comid
          )
        ]
      )
      
      temp_point_effic_other_tp[2:(1+length(Point_BMPs))] <- 0
      names(temp_point_effic_other_tp) <- c("comid", Point_BMPs)
      
      if(nrow(temp_point_effic_dat_tp) > 0) {
        temp_point_effic_dat_rev_tp <- rbind(
          temp_point_effic_dat_tp, temp_point_effic_other_tp
        ) %>% 
          arrange(comid) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      } else {
        temp_point_effic_dat_rev_tp <- temp_point_effic_other_tp %>% 
          arrange(comid) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      }
      
      point_effic_dat_tp <- temp_point_effic_dat_rev_tp %>% 
        select(comid_form, all_of(Point_BMPs)) %>%
        unique() # function used to ensure duplicates don't creep in
    }
  }
  # Format grazing BMP costs
  
  if(length(Graz_BMPs) > 0) {
    temp_bmp_costs_graz <- merge(
      bmp_nonptsrc_costs %>%
        filter(category == "graz") %>%
        select(
          bmp, contains(c("capital", "operations"))
        ),
      COMID_State
    ) %>%
      mutate(
        capital = my_key_fun(., "State", ~paste0("capital_", .x)),
        operations = my_key_fun(., "State", ~paste0("operations_", .x))
      ) %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    if(
      any(is.na(temp_bmp_costs_graz$capital)) | 
      any(is.na(temp_bmp_costs_graz$operations))
    ) {
      stop(
        paste0(
          "Grazing ",
          if(
            any(is.na(temp_bmp_costs_graz$capital)) & 
            any(is.na(temp_bmp_costs_graz$operations))
          ) {paste("capital and operations ")} else if (
            any(is.na(temp_bmp_costs_graz$capital))
          ) {paste("capital ")} else {paste("operations ")},
          "costs and/or units for ", 
          paste(
            temp_bmp_costs_graz %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(bmp) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " in ",
          paste(
            temp_bmp_costs_graz %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(State) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`, and that units are one of: ft2, km2, yd2, ac."
        )
      )
    }
    
    bmp_costs_graz_capital <- reshape2::dcast(
      temp_bmp_costs_graz[, c("comid", "comid_form", "bmp", "capital")], 
      comid_form + comid ~ bmp,
      value.var = "capital"
    )
    
    bmp_costs_graz_operations <- reshape2::dcast(
      temp_bmp_costs_graz[, c("comid", "comid_form", "bmp", "operations")],
      comid_form + comid ~ bmp,
      value.var = "operations"
    )
    
    bmp_costs_graz_capital_rev <- bmp_costs_graz_capital[
      order(bmp_costs_graz_capital$comid),
    ]
    graz_costs_cap_dat <- bmp_costs_graz_capital_rev[
      ,names(bmp_costs_graz_capital_rev) != "comid"
    ]
    graz_costs_cap_dat <- graz_costs_cap_dat %>% 
      select(comid_form, everything())
    
    bmp_costs_graz_operations_rev <- bmp_costs_graz_operations[
      order(bmp_costs_graz_operations$comid),
    ]
    graz_costs_op_dat <- bmp_costs_graz_operations_rev[ 
      ,names(bmp_costs_graz_operations_rev) != "comid"
    ]
    graz_costs_op_dat <- graz_costs_op_dat %>% 
      select(comid_form, everything())
    
  }
  
  # Format agricultural BMP efficiency data
  
  if(length(Ag_BMPs) > 0) {
    temp_ag_effic_dat_tn <- ag_effic_bycomid_tn[
      ag_effic_bycomid_tn$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    ag_effic_dat_tn <- temp_ag_effic_dat_tn %>% 
      select(-comid) %>% 
      select(comid_form, everything())
    
    temp_ag_effic_dat_tp <- ag_effic_bycomid_tp[
      ag_effic_bycomid_tp$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    ag_effic_dat_tp <- temp_ag_effic_dat_tp %>% 
      select(-comid) %>% 
      select(comid_form, everything())
  }
  
  # Format grazing BMP efficiency data
  
  if(length(Graz_BMPs) > 0) {
    temp_graz_effic_dat_tn <- graz_effic_bycomid_tn[
      graz_effic_bycomid_tn$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    graz_effic_dat_tn <- temp_graz_effic_dat_tn %>% 
      select(-comid) %>% 
      select(comid_form, everything())
    
    temp_graz_effic_dat_tp <- graz_effic_bycomid_tp[
      graz_effic_bycomid_tp$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    graz_effic_dat_tp <- temp_graz_effic_dat_tp %>% 
      select(-comid) %>% 
      select(comid_form, everything())
  }
  
  # Format urban BMP efficiency data
  
  if(length(Urban_BMPs) > 0) {
    temp_urban_effic_dat_tn <- urban_effic_bycomid_tn[
      urban_effic_bycomid_tn$comid %in% 
        unlist(streamcat_subset_tn, use.names = FALSE),
    ] %>%
      merge(., year_xwalk, by = "year") %>%
      select(-year) %>%
      rename(year = model_year) %>%
      merge(., complete_urban_N, by.x = c("comid", "season", "year", "Land_Use", "HSG"), by.y = c("comid", "season", "year", "urban_land_use", "urban_hsg"), all = T) %>%
      arrange(comid, season, year, Land_Use, HSG) %>%
      mutate(comid_form = paste0("'", comid, "'"),
             Land_Use = paste0("'", Land_Use, "'"),
             HSG = paste0("'", HSG, "'")) %>%
      mutate(across(any_of(Urban_BMPs), ~ case_when(is.na(.) ~ 0, T ~ .))) %>%
      filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
    
    urban_effic_dat_tn <- temp_urban_effic_dat_tn %>% 
      select(-comid) %>% 
      select(comid_form, season, year, Land_Use, HSG, everything()) 
    
    temp_urban_effic_dat_tp <- urban_effic_bycomid_tp[
      urban_effic_bycomid_tp$comid %in% 
        unlist(streamcat_subset_tp, use.names = FALSE),
    ] %>%
      merge(., year_xwalk, by = "year") %>%
      select(-year) %>%
      rename(year = model_year) %>%
      merge(., complete_urban_P, by.x = c("comid", "season", "year", "Land_Use", "HSG"), by.y = c("comid", "season", "year", "urban_land_use", "urban_hsg"), all = T) %>%
      arrange(comid, season, year, Land_Use, HSG) %>%
      mutate(comid_form = paste0("'", comid, "'"),
             Land_Use = paste0("'", Land_Use, "'"),
             HSG = paste0("'", HSG, "'")) %>%
      mutate(across(any_of(Urban_BMPs), ~ case_when(is.na(.) ~ 0, T ~ .))) %>%
      filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
    
    urban_effic_dat_tp <- temp_urban_effic_dat_tp %>% 
      select(-comid,) %>%
      select(comid_form, season, year, Land_Use, HSG, everything())
    
    # Add in missing BMP columns for TN
    bmp_cols <- urban_effic_dat_tn %>%
      select(!comid_form)
    missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
    
    for (col in missing_columns) {
      if (nrow(urban_effic_dat_tn) > 0) {
        urban_effic_dat_tn[[col]] <- 0
      } else {
        urban_effic_dat_tn[[col]] <- vector("numeric", length = 0)
      }
    }

    # Add in missing BMP columns for TP    
    bmp_cols <- urban_effic_dat_tp %>%
      select(!comid_form)
    missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
    
    for (col in missing_columns) {
      if (nrow(urban_effic_dat_tp) > 0) {
        urban_effic_dat_tp[[col]] <- 0
      } else {
        urban_effic_dat_tp[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
  # Format road BMP efficiency data
  
  if(length(road_BMPs) > 0) {
    if(length(road_effic_bycomid_tn$comid) > 0) {
      temp_road_effic_dat_tn <- road_effic_bycomid_tn[
        road_effic_bycomid_tn$comid %in% 
          unlist(streamcat_subset_tn, use.names = FALSE),
      ] %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        merge(., complete_road_N, by.x = c("comid", "season", "year", "HSG"), by.y = c("comid", "season", "year", "road_hsg"), all = T) %>%
        arrange(comid, season, year, HSG) %>%
        mutate(comid_form = paste0("'", comid, "'"),
               HSG = paste0("'", HSG, "'")) %>%
        mutate(across(any_of(road_BMPs), ~ case_when(is.na(.) ~ 0, T ~ .))) %>%
        filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
      
      road_effic_dat_tn <- temp_road_effic_dat_tn %>% 
        select(-comid) %>% 
        select(comid_form, season, year, HSG, everything())
    } else {
      temp_road_effic_dat_tn <- data.frame(
        comid = unique(reaches_all$comid),
        HSG = NA,
        effic = 0
      ) %>%
        crossing(., road_BMPs) %>%
        crossing(., years) %>%
        crossing(., seasons) %>%
        rename(season = seasons, year = years) %>%
        pivot_wider(., id_cols = c(comid, season, year, HSG), names_from = road_BMPs, values_from = effic) %>%
        arrange(comid, season, year, HSG) %>%
        mutate(comid_form = paste0("'", comid, "'"),
               HSG = paste0("'", HSG, "'")) %>%
        mutate(across(any_of(road_BMPs), ~ case_when(is.na(.) ~ 0, T ~ .)))
      
      road_effic_dat_tn <- temp_road_effic_dat_tn %>% 
        select(-comid) %>% 
        select(comid_form, season, year, HSG, everything())
    }
    
    if(length(road_effic_bycomid_tp$comid) > 0) {
      temp_road_effic_dat_tp <- road_effic_bycomid_tp[
        road_effic_bycomid_tp$comid %in% 
          unlist(streamcat_subset_tp, use.names = FALSE),
      ] %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        merge(., complete_road_P, by.x = c("comid", "season", "year", "HSG"), by.y = c("comid", "season", "year", "road_hsg"), all = T) %>%
        arrange(comid, season, year, HSG) %>%
        mutate(comid_form = paste0("'", comid, "'"),
               HSG = paste0("'", HSG, "'")) %>%
        mutate(across(any_of(road_BMPs), ~ case_when(is.na(.) ~ 0, T ~ .))) %>%
        filter(comid_form %in% paste0("'", streamcat_subset_all$comid, "'"))
      
      road_effic_dat_tp <- temp_road_effic_dat_tp %>% 
        select(-comid) %>% 
        select(comid_form, season, year, HSG, everything())
    } else {
      temp_road_effic_dat_tp <- data.frame(
        comid = unique(reaches_all$comid),
        HSG = NA,
        effic = 0
      ) %>%
        crossing(., road_BMPs) %>%
        crossing(., years) %>%
        crossing(., seasons) %>%
        rename(season = seasons, year = years) %>%
        pivot_wider(., id_cols = c(comid, season, year, HSG), names_from = road_BMPs, values_from = effic) %>%
        arrange(comid, season, year, HSG) %>%
        mutate(comid_form = paste0("'", comid, "'"),
               HSG = paste0("'", HSG, "'")) %>%
        mutate(across(any_of(road_BMPs), ~ case_when(is.na(.) ~ 0, T ~ .)))
      
      road_effic_dat_tp <- temp_road_effic_dat_tp %>% 
        select(-comid) %>% 
        select(comid_form, season, year, HSG, everything())
    }

    # Add in missing BMP columns for TN
    bmp_cols <- road_effic_dat_tn %>%
      select(!comid_form)
    missing_columns <- setdiff(road_BMPs, names(bmp_cols))
    
    for (col in missing_columns) {
      if (nrow(road_effic_dat_tn) > 0) {
        road_effic_dat_tn[[col]] <- 0
      } else {
        road_effic_dat_tn[[col]] <- vector("numeric", length = 0)
      }
    }
    
    # Add in missing BMP columns for TP    
    bmp_cols <- road_effic_dat_tp %>%
      select(!comid_form)
    missing_columns <- setdiff(road_BMPs, names(bmp_cols))
    
    for (col in missing_columns) {
      if (nrow(road_effic_dat_tp) > 0) {
        road_effic_dat_tp[[col]] <- 0
      } else {
        road_effic_dat_tp[[col]] <- vector("numeric", length = 0)
      }
    }    
    
  } 

#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
# ND edited 5/31/2026 substituted SeptUpgrCl for SepticUpgradeClass  
  ## Format septic BMP efficiency data -----
  if(length(Septic_BMPs) > 0) {
    
    septic_effic_dat_temp <- septic.upgrade %>% select(comid, SeptUpgrCl, efficiency_per_parcel)%>% #ND fixed var name
      mutate(
        SeptUpgrCl = case_when(
          SeptUpgrCl == 1 ~ "class_1_upgrade",
          SeptUpgrCl == 2 ~ "class_2_upgrade",
          SeptUpgrCl == 3 ~ "class_3_upgrade",
          SeptUpgrCl == 4 ~ "class_4_upgrade",
          SeptUpgrCl == 5 ~ "class_5_upgrade",
          SeptUpgrCl == 6 ~ "class_6_upgrade",
          SeptUpgrCl == 7 ~ "class_7_upgrade",
          SeptUpgrCl == 8 ~ "class_8_upgrade"
        )
      )
    
    if(("Sewer_convert" %in% Septic_BMPs) == TRUE){
      
      # Create efficiency for sewer conversions
      sewer_effic_dat_temp <- septic.conversion%>%select(comid, efficiency_per_parcel)%>%
        mutate(
          SeptUpgrCl = "Sewer_convert"
        )
      
      # Combine septic and sewer efficiency data
      septic_effic_dat_tn <-  rbind(septic_effic_dat_temp, sewer_effic_dat_temp)%>%
        pivot_wider(
          names_from = SeptUpgrCl,
          values_from = efficiency_per_parcel
        ) %>%
        right_join(
          data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
          by = "comid"
        ) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    } else {
      
      # Combine septic and sewer efficiency data
      septic_effic_dat_tn <-  septic_effic_dat_temp%>%
        pivot_wider(
          names_from = SeptUpgrCl,
          values_from = efficiency_per_parcel
        ) %>%
        right_join(
          data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
          by = "comid"
        ) %>%
        mutate(comid_form = paste0("'", comid, "'"))
      
    }
    
    # replace NAs with  0
    septic_effic_dat_tn[is.na(septic_effic_dat_tn)] <- 0
  }
  
#### end edits ####
  
  # Format point source BMP costs data
  
  temp_point_costs_dat <- bmp_costs_point[
    bmp_costs_point$COMID %in% streamcat_subset_all$comid,
  ] %>% 
    select(comid = COMID, State, BMP, CostType, Cost)
  
  temp_point_costs_capital <- temp_point_costs_dat %>%
    filter(CostType == "Capital") %>%
    pivot_wider(id_cols = c(comid, State), names_from = BMP, values_from = Cost) %>%
    mutate(across(.cols = everything(), .fns = ~replace_na(., 1e30))) %>% # Extremely high cost (1e^30) deters model from selecting BMPs without data
    select(comid, State, any_of(Point_BMPs)) # selection forces the order
  
  temp_point_costs_operations <- temp_point_costs_dat %>%
    filter(CostType == "OM") %>%
    pivot_wider(id_cols = c(comid, State), names_from = BMP, values_from = Cost) %>%
    mutate(across(.cols = everything(), .fns = ~replace_na(., 1e30))) %>% # Extremely high cost (1e^30) deters model from selecting BMPs without data
    select(comid, State, any_of(Point_BMPs)) # selection forces the order
  
  temp_point_costs_other <- as.data.frame(
    streamcat_subset_all[
      !(streamcat_subset_all$comid %in% temp_point_costs_dat$comid),
    ]
  )
  names(temp_point_costs_other) <- "comid"
  temp_point_costs_other_rev <- merge(
    temp_point_costs_other, COMID_State, by = c("comid"), all.x = TRUE
  )
  
  temp_point_costs_other_rev[3:(2+length(Point_BMPs))] <- 0
  names(temp_point_costs_other_rev) <- c(
    "comid", "State", Point_BMPs
  )
  
  if(nrow(temp_point_costs_capital) > 0) {
    temp_point_capital_costs_dat_rev <- rbind(
      temp_point_costs_capital, temp_point_costs_other_rev
    )
  } else {
    temp_point_capital_costs_dat_rev <- temp_point_costs_other_rev
  }
  
  temp_point_capital_costs_dat_rev <- temp_point_capital_costs_dat_rev[
    order(temp_point_capital_costs_dat_rev$comid),
  ]
  temp_point_capital_costs_dat_rev$comid_form <- paste0(
    "'", temp_point_capital_costs_dat_rev$comid, "'"
  )
  point_costs_capital_dat <- temp_point_capital_costs_dat_rev %>% 
    select(comid_form, all_of(Point_BMPs))
  
  if(nrow(temp_point_costs_operations) > 0) {
    temp_point_operations_costs_dat_rev <- rbind(
      temp_point_costs_operations, temp_point_costs_other_rev
    )
  } else {
    temp_point_operations_costs_dat_rev <- temp_point_costs_other_rev
  }
  
  temp_point_operations_costs_dat_rev <- temp_point_operations_costs_dat_rev[
    order(temp_point_operations_costs_dat_rev$comid),
  ]
  temp_point_operations_costs_dat_rev$comid_form <- paste0(
    "'", temp_point_operations_costs_dat_rev$comid, "'"
  )
  point_costs_operations_dat <- temp_point_operations_costs_dat_rev %>% 
    select(comid_form, all_of(Point_BMPs))
  
  # Format user defined limits on BMP implementation
  
  temp_bmp_imp <- user_specs_BMPs %>% 
    select(BMP_Category, BMP, BMP_Selection, frac_min, frac_max) %>%
    filter(BMP_Selection == "X") 
  
  urban_bmp_imp <- temp_bmp_imp %>% 
    filter(BMP_Category == "urban") %>% 
    select(BMP, frac_min, frac_max) %>%
    mutate(BMP = gsub("/", "", BMP)) %>%
    filter(BMP %in% bmp_urban_vec_direct) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  
  road_bmp_imp <- temp_bmp_imp %>%
    filter(BMP_Category == "road") %>%
    select(BMP, frac_min, frac_max) %>%
    mutate(BMP = gsub("/", "", BMP)) %>%
    filter(BMP %in% bmp_road_vec_direct) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  
  ag_bmp_imp <- temp_bmp_imp %>% 
    filter(BMP_Category == "ag") %>% 
    select(BMP, frac_min, frac_max) %>%
    filter(BMP %in% bmp_ag_vec_direct) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  
  graz_bmp_imp <- temp_bmp_imp %>% 
    filter(BMP_Category == "graz") %>% 
    select(BMP, frac_min, frac_max) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  
  ripbuf_bmp_imp <- temp_bmp_imp %>% 
    filter(BMP_Category == "ripbuf") %>% 
    select(BMP, frac_min, frac_max) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  

  ##### h. Make urban matrix of implementability ------
  
  if(length(Urban_BMPs) > 0) {
    urban_bmp_implementationpotential_dat <- setNames(
      data.frame(
        matrix(
          ncol = length(bmp_urban_vec_direct) + 1, nrow = length(urban_costs_capital_dat$comid_form)
        )
      ), 
      c("comid", bmp_urban_vec_direct)
    ) %>%
      mutate(
        comid_form = urban_costs_capital_dat$comid_form
      ) %>%
      mutate(across(.cols = any_of(bmp_urban_vec_direct), .fns = ~1)) %>%
      crossing(., urban_land_use) %>%
      crossing(., urban_hsg) %>%
      rename(Land_Use = urban_land_use, HSG = urban_hsg) %>%
      mutate(Land_Use = paste0("'", Land_Use, "'"),
             HSG = paste0("'", HSG, "'")) %>%
      select(comid_form, Land_Use, HSG, any_of(bmp_urban_vec_direct)) %>%
      arrange(comid_form, Land_Use, HSG)
  }
  ##### h2. Make road matrix of implementability ------
  
  if(length(road_BMPs) > 0) {
    road_bmp_implementationpotential_dat <- setNames(
      data.frame(
        matrix(
          ncol = length(bmp_road_vec_direct) + 1, nrow = length(road_costs_capital_dat$comid_form)
        )
      ), 
      c("comid", bmp_road_vec_direct)
    ) %>%
      mutate(
        comid_form = road_costs_capital_dat$comid_form
      ) %>%
      mutate(across(.cols = any_of(bmp_road_vec_direct), .fns = ~1)) %>%
      crossing(., road_hsg) %>%
      rename(HSG = road_hsg) %>%
      mutate(HSG = paste0("'", HSG, "'")) %>%
      select(comid_form, HSG, any_of(bmp_road_vec_direct)) %>%
      arrange(comid_form, HSG)
  }
  
  ##### i. Make ag matrix of implementability ------  
  # Drainage_Management can only be implemented on tile-drained areas
  # Users also can restrict the custom BMPs to tile-drained areas
  
  if(length(Ag_BMPs) > 0) {
    
    ag_bmp_implementationpotential_dat <- setNames(
      data.frame(
        matrix(
          ncol = length(Ag_BMPs) + 1, nrow = length(streamcat_subset_all$comid)
        )
      ), 
      c("comid", Ag_BMPs)
    ) %>%
      mutate(
        comid = streamcat_subset_all$comid,
        comid_form = paste0("'", comid, "'")
      ) %>%
      left_join(., pct_tiled, by = "comid") %>%
      mutate(
        pct_tiled_adj = replace_na(pct_tiled_adj, 0),
        across(
          .cols = all_of(Ag_BMPs), 
          .fns = ~case_when(
            cur_column() %in% TileAg_BMPs ~ pct_tiled_adj,
            !(cur_column() %in% TileAg_BMPs)  ~ 1
          )
        )
      ) %>%
      select(comid_form, all_of(Ag_BMPs))
    
  }

#### CTC edited: 2/5/2026 ####
  
  ### Make matrix of implementability (max parcels for each COMID for each class of septic upgrades) for septic BMPs
  
  if(length(Septic_BMPs) > 0){
    
    # Define implementation potential for septic upgrades
    septic_implementationpotential_upgrade <- septic.upgrade%>%
      select(comid:parcels)%>%
      pivot_wider(
        names_from = SepticUpgradeClass,
        values_from = parcels
      ) %>%
      right_join(
        data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
        by = "comid"
      )
    
    # rename cols to match BMPs
    colnames(septic_implementationpotential_upgrade)[2:9] <- 
      paste0("class_", colnames(septic_implementationpotential_upgrade)[2:9], "_upgrade")
    
    # replace NAs with 0s
    septic_implementationpotential_upgrade[is.na(septic_implementationpotential_upgrade)] <- 0
    
    # if sewer conversion is selected, add to implementation potential
    if(("Sewer_convert" %in% Septic_BMPs) == TRUE){
      
      # conversion implementation
      septic_implementationpotential_convert <- septic.conversion %>%
        right_join(
          data.frame(comid = unique(unlist(streamcat_subset_tn, use.names = FALSE))),
          by = "comid"
        ) %>%
        select(comid, Sewer_convert = parcels)
      
      # merge with septic implementation potential
      septic_bmp_implementationpotential <- merge(
        septic_implementationpotential_upgrade, 
        septic_implementationpotential_convert, 
        by = "comid", all = TRUE
      ) %>%
        mutate(comid_form = paste0("'", comid, "'"))
      
      # replace NA with 0
      septic_bmp_implementationpotential$Sewer_convert[is.na(septic_bmp_implementationpotential$Sewer_convert)] <-0
      
      # merge with totals
      septic_bmp_implementationpotential <- merge(
        septic_bmp_implementationpotential, 
        total_septic_totals %>% 
          select(comid, final_total),
        by = "comid", all.x = TRUE
      )%>%
        rename(total_parcels = final_total)
      
      # replace NAs with super high value so that the load/total = 0
      septic_bmp_implementationpotential <- septic_bmp_implementationpotential%>%
        mutate(
          total_parcels = case_when(
            is.na(total_parcels) ~ 1e25,
            total_parcels == 0 ~ 1e25,
            TRUE ~ total_parcels
          )
        )
      
    } else {
      
      # Sewer conversions not found, move forwar with just upgrades
      septic_bmp_implementationpotential <- septic_implementationpotential_upgrade %>%
        mutate(comid_form = paste0("'", comid, "'"))
      
      # merge with totals
      septic_bmp_implementationpotential <- merge(
        septic_bmp_implementationpotential, 
        septic.upgrade_totals %>% 
          select(comid, total_parcels),
        by = "comid", all.x = TRUE
      )%>%
        rename(total_parcels = total_parcels)
      
      # replace NAs with large value so that the load/total = 0
      septic_bmp_implementationpotential <- septic_bmp_implementationpotential%>%
        mutate(
          total_parcels = case_when(
            is.na(total_parcels) ~ 1e25,
            total_parcels == 0 ~ 1e25,
            TRUE ~ total_parcels
          )
        )
      # Would be able to replace this with 0 here too
    }
    
  }
  
#### end edits ####
  
  ##### j. Write AMPL data file ------
  
  write( "# Tier 1 Data AMPL File", file = paste(OutPath,"STdata_seasonal.dat",sep = ""))
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(inc_tn_dat)) %do% {
        write(
          paste0("\nparam baseloads_N", i, " : 'point' 'urban' 'road' 'ag' 'septic' 'graz' 'storage' 'other' :="), # CTC edited: 2/5/2026
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = T
        )
        write.table(
          inc_tn_dat[[i]] %>% 
            select(
              comid_form, season, year, point = in_poin, urban = in_urb, road = in_road, ag = in_ag, septic = in_septic, graz = in_graz, # CTC edited: 2/5/2026
              storage = in_storage, other = in_other
            ) %>%
            arrange(comid_form, year, season) %>%
            unique(), # renaming also forces the order in case they are disordered in processing code above
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
        write( ";", file = paste(OutPath,"STdata_seasonal.dat",sep = ""), append = T)
      }
    }
  )

# ND 6/18/26 Add parameter for total baseline loads season x year, 8/4/26 add missing septic to list
  
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(inc_tp_dat)) %do% {
        write(
          paste0("\nparam baseloads_P", i, " : 'point' 'urban' 'road' 'ag' 'septic' 'graz' 'storage' 'other' :="), 
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = T
        )
        write.table(
          inc_tp_dat[[i]] %>% 
            select(
              comid_form, season, year, point = ip_poin, urban = ip_urb, road = ip_road, ag = ip_ag, septic = ip_septic, graz = ip_graz,
              storage = ip_storage, other = ip_other
            ) %>%
            arrange(comid_form,  year, season) %>%
            unique(), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
        write( ";", file = paste(OutPath,"STdata_seasonal.dat",sep = ""), append = T)
      }
    }
  )
  
  ### TN Storage Coefficients ------
  invisible(
    if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(sm_N_dat)) %do% {
        
        temp_sm_N_dat <- sm_N_dat[[i]]
        
        if (!is.null(temp_sm_N_dat) && nrow(temp_sm_N_dat) > 0) {
          
          # Write the header for the storage coefficients
          write(
            paste0("\nparam transfer_coefficients_N", i, " : 'coeff' 'notrib' :="),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE
          )
          
          # Write the data ND 6/28/26 remove notrib
          write.table(
            temp_sm_N_dat %>% # select(-notrib) %>% 
              mutate(year = as.numeric(year)),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            na = "",
            quote = FALSE
          )
          
          # Close the table with a semicolon
          write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = TRUE)
        } else {
          # If the data is NULL or empty, write a message
          write(
            paste0("\nparam transfer_coefficients_N", i, " : 'coeff' 'notrib' :=\n;"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE
          )
        }
      }
    }
  )
  
  ### TP Storage Coefficients -----
  invisible(
    if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(sm_P_dat)) %do% {
        
        temp_sm_P_dat <- sm_P_dat[[i]]
        
        if (!is.null(temp_sm_P_dat) && nrow(temp_sm_P_dat) > 0) {
          
          # Write the header for the storage coefficients
          write(
            paste0("\nparam transfer_coefficients_P", i, " : 'coeff' 'notrib' :="),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE
          )
          
          # Write the data ND 6/28/26 remove notrib
          write.table(
            temp_sm_P_dat %>%  # select(-notrib) %>%
              mutate(year = as.numeric(year)),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            na = "",
            quote = FALSE
          )
          
          # Close the table with a semicolon
          write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = TRUE)
        } else {
          # If the data is NULL or empty, write a message
          write(
            paste0("\nparam transfer_coefficients_P", i, " : 'coeff' 'notrib' :=\n;"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE
          )
        }
      }
    }
  )
  
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(riparian_tn_dat)) %do% {
        if (!is.null(riparian_tn_dat[[i]]) && nrow(riparian_tn_dat[[i]]) > 0) {
          temp_ripar <- riparian_tn_dat[[i]]
          write(
            paste0("\nparam riparianload_N", i, " : 'riparian' :="), 
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T
          )
          
          formatted_data <- temp_ripar %>%
            select(comid_form, season, year, riparian = in_riparian) %>% # renaming also forces the order in case they are disordered in processing code above
            arrange(comid_form,  year, season) %>%
            unique()  %>%
            # replace NA values with 0
            mutate(riparian = ifelse(is.na(riparian), 0, riparian)) 
          
          if(length(RiparianBuffer_BMPs) > 0) {
            write.table(
              formatted_data,
              file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          } else {
            write.table(
             ripbuf_bmp_dummy_tn[[i]] %>%
              select(comid, none = none) %>%
              arrange(comid) %>%
              unique(), # renaming also forces the order in case they are disordered in processing code above
                file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          }
          write( ";", file = paste(OutPath,"STdata_seasonal.dat",sep = ""), append = T) 
        }
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(riparian_tp_dat)) %do% {
        
        if (!is.null(riparian_tp_dat[[i]]) && nrow(riparian_tp_dat[[i]]) > 0) {
          
          temp_ripar <- riparian_tp_dat[[i]]
          
          # Write the header for the table
          write(
            paste0("\nparam riparianload_P", i, " : 'riparian' :="), #*#,
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE
          )
          
          # Prepare the data in the required format
          formatted_data <- temp_ripar %>%
            mutate(year = as.numeric(year)) %>%
            select(
              comid_form,
              season,
              year,
              riparian = ip_riparian
            ) %>%
            arrange(comid_form,  year, season) %>%
            # replace NA values with 0
            mutate(riparian = ifelse(is.na(riparian), 0, riparian))
          
          if(length(RiparianBuffer_BMPs) > 0) {
            # Write the formatted data to the file
            write.table(
              formatted_data,
              file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
              append = TRUE,
              sep = "\t",
              row.names = FALSE,
              col.names = FALSE,
              na = "",
              quote = FALSE
            )
          } else {
              write.table(
                ripbuf_bmp_dummy_tp[[i]] %>%
                select(comid, none = none) %>%
                arrange(comid) %>%
                unique(), # renaming also forces the order in case they are disordered in processing code above
              file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          }
          write( ";", file = paste(OutPath,"STdata_seasonal.dat",sep = ""), append = T)
        }
      }
    }
  )
  
  write( 
    "\nparam area : 'ag' 'graz' :=", 
    file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
    append = T
  )
  write.table(
    area_dat %>%
      select(comid_form, ag = ag_ac_bmpadj, graz = graz_ac) %>%
      arrange(comid_form) %>%
      unique(), # renaming also forces the order in case they are disordered in processing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  write( "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  write( 
    paste0("param urban_area : 'urban' :="), 
    file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
    append = T
  )
  write.table(
    urban_area_dat %>%
      select(comid_form, Land_Use, HSG, urban_ac) %>%
      arrange(comid_form) %>%
      unique(), # renaming also forces the order in case they are disordered in processing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  write( "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  write( 
    paste0("param road_area : 'road' :="), 
    file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
    append = T
  )
  write.table(
    road_area_dat %>%
      select(comid_form, HSG, road_ac) %>%
      arrange(comid_form) %>%
      unique(), # renaming also forces the order in case they are disordered in processing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  if(length(Urban_BMPs) > 0) {
    write(
      paste0(
        "\nparam urban_bmp_implementationpotential : ", 
        paste(bmp_urban_vec, collapse = "  "), 
        " :="
      ),
      file = paste0(OutPath, "STdata_seasonal.dat"),
      append = TRUE
    )
    write.table(
      urban_bmp_implementationpotential_dat %>% 
        select(comid_form, Land_Use, HSG, any_of(bmp_urban_vec_direct)) %>%
        arrange(comid_form, Land_Use, HSG) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      paste0("\nparam urban_bmp_implementationpotential : 'none' :="),
      file = paste0(OutPath, "STdata_seasonal.dat"),
      append = TRUE
    )
    write.table(
      urban_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

# ND 7/20/26 Added road_bmp_implementationpotential
  if(length(road_BMPs) > 0) {
    write(
      paste0(
        "\nparam road_bmp_implementationpotential : ", 
        paste(bmp_road_vec, collapse = "  "), 
        " :="
      ),
      file = paste0(OutPath, "STdata_seasonal.dat"),
      append = TRUE
    )
    write.table(
      road_bmp_implementationpotential_dat %>% 
        select(comid_form, HSG, any_of(bmp_road_vec_direct)) %>%
        arrange(comid_form, HSG) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      paste0("\nparam road_bmp_implementationpotential : 'none' :="),
      file = paste0(OutPath, "STdata_seasonal.dat"),
      append = TRUE
    )
    write.table(
      road_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  if(length(Septic_BMPs) > 0 & "TN" %in% user_specs_loadingtargets$TN_or_TP) {
    write(
      paste0(
        "\nparam septic_bmp_implementationpotential : ", 
        paste(bmp_septic_vec, collapse = "  "), 
        ":="
      ),
      file = paste0(OutPath, "STdata_seasonal.dat"),
      append = TRUE
    )
    write.table(
      septic_bmp_implementationpotential%>% 
        select(comid_form, any_of(Septic_BMPs)), # selection forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    write( 
      "\nparam septic_bmp_implementationtotal : 'total_parcels' :=", # new version taken from working component; still not working. receiving same NEOS error
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      append = T
    )
    write.table(
      septic_bmp_implementationpotential%>% 
        select(comid_form, total_parcels), # selection forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    # Write dummy septic parameters if TN exists globally (mirrors TP behavior)
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      write(
        paste0("\nparam septic_bmp_implementationpotential: 'none' :="),
        file = paste0(OutPath, "STdata_seasonal.dat"),
        append = TRUE
      )
      write.table(
        septic_bmp_dummy %>% 
          select(comid_form, none), # selection forces the order incase they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
  }
  
#### end edits ####
  
  if(length(Ag_BMPs) > 0) {
    write(
      paste0(
        "\nparam ag_bmp_implementationpotential : ", 
        paste(bmp_ag_vec, collapse = "  "), 
        " :="
      ),
      file = paste0(OutPath, "STdata_seasonal.dat"),
      append = TRUE
    )
    write.table(
      ag_bmp_implementationpotential_dat %>% 
        select(comid_form, any_of(Ag_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      paste0("\nparam ag_bmp_implementationpotential : 'none' :="),
      file = paste0(OutPath, "STdata_seasonal.dat"),
      append = TRUE
    )
    write.table(
      ag_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  if(length(RiparianBuffer_BMPs) > 0) {
    write( 
      paste0(
        "\nparam unbuffered_banklength : ",  
        paste(bmp_ripbuf_vec, collapse  = "  "), 
        " :="
      ), 
      file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
      append = T
    )
    write.table(
      streambanklength_available_dat %>% 
        select(comid_form, any_of(RiparianBuffer_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write( 
      paste0("\nparam unbuffered_banklength: 'none' :="), 
      file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
      append = T
    )
    write.table(
      ripbuf_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  write( 
    paste0("\nparam total_banklength :="), 
    file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
    append = T
  )
  if(length(RiparianBuffer_BMPs) > 0) {
    write.table(
      streambanklength_total_dat %>% 
        select(comid_form, totalbanklength_ft) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write.table(
      ripbuf_bmp_dummy %>% 
        select(comid, none) %>% 
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  if(length(Ag_BMPs) > 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam ag_effic_N : ",bmp_ag_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        ag_effic_dat_tn %>% 
          select(comid_form, all_of(bmp_ag_vec_direct)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam ag_effic_P : ",bmp_ag_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        ag_effic_dat_tp %>%
          select(comid_form, all_of(bmp_ag_vec_direct)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
  } else {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam ag_effic_N : 'none' :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", ag_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam ag_effic_P : 'none' :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", ag_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
  }
  
#### CTC edited: 2/5/2026 ####
  
  if(length(Septic_BMPs) > 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        paste0("\nparam septic_effic_N : ",paste(bmp_septic_vec, collapse = " ")," :=	"), 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        septic_effic_dat_tn %>%
          select(comid_form, all_of(bmp_septic_vec_direct)),
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
    
  } else {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam septic_effic_N : 'none' :=	", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", septic_bmp_dummy_tn)) %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
    
  }
  
#### end edits ####
  
  if(length(Graz_BMPs) > 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam graz_effic_N : ",bmp_graz_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        graz_effic_dat_tn %>% 
          select(comid_form, all_of(Graz_BMPs)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam graz_effic_P : ",bmp_graz_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        graz_effic_dat_tp %>% 
          select(comid_form, all_of(Graz_BMPs)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
  } else {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam graz_effic_N : 'none' :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", graz_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam graz_effic_P : 'none' :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", graz_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
  }
  
  
  if(length(Point_BMPs) > 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_N : ",bmp_point_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        point_effic_dat_tn %>% 
          select(comid_form, all_of(Point_BMPs)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_P : ",bmp_point_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        point_effic_dat_tp %>% 
          select(comid_form, all_of(Point_BMPs)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
  } else {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_N : 'none' :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", point_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_P : 'none' :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", point_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
  }
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    if(length(Urban_BMPs) > 0) {
      cat(
        "\nparam urban_effic_N : ",bmp_urban_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        urban_effic_dat_tn %>%
          select(comid_form, season, year, Land_Use, HSG, all_of(bmp_urban_direct)) %>%
          arrange(comid_form,  year, season, Land_Use, HSG)%>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        "\nparam urban_effic_N : 'none' :=", 
        file = paste0(OutPath, "STdata_seasonal.dat"), 
        sep = "", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", urban_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    if(length(Urban_BMPs) > 0) {
      cat(
        "\nparam urban_effic_P : ",bmp_urban_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        urban_effic_dat_tp %>%
          select(comid_form, season, year, Land_Use, HSG, all_of(bmp_urban_direct)) %>%
          arrange(comid_form,  year, season, Land_Use, HSG) %>%
          unique(),  # selecting forces the order in case they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        "\nparam urban_effic_P: 'none' :=", 
        file = paste0(OutPath, "STdata_seasonal.dat"), 
        sep = "", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", urban_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    if(length(road_BMPs) > 0) {
      cat(
        "\nparam road_effic_N : ",bmp_road_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        road_effic_dat_tn %>%
          select(comid_form, season, year, HSG, all_of(road_BMPs)) %>%
          arrange(comid_form,  year, season, HSG)%>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        "\nparam road_effic_N : 'none' :=", 
        file = paste0(OutPath, "STdata_seasonal.dat"), 
        sep = "", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", road_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    if(length(road_BMPs) > 0) {
      cat(
        "\nparam road_effic_P : ",bmp_road_vec," :=", 
        file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        road_effic_dat_tp %>%
          select(comid_form, season, year, HSG, all_of(road_BMPs)) %>%
          arrange(comid_form,  year, season, HSG) %>%
          unique(),  # selecting forces the order in case they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        "\nparam road_effic_P: 'none' :=", 
        file = paste0(OutPath, "STdata_seasonal.dat"), 
        sep = "", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", urban_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        
        temp_ripar_rem <- riparian_tn_removal[[i]]
        
        if (!is.null(temp_ripar_rem) && nrow(temp_ripar_rem) > 0) {
          write(
            paste0(
              "\nparam riparianremoval_N",
              i,
              " : 'Grassed_Buffer' 'Forested_Buffer':="
            ),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T
          )
          
          # Prepare the data in the required format
          formatted_data <- temp_ripar_rem %>%
            mutate(year = as.numeric(year)) %>%
            select(
              comid_form,
              season,
              year,
              `'Grassed_Buffer'` = Grassed_Buffer,
              `'Forested_Buffer'` = Forested_Buffer
            ) %>%
            arrange(comid_form,  year, season)
          
          # Write the formatted data to the file
          write.table(
            formatted_data,
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            na = "",
            quote = FALSE
          )
        } else {
          write(
            paste0("\nparam riparianremoval_N", i, " : 'none' :="),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T
          )
          write.table(
            ripbuf_bmp_dummy_tn[[i]] %>%
              select(comid, none),
            # renaming also forces the order incase they are disordered in preprocessing code above
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        write(
          ";",
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tp)) %do% {
        
        temp_ripar_rem <- riparian_tp_removal[[i]]
        
        if (!is.null(temp_ripar_rem) && nrow(temp_ripar_rem) > 0) {
          write(
            paste0(
              "\nparam riparianremoval_P",
              i,
              " : 'Grassed_Buffer' 'Forested_Buffer':="
            ),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T
          )
          
          # Prepare the data in the required format
          formatted_data <- temp_ripar_rem %>%
            mutate(year = as.numeric(year)) %>%
            select(
              comid_form,
              season,
              year,
              `'Grassed_Buffer'` = Grassed_Buffer,
              `'Forested_Buffer'` = Forested_Buffer
            ) %>%
            arrange(comid_form,  year, season)
          
          # Write the formatted data to the file
          write.table(
            formatted_data,
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            na = "",
            quote = FALSE
          )
        } else {
          write(
            paste0("\nparam riparianremoval_P", i," : 'Grassed_Buffer' 'Forested_Buffer' :="),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T
          )
        }
        write(
          ";",
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = T
        )
      }
    }
  )
  
  if(length(Graz_BMPs) > 0) {
    cat(
      "\nparam graz_costs_capital : ",bmp_graz_vec," :=	",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      graz_costs_cap_dat %>% 
        select(comid_form, all_of(Graz_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam graz_costs_operations : ",bmp_graz_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      graz_costs_op_dat %>% 
        select(comid_form, all_of(Graz_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam graz_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      graz_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam graz_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      graz_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  if(length(Ag_BMPs) > 0) {
    cat(
      "\nparam ag_costs_capital : ",bmp_ag_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ag_costs_cap_dat %>% 
        select(comid_form, all_of(bmp_ag_vec_direct)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam ag_costs_operations : ",bmp_ag_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ag_costs_op_dat %>% 
        select(comid_form, all_of(bmp_ag_vec_direct)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam ag_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ag_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam ag_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ag_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }

#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####

  ### Septic BMP Costs ------
  if(length(Septic_BMPs) > 0 & "TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      paste0("\nparam septic_costs_capital : ",paste(bmp_septic_vec, collapse = " ")," :=	"),
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      septic_cap_costs %>% select(comid_form, all_of(bmp_septic_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      paste0("\nparam septic_costs_operations : ", paste(bmp_septic_vec, collapse = " ")," :=	"),
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      septic_op_costs %>% select(comid_form, all_of(bmp_septic_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    # Write dummy septic costs if TN exists globally (mirrors TP behavior)
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam septic_costs_capital : 'none' :=	",
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "", 
        append = T
      )
      cat(
        "\n",
        file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "", 
        append = T
      )
      write.table(
        septic_bmp_dummy %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na="",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
      
      cat(
        "\nparam septic_costs_operations : 'none' :=	",
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "", 
        append = T
      )
      cat(
        "\n",
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "", 
        append = T
      )
      write.table(
        septic_bmp_dummy %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    }
  }
  
#### end edits ####
  
  if(length(Point_BMPs) > 0) {
    cat(
      "\nparam point_costs_capital : ",bmp_point_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      point_costs_capital_dat %>% 
        select(comid_form, all_of(Point_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam point_costs_operations : ",bmp_point_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      point_costs_operations_dat %>% 
        select(comid_form, all_of(Point_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam point_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      point_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam point_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      point_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }

  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_costs_capital : ",bmp_urban_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      urban_costs_capital_dat %>% 
        select(comid_form, Land_Use, HSG, all_of(bmp_urban_direct)) %>%
        arrange(comid_form, Land_Use, HSG) %>%
        mutate(across(where(is.list), unlist)) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam urban_costs_operations : ",bmp_urban_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      urban_costs_operations_dat %>% 
        select(comid_form, Land_Use, HSG, all_of(bmp_urban_direct))%>%
        arrange(comid_form, Land_Use, HSG) %>%
        mutate(across(where(is.list), unlist)) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam urban_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      urban_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam urban_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      urban_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  

  if(length(road_BMPs) > 0) {
    cat(
      "\nparam road_costs_capital : ",bmp_road_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      road_costs_capital_dat %>% 
        select(comid_form, HSG, all_of(bmp_road_direct)) %>%
        arrange(comid_form, HSG) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam road_costs_operations : ",bmp_road_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      road_costs_operations_dat %>%
        select(comid_form, HSG, all_of(bmp_road_direct))%>%
        arrange(comid_form, HSG) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam road_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      road_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam road_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      road_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  if(length(RiparianBuffer_BMPs) > 0) {
    cat(
      "\nparam ripbuf_costs_capital : ", bmp_ripbuf_vec, " :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ripbuf_costs_cap_dat %>% 
        select(comid_form, all_of(RiparianBuffer_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam ripbuf_costs_operations : ", bmp_ripbuf_vec," :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ripbuf_costs_op_dat %>% 
        select(comid_form, all_of(RiparianBuffer_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam ripbuf_costs_capital : 'none' :=	",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ripbuf_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam ripbuf_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ripbuf_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        
        temp_load_lim <- param_loads_lim_tn[[i]]
        
        if (nrow(temp_load_lim) > 0) {
          cat(
            paste0("\nparam loads_lim_N", i, " : 'limits' :=  \n"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            sep = "",
            append = T
          )
          write.table(
            temp_load_lim %>%
              mutate(year = as.numeric(year)) %>%
              select(season, year, load_lim) %>%
              arrange(year,season),
            # renaming also forces the order incase they are disordered in processing code above
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        } else {
          cat(
            paste0("\nparam loads_lim_N", i, " : 'limits' :=  \n"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            sep = "",
            append = T
          )
        } 
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
      }
    }
  )

  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tp)) %do% {
        
        temp_load_lim <- param_loads_lim_tp[[i]]
        
        if (nrow(temp_load_lim) > 0) {
          cat(
            paste0("\nparam loads_lim_P", i, " : 'limits' :=  \n"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            sep = "",
            append = T
          )
          write.table(
            temp_load_lim %>%
              mutate(year = as.numeric(year)) %>%
              select(season, year, load_lim) %>%
              arrange(year,season),
            # renaming also forces the order incase they are disordered in processing code above
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        } else {
          cat(
            paste0("\nparam loads_lim_P", i, " : 'limits' :=  \n"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            sep = "",
            append = T
          )
        }
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
      }
    }
  )
  
  # ND 6/19/26 Add print out of total loads season x year
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_total_loads_tn)) %do% {
        
        temp_total_load <- param_total_loads_tn[[i]]
        
        if (nrow(temp_total_load) > 0) {
          cat(
            paste0("\nparam total_load_N", i, " : 'total_load' :=  \n"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            sep = "",
            append = T
          )
          write.table(
            temp_total_load %>%
              mutate(year = as.numeric(year)) %>%
              select(season, year, total_load) %>%
              arrange(year,season),
            # renaming also forces the order incase they are disordered in processing code above
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        } else {
          cat(
            paste0("\nparam total_load_N", i, " : 'total_load' :=  \n"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            sep = "",
            append = T
          )
        } 
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
      }
    }
  )
# ND 8/7/26 fixed parameter name for param total_load_P  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_total_loads_tp)) %do% {
        
        temp_total_load <- param_total_loads_tp[[i]]
        
        if (nrow(temp_total_load) > 0) {
          cat(
            paste0("\nparam total_load_P", i, " : 'total_load' :=  \n"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            sep = "",
            append = T
          )
          write.table(
            temp_total_load %>%
              mutate(year = as.numeric(year)) %>%
              select(season, year, total_load) %>%
              arrange(year,season),
            # renaming also forces the order incase they are disordered in processing code above
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        } else {
          cat(
            paste0("\nparam total_load_P", i, " : 'total_load' :=  \n"),
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            sep = "",
            append = T
          )
        }
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T)
  # ND 6/18/26 end addition
  
  cat(
    "\nparam agcost_frac :=",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    sep = "", 
    append = T
  )
  cat(
    agcost_frac, 
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    sep = "", 
    append = T
  )
  cat(
    ";",file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
  )
  
  cat(
    "
param agBMP_minarea := 1;
param grazBMP_minarea := 1;
param urbanBMP_minarea := 1;
param roadBMP_minarea := 1;",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "\n", append = T
  )
  
  write( 
    "\nparam: urban_frac_min urban_frac_max :=", 
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T
  )
  if(length(Urban_BMPs) > 0) {
    write.table(
      urban_bmp_imp,
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      append = T
    )
  }
  
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  write( 
    "\nparam: road_frac_min road_frac_max :=", 
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T
  )
  if(length(road_BMPs) > 0) {
    write.table(
      road_bmp_imp,
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      append = T
    )
  }
  
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  write( 
    "\nparam: ag_frac_min ag_frac_max :=", 
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T
  )
  if(length(Ag_BMPs) > 0) {
    write.table(
      ag_bmp_imp,
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      append = T
    )
  }
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  write( 
    "\nparam: graz_frac_min graz_frac_max :=", 
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T
  )
  if(length(Graz_BMPs) > 0) {
    write.table(
      graz_bmp_imp,
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      append = T
    )
  }
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  write( 
    "\nparam: ripbuf_frac_min ripbuf_frac_max :=", 
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T
  )
  if(length(RiparianBuffer_BMPs) > 0) {
    write.table(
      ripbuf_bmp_imp,
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      append = T
    )
  }
  
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  

  ### III. Develop model file ------
  
  ##### a. Save COMIDs as vector ------
  
  comid_vec_all <- paste0("'", unique(streamcat_subset_all$comid),"'")
  comid_vec_all[-length(comid_vec_all)] <- paste0(
    comid_vec_all[-length(comid_vec_all)], ','
  )
  
  comid_vec_all_N <- paste0(
    "'", unique(unlist(streamcat_subset_tn, use.names = FALSE)), "'"
  )
  comid_vec_all_N[-length(comid_vec_all_N)] <- paste0(
    comid_vec_all_N[-length(comid_vec_all_N)], ','
  )
  
  comid_vec_all_P <- paste0(
    "'", unique(unlist(streamcat_subset_tp, use.names = FALSE)), "'"
  )
  comid_vec_all_P[-length(comid_vec_all_P)] <- paste0(
    comid_vec_all_P[-length(comid_vec_all_P)], ','
  )
  
  
  comid_vec_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
    if (length(unlist(streamcat_subset_tn[[i]])) > 0) {
      comid_vec_N_tmp <- paste0("'", streamcat_subset_tn[[i]]$comid, "'")
      comid_vec_N_tmp[-length(comid_vec_N_tmp)] <- paste0(
        comid_vec_N_tmp[-length(comid_vec_N_tmp)], ','
      )
      comid_vec_N_tmp} else {
        comid_vec_N_tmp <- ""
        comid_vec_N_tmp
      }
  }
  
  comid_vec_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
    if (length(unlist(streamcat_subset_tp[[i]])) > 0) {
      comid_vec_P_tmp <- paste0("'", streamcat_subset_tp[[i]]$comid, "'")
      comid_vec_P_tmp[-length(comid_vec_P_tmp)] <- paste0(
        comid_vec_P_tmp[-length(comid_vec_P_tmp)], ','
      )
      comid_vec_P_tmp} else {
        comid_vec_P_tmp <- ""
        comid_vec_P_tmp
      }
  }
  
  # Extract unique years from the 'year' column
  
  unique_years <- unique(as.numeric(year_xwalk$model_year))
  
  min_year <- min(unique_years)
  max_year <- max(unique_years)
  
  urban_land_use <- unique(urban_area_dat$Land_Use)
  urban_land_set <- urban_land_use
  urban_land_set[-length(urban_land_set)] <- paste0(
    urban_land_set[-length(urban_land_set)], ','
  )
  urban_hsg <- unique(urban_area_dat$HSG)
  urban_hsg_set <- urban_hsg
  urban_hsg_set[-length(urban_hsg_set)] <- paste0(
    urban_hsg_set[-length(urban_hsg_set)], ','
  )
  road_hsg <- unique(road_area_dat$HSG)
  road_hsg_set <- road_hsg
  road_hsg_set[-length(road_hsg_set)] <- paste0(
    road_hsg_set[-length(road_hsg_set)], ','
  )
  
  
  ##### b. Write AMPL model file ------
  
  write(
    "#STmodel_seasonal.mod
      \nset comid_all :=", 
    file = paste(OutPath, "STmodel_seasonal.mod", sep = "")
  )
  cat(
    "{",comid_vec_all,"};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
  )
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T)
  

  cat(
    paste0("\nset comid_all_N within comid_all := "),
    file = paste0(OutPath, "STmodel_seasonal.mod"),
    sep = "",
    append = T
  )
  if (length(comid_vec_all_N) == 0 || all(comid_vec_all_N == "''") || all(comid_vec_all_N == "'NA'")) {
    cat(
      "{};",
      file = paste0(OutPath, "STmodel_seasonal.mod"),
      sep = "",
      append = T
    )
  } else {
    cat(
      "{",unique(comid_vec_all_N),"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
    )
  }
  cat(
    "\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
  )


  cat(
    paste0("\nset comid_all_P within comid_all := "),
    file = paste0(OutPath, "STmodel_seasonal.mod"),
    sep = "",
    append = T
  )
  if (length(comid_vec_all_P) == 0 || all(comid_vec_all_P == "''") || all(comid_vec_all_P == "'NA'")) {
    cat(
      "{};",
      file = paste0(OutPath, "STmodel_seasonal.mod"),
      sep = "",
      append = T
    )
  } else {
    cat(
      "{",unique(comid_vec_all_P),"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
    )
  }
  cat(
    "\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
  )

  
invisible(
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0("\nset comid_N", i, " within comid_all_N := "),
        file = paste0(OutPath, "STmodel_seasonal.mod"),
        sep = "",
        append = T
      )
      # Check if comid_vec_N[[i]] is empty or contains only empty strings
      if (length(comid_vec_N[[i]]) == 0 || all(comid_vec_N[[i]] == "''") || all(comid_vec_N[[i]] == "'NA'") || all(comid_vec_N[[i]] == "")) {
        cat(
          "{};",
          file = paste0(OutPath, "STmodel_seasonal.mod"),
          sep = "",
          append = T
        )
      } else {
        cat(
          "{", comid_vec_N[[i]], "};",
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "",
          append = T
        )
      }
      cat(
        "\n",
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "",
        append = T
      )
    }
)

invisible(
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0("\nset comid_P", i, " within comid_all_P := "),
        file = paste0(OutPath, "STmodel_seasonal.mod"),
        sep = "",
        append = T
      )
      # Check if comid_vec_P[[i]] is empty or contains only empty strings
      if (length(comid_vec_P[[i]]) == 0 || all(comid_vec_P[[i]] == "''") || all(comid_vec_P[[i]] == "'NA'")) {
        cat(
          "{};",
          file = paste0(OutPath, "STmodel_seasonal.mod"),
          sep = "",
          append = T
        )
      } else {
        cat(
          "{", comid_vec_P[[i]], "};",
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "",
          append = T
        )
      }
      cat(
        "\n",
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "",
        append = T
      )
    }
)
  
  #Set years and seasons
cat(
  "\nset seasons := 1..4 ordered; ",
  file = paste(OutPath, 'STmodel_seasonal.mod', sep = ""),
  sep = "\n",
  append = T
)
  
  # Create the AMPL set syntax
  ampl_years_set <- paste0("\nset years := ",min_year,"..",max_year," ordered;") #  ordered
  
  # Print the result
  cat(ampl_years_set,
      file = paste(OutPath, 'STmodel_seasonal.mod', sep = ""),
      sep = "\n",
      append = T
  )
  
  # Set land uses
  cat(
    paste0("\nset urban_land_uses := "),
    file = paste0(OutPath, "STmodel_seasonal.mod"),
    sep = "",
    append = T
  )
  cat(
    "{", urban_land_set, "};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), 
    sep = "", 
    append = T
  )
  
  # Set soil groups
  cat(
    paste0("\nset urban_soil_groups := "),
    file = paste0(OutPath, "STmodel_seasonal.mod"),
    sep = "",
    append = T
  )
  cat(
    "{", urban_hsg_set, "};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), 
    sep = "", 
    append = T
  )
  
  # Set road soil groups
  cat(
    paste0("\nset road_soil_groups := "),
    file = paste0(OutPath, "STmodel_seasonal.mod"),
    sep = "",
    append = T
  )
  cat(
    "{", road_hsg_set, "};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), 
    sep = "", 
    append = T
  )
  
  #Set BMPs
  
  cat(
    "\n\nset urban_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Urban_BMPs) > 0) {
    cat(
      "{",bmp_urban_vec_comma,"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  }
  
  
  cat(
    "\n\nset urban_pervbmp within urban_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Urban_BMPs) > 0) {
    if(!grepl("''", paste0(pervbmp_urban_vec_comma, collapse = ""))) {
      cat(
        "{",pervbmp_urban_vec_comma,"};",
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "", 
        append = T
      )  
    } else {
      cat(
        "{};",
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "", 
        append = T
      )  
    }
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )  
  }

  cat(
    "\n\nset road_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(road_BMPs) > 0) {
    cat(
      "{",bmp_road_vec_comma,"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  }

  cat(
    "\n\nset ag_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Ag_BMPs) > 0) {
    cat(
      "{",bmp_ag_vec_comma,"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  cat(
    "\n\nset ag_tilebmp within ag_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Ag_BMPs) > 0) {
    if(!grepl("''", paste0(tilebmp_ag_vec_comma, collapse = ""))) {
      cat(
        "{",tilebmp_ag_vec_comma,"};",
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "", 
        append = T
      )  
    } else {
      cat(
        "{};",
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "", 
        append = T
      )  
    }
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )  
  }

#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  ### Adds septic BMPs to model file ----
  cat(
    "\n\nset septic_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  
  if(length(Septic_BMPs) > 0) {
    cat(
      "{", paste(bmp_septic_vec_comma, collapse = " "), "};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "",
      append = T
    )
    
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
#### end edits ####
  
  cat(
    "\n\nset graz_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Graz_BMPs) > 0) {
    cat(
      "{",bmp_graz_vec_comma,"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  
  cat(
    "\n\nset ripbuf_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(RiparianBuffer_BMPs) > 0) {
    cat(
      "{",bmp_ripbuf_vec_comma,"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  }
  
  cat(
    "\n\nset point_bmp :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Point_BMPs) > 0) {
    cat(
      "{",bmp_point_vec_comma,"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  
  cat(
    "\n\nset loads_all :=
{'point', 'urban', 'road', 'ag', 'septic', 'graz', 'storage', 'other'};
  
set area_sub :=
{'ag', 'graz'};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  
  cat(
    "set urban_area_sub :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Urban_BMPs) > 0) {
    cat(
      "{'urban'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T)
  
  cat(
    "set road_area_sub :=",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(road_BMPs) > 0) {
    cat(
      "{'road'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T)
  
  cat(
    "\nset transfer_types := {'coeff', 'notrib'};\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )
  
  cat(
    "\nset loads_riparian := \n {'riparian'};\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )
  
  cat(
    "\nset limits := \n {'limits'};\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )
  
  cat(
    "\nset total_load := \n {'total_load'};\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )
  
  cat(
    "\nset other_loads := \n {'other_load'};\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )

#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  cat(
    "\nset septic_bmp_totals := \n {'total_parcels'};\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )

#### end edits ####
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T)
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_N)) %do% {
        cat(
          paste0("param baseloads_N", i, " {comid_N", i, ", seasons, years, loads_all} >= 0;"),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_P)) %do% {
        cat(
          paste0("param baseloads_P", i, " {comid_P", i, ", seasons, years, loads_all} >= 0;"),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_N)) %do% {
        cat(
          paste0("param riparianload_N", i, " {comid_N", i, ", seasons, years, loads_riparian} >= 0;"),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_P)) %do% {
        cat(
          paste0("param riparianload_P", i, " {c in comid_P", i, ", seasons, years, loads_riparian} >= 0;"),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )

#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam septic_bmp_implementationpotential {comid_all_N, septic_bmp} >= 0;
param septic_bmp_implementationtotal {comid_all_N, septic_bmp_totals} >= 0;",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }

#### end edits ####
  
  cat(
    "\nparam area {comid_all, area_sub} >=0;
    
param urban_area {comid_all, urban_land_uses, urban_soil_groups, urban_area_sub} >=0;
param road_area {comid_all, road_soil_groups, road_area_sub} >=0;
param urban_bmp_implementationpotential {comid_all, urban_land_uses, urban_soil_groups, urban_bmp} >= 0;
# ND 7/20/26 added
param road_bmp_implementationpotential {comid_all, road_soil_groups, road_bmp} >= 0;
param ag_bmp_implementationpotential {comid_all, ag_bmp} >= 0;
param unbuffered_banklength {comid_all, ripbuf_bmp} >= 0;
param total_banklength {comid_all} >= 0;

param ag_costs_capital {comid_all, ag_bmp};
param ag_costs_operations {comid_all, ag_bmp};
param graz_costs_capital {comid_all, graz_bmp};
param graz_costs_operations {comid_all, graz_bmp};
param point_costs_capital {comid_all, point_bmp};
param point_costs_operations {comid_all, point_bmp};
param urban_costs_capital {comid_all, urban_land_uses, urban_soil_groups, urban_bmp};
param urban_costs_operations {comid_all, urban_land_uses, urban_soil_groups, urban_bmp};
param road_costs_capital {comid_all, road_soil_groups, road_bmp};
param road_costs_operations {comid_all, road_soil_groups, road_bmp};
param ripbuf_costs_capital {comid_all, ripbuf_bmp};
param ripbuf_costs_operations {comid_all, ripbuf_bmp};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n",
    append = T
  )  

#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam septic_costs_capital {comid_all_N, septic_bmp} >= 0;
param septic_costs_operations {comid_all_N, septic_bmp} >= 0;",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }

  cat( # CTC Note: not sure this code is needed
    "\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n",
    append = T
  )

# CTC edited: added septic_effic_N line below
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_N {comid_all_N, ag_bmp};
param graz_effic_N {comid_all_N, graz_bmp};
param point_effic_N {comid_all_N, point_bmp};
param urban_effic_N {comid_all_N, seasons, years, urban_land_uses, urban_soil_groups, urban_bmp};
param road_effic_N {comid_all_N, seasons, years, road_soil_groups, road_bmp};
param septic_effic_N {comid_all_N, septic_bmp};\n",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n", 
      append = T
    )
  }

#### end edits ####

  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_P {comid_all_P, ag_bmp};
param graz_effic_P {comid_all_P, graz_bmp};
param point_effic_P {comid_all_P, point_bmp};
param urban_effic_P {comid_all_P, seasons, years, urban_land_uses, urban_soil_groups, urban_bmp};
param road_effic_P {comid_all_P, seasons, years, road_soil_groups, road_bmp};\n",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n", 
      append = T
    )
  }
  
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_N)) %do% {
        cat(
          paste0(
            "param riparianremoval_N", 
            i, 
            " {c in comid_N", 
            i, 
            ", s in seasons, y in years, b in ripbuf_bmp};"
          ),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP > 0) {
      foreach(i = 1:length(comid_vec_P)) %do% {
        cat(
          paste0(
            "param riparianremoval_P", 
            i, 
            " {c in comid_P", 
            i, 
            ", s in seasons, y in years, b in ripbuf_bmp};"
          ),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(user_specs_loadingtargets$TN_or_TP)) %do% {
        if(user_specs_loadingtargets[i,"TN_or_TP"] == "TN") {
          cat(
            paste0("param loads_lim_N", i, " {seasons, years, limits} >= 0;"),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n",
            append = T
          ) 
        }
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)
  
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(user_specs_loadingtargets$TN_or_TP)) %do% {
        if (user_specs_loadingtargets[i,"TN_or_TP"] == "TP") {
          cat(
            paste0("param loads_lim_P", i, " {seasons, years, limits} >= 0;"),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n",
            append = T
          ) 
        }
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(sm_N_dat)) %do% {
        cat(
          paste0("param transfer_coefficients_N", i, " {comid_N", i, ", seasons, years, transfer_types};"),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(sm_P_dat)) %do% {
        cat(
          paste0("param transfer_coefficients_P", i, " {comid_P", i, ", seasons, years, transfer_types};"),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  ) 
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)
  
  # add param total_load_Ni and total_load_Pi
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(user_specs_loadingtargets$TN_or_TP)) %do% {
        if(user_specs_loadingtargets[i,"TN_or_TP"] == "TN") {
          cat(
            paste0("param total_load_N", i, " {s in seasons, y in years, total_load} >= 0;"),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n",
            append = T
          ) 
        }
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)
  
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(user_specs_loadingtargets$TN_or_TP)) %do% {
        if (user_specs_loadingtargets[i,"TN_or_TP"] == "TP") {
          cat(
            paste0("param total_load_P", i, " {s in seasons, y in years, total_load} >= 0;"),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n",
            append = T
          ) 
        }
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)
  
  # All land use included in urban components
  cat(
    "param agcost_frac >=0;
    
param agBMP_minarea;
param grazBMP_minarea;
param urbanBMP_minarea;
param roadBMP_minarea;

param urban_frac_min {urban_bmp} >=0, <= 1;
param urban_frac_max {u in urban_bmp} >= urban_frac_min[u], <= 1;
param road_frac_min {road_bmp} >=0, <= 1;
param road_frac_max {r in road_bmp} >= road_frac_min[r], <= 1;
param ag_frac_min {ag_bmp} >= 0, <= 1;
param ag_frac_max {a in ag_bmp} >= ag_frac_min[a], <= 1;
param graz_frac_min {graz_bmp} >= 0, <= 1;
param graz_frac_max {g in graz_bmp} >= graz_frac_min[g], <= 1;
param ripbuf_frac_min {ripbuf_bmp} >= 0, <= 1;
param ripbuf_frac_max {b in ripbuf_bmp} >= ripbuf_frac_min[b], <= 1;
  
param ps_coef {c in comid_all, p in point_bmp} := (point_costs_capital[c, p] + 
    point_costs_operations[c, p]);
param urban_coef {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} := urban_area[c,d,n,'urban'] * 
  (urban_costs_capital[c,d,n,u] + urban_costs_operations[c,d,n,u]) ;
param road_coef {c in comid_all, h in road_soil_groups, r in road_bmp} := road_area[c,h,'road'] * 
  (road_costs_capital[c,h,r] + road_costs_operations[c,h,r]) ;
param ag_coef {c in comid_all,  a in ag_bmp} := agcost_frac * area[c,'ag'] * 
  (ag_costs_capital[c,a] + ag_costs_operations[c,a]) ;

param graz_coef {c in comid_all, g in graz_bmp} := agcost_frac * area[c,'graz'] * 
  (graz_costs_capital[c,g] + graz_costs_operations[c,g]) ;
param ripbuf_coef {c in comid_all, b in ripbuf_bmp} := 
  agcost_frac * (ripbuf_costs_capital[c, b] + ripbuf_costs_operations[c, b]);
  
var agBMP_bin {comid_all, ag_bmp} binary;
var grazBMP_bin {comid_all, graz_bmp} binary;
var urbanBMP_bin {comid_all, urban_bmp} binary;
var roadBMP_bin {comid_all, road_bmp} binary;

var point_dec {c in comid_all, p in point_bmp} binary;
# correct unbounded fraction variables - AI solution created NEOS error  
var urban_frac {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}
  >= 0, <= urban_frac_max[u] * urban_bmp_implementationpotential[c,d,n,u];
#  := 0; # default is 0 so not needed; 
#    >= urban_frac_min[u] * urban_bmp_implementationpotential[c,d,n,u],  
#    <= urban_frac_max[u] * urban_bmp_implementationpotential[c,d,n,u],
#    :=0;
var ag_frac {c in comid_all, a in ag_bmp}
   >= 0,
   <= ag_frac_max[a] * ag_bmp_implementationpotential[c,a];
#   := 0
#    >= ag_frac_min[a] * ag_bmp_implementationpotential[c, a],  
#    <= ag_frac_max[a] * ag_bmp_implementationpotential[c, a],
#    :=0;
# ND 7/20/26 Added road_bmp_implementationpotential
var road_frac {c in comid_all, h in road_soil_groups, r in road_bmp}
  >= 0, 
  <= road_frac_max[r] * road_bmp_implementationpotential[c,h,r];
#  := 0;  
#   >= road_frac_min[r],
#   <= road_frac_max[r],
#   :=0;
var graz_frac {c in comid_all, g in graz_bmp}
  >= 0,
  <= graz_frac_max[g];
#  := 0;
#   >= graz_frac_min[g],
#   <= graz_frac_max[g],
#   :=0;
var ripbuf_length {c in comid_all, b in ripbuf_bmp} 
   >= 0, 
   <= unbuffered_banklength[c, b] * ripbuf_frac_max[b];
#   := 0;
   \n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  
#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
# CTC Note: the above script was split into groups, with the script below inserted in between for LIS
  
  # Add septic coefficient only if septic BMPs are enabled
  if(length(Septic_BMPs) > 0) {
    cat(
      "   
# param septic_coef {c in comid_all, e in septic_bmp} :=
# ND substitute
param septic_coef {c in comid_all_N, e in septic_bmp} :=
   septic_costs_capital[c,e] + septic_costs_operations[c,e];",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }

  # Add septic variables only if septic BMPs are enabled
  if(length(Septic_BMPs) > 0) {
    cat(
      "var septic_bin {comid_all_N, septic_bmp} binary;",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
  
  # Add septic fraction variable only if septic BMPs are enabled
  if(length(Septic_BMPs) > 0) {
    cat(
      "var septic_frac {c in comid_all_N, e in septic_bmp} 
   >= 0, 
   <= septic_bmp_implementationpotential[c,e] := 0;",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
  
#### end edits ####
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(user_specs_loadingtargets$TN_or_TP)) %do% {
        if(user_specs_loadingtargets[i,"TN_or_TP"] == "TN") {
          cat(
            paste0("var inc_storage_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;"),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n",
            append = T
          )
        }
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(user_specs_loadingtargets$TN_or_TP)) %do% {
        if(user_specs_loadingtargets[i,"TN_or_TP"] == "TP") {
          cat(
            paste0("var inc_storage_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;"),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n",
            append = T
          )
        }
      }
    }
  )
  
  #ND 6/6/26 Added var no_storage_NPSload_N
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(user_specs_loadingtargets$TN_or_TP)) %do% {
        if(user_specs_loadingtargets[i,"TN_or_TP"] == "TN") {
          cat(
            paste0("var no_storage_NPSload_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;\n
            var reduced_ag_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;\n
            var reduced_graz_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;\n
            var reduced_urb_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;\n
            var reduced_road_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;\n
            subject to road_load_upper {c in comid_N",i,", s in seasons, y in years}:\n
              reduced_road_load_N",i,"[c,s,y] <= baseloads_N", i, "[c,s,y,'road'];\n

subject to road_load_lower {c in comid_N", i, ", s in seasons, y in years}:
    reduced_road_load_N",i,"[c,s,y] >=
        baseloads_N",i,"[c,s,y,'road'] *
        (1 - sum {h in road_soil_groups, r in road_bmp}
              road_effic_N[c,s,y,h,r] * road_frac[c,h,r]);

            
            
            
            var reduced_point_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;\n
            var reduced_septic_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;\n
            var reduced_storage_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;\n
            var riparian_reduction_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;"),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n",
            append = T
          )
        }
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(user_specs_loadingtargets$TN_or_TP)) %do% {
        if(user_specs_loadingtargets[i,"TN_or_TP"] == "TP") {
          cat(
            paste0("var no_storage_NPSload_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;
            var reduced_ag_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;\n
            var reduced_graz_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;\n
            var reduced_urb_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;\n
            var reduced_road_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;\n
            var reduced_point_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;\n
            var reduced_septic_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;\n
            var reduced_storage_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;\n
            var riparian_reduction_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;"),

            
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n",
            append = T
          )
        }
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)

  
  #### end edits ####
  
  ### Create Optimization Equation ------
# Uncomment to restore cost minimization objective
cat(
 "minimize cost: sum {c in comid_all, p in point_bmp} (ps_coef[c,p] * point_dec[c,p]) +
sum {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} (urban_coef[c,d,n,u] * urban_frac[c,d,n,u]) +
sum {c in comid_all, h in road_soil_groups, r in road_bmp} (road_coef[c,h,r] * road_frac[c,h,r]) +
sum {c in comid_all, a in ag_bmp} (ag_coef[c,a] * ag_frac[c,a]) +
sum {c in comid_all, g in graz_bmp} (graz_coef[c,g] * graz_frac[c,g]) +
 \n",
 file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
 sep = "\n",
 append = T
)

  #### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  # Add septic cost component only if septic BMPs are enabled
 if(length(Septic_BMPs) > 0) {
   cat(
     "sum {c in comid_all, b in ripbuf_bmp} (ripbuf_coef[c, b] * ripbuf_length[c, b]) +
     sum {c in comid_all_N, e in septic_bmp} (septic_coef[c,e] * septic_frac[c,e]);",
     file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
     sep = "\n",
     append = T
   )
 }
 if(length(Septic_BMPs) == 0) {
   cat(
     "sum {c in comid_all, b in ripbuf_bmp} (ripbuf_coef[c, b] * ripbuf_length[c, b]);",
     file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
     sep = "\n",
     append = T
   )
 }
# ND 8/3/26 Updated to include TP for lakes
# Comment this out if cost minimization objective above is enabled (uncommented)
#   if ("TN" %in% user_specs_loadingtargets$TN_or_TP){  
# cat(
#     "/* selector parameters for the diagnostic objective */
# param S integer in seasons default 4;
# param Y integer in years   default 7;
# 
# /* temporary objective: minimize the seasonal total load */
# minimize min_total_load2:   sum {c in comid_N1} inc_storage_load_N1[c, 4,7];
#   \n",
#     file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
#     sep = "\n",
#     append = T
#   )
#   }  
#   if ("TP" %in% user_specs_loadingtargets$TN_or_TP){  
#     cat(
#       "/* selector parameters for the diagnostic objective */
# param S integer in seasons default 4;
# param Y integer in years   default 7;
# 
# /* temporary objective: minimize the seasonal total load */
# minimize min_total_load_P:   sum {c in comid_P1} inc_storage_load_P1[c, 4,7];
#   \n",
#       file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
#       sep = "\n",
#       append = T
#     )
#   }    
  ### Incremental Storage Load Equations ---------------------------------------- 

#### CTC edited: 2/5/2026 transferred from dynamic LIS RBEROST ####

  # Function to create septic terms conditionally
  # ND remove duplicative point baseloads and correct term;
  # ND 6/2/26 added point sources back in this term but removed it from code below to prevent
  # double counting for watersheds without septic
  
  create_septic_terms <- function() {
    if(length(Septic_BMPs) > 0) {
      return(paste0(
        "(baseloads_N", "PLACEHOLDER_I", "[c,s,y,'point'] + ((baseloads_N", "PLACEHOLDER_I", "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])) * (1 - (point_effic_N[c,'point'] * point_dec[c,p])) + \n",
        "(((baseloads_N", "PLACEHOLDER_I", "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])) * (1 - sum {p in point_bmp} (point_effic_N[c,p] * point_dec[c,p])) + \n",
        "          (baseloads_N", "PLACEHOLDER_I", "[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N", "PLACEHOLDER_I", "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e])"
      ))
    } else {
      return("baseloads_NPLACEHOLDER_I[c,s,y,'point'] * (1 - sum {p in point_bmp} (point_effic_N[c,p] * point_dec[c,p]))")
    }
  }

#### end edits ####
  
  #### CTC edited: 2/5/2026 ####
  ### Total Load Equations -----
  # ND 6/5/26: First season is okay - storage is not changed from base model because BMPs have not affected it yet
  # Subsequent estimates of storage require CURRENT transfer coefficient multiplied by (prior season NPS loads w/out storage
  # ND 7/7/26 Added intermediate calculations of reduced load components to simplify
  # final equation, reduce sets of conditional equations
  # and create adjusted load component parameters for reporting at end
  
  # TN 

  if(actionable_load == TRUE){ #@HP TN with actionable load = true updated
    invisible(
      if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(param_loads_lim_tn)) %do% {
          
          # ND 6/2/26 Redefined septicpoint_reduction_term to replace septic_reduction_term
          
          # Create conditional septic terms for complex equation
          if(length(Septic_BMPs) > 0) {
            septic_point_term <- paste0("((baseloads_N", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])")
            septicpoint_reduction_term <- paste0("(baseloads_N", i, "[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e])")
          } else {
            septic_point_term <- "0"
            septicpoint_reduction_term <- "0"
            #        septicpoint_reduction_term <- paste0("(baseloads_N", i, "[c,s,y,'point'] * -1 * sum{p in point_bmp}(point_effic_N[c,p] * point_dec[c,p])))")
          }        
          # CTC Note: inserted ", septic_terms, " - \n based on LIS arrangement
          # ND 6/6/26 Added calculation of total NPS load (incl other but no PS,storage)        
          
          cat(
            paste0(
    "subject to calc_red_ag_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
      reduced_ag_load_N", i ,"[c,s,y] =\n
      baseloads_N", i ,"[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a]));\n
  
    subject to calc_red_graz_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
      reduced_graz_load_N", i ,"[c,s,y] =\n
      baseloads_N", i ,"[c,s,y,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_N[c,g] * graz_frac[c,g]));\n
  
    subject to calc_red_urb_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
      reduced_urb_load_N", i ,"[c,s,y] =\n
      baseloads_N", i ,"[c,s,y,'urban'] * (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}(urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u]));\n
      
    subject to road_removal_cap_N",i,"{c in comid_N",i,", s in seasons, y in years}:
    sum {h in road_soil_groups, r in road_bmp}\n
        road_effic_N[c,s,y,h,r] * road_frac[c,h,r] <= 1;\n
  
    subject to calc_red_road_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
      reduced_road_load_N", i ,"[c,s,y] =\n
      baseloads_N", i ,"[c,s,y,'road'] * (1 - sum{h in road_soil_groups, r in road_bmp}(road_effic_N[c,s,y,h,r] * road_frac[c,h,r]));\n 
    
    subject to calc_red_point_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
        reduced_point_load_N", i ,"[c,s,y] = \n 
        (baseloads_N", i ,"[c,s,y,'point'] + ", septic_point_term, ")  * (1 - sum{p in point_bmp}(point_effic_N[c,p] * point_dec[c,p]));\n
          
    subject to calc_red_septic_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
        reduced_septic_load_N", i ,"[c,s,y] =\n", 
              septicpoint_reduction_term,";\n
          
    subject to calc_riparian_reduction_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:
          riparian_reduction_load_N", i ,"[c,s,y] =  
          sum {b in ripbuf_bmp}\n
        ripbuf_length[c,b] * riparianremoval_N",i,"[c,s,y,b];
          #*\n
          # (
          #   reduced_ag_load_N",i,"[c,s,y] / riparianload_N",i,"[c,s,y,'riparian']\n
          #   +\n
          #     (1 - sum {d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}\n
          #      urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u])\n
          #   * (riparianload_N",i,"[c,s,y,'riparian'] - baseloads_N",i,"[c,s,y,'ag'])\n
          #   / riparianload_N",i,"[c,s,y,'riparian']\n
          # );\n
  
      subject to calc_no_storage_NPSload_N",i,"{c in comid_N", i, ", s in seasons, y in years}:\n
          no_storage_NPSload_N",i,"[c,s,y] =
          baseloads_N", i, "[c,s,y,'other'] +
          reduced_ag_load_N", i ,"[c,s,y] + 
          reduced_urb_load_N", i ,"[c,s,y] +   
          reduced_road_load_N", i ,"[c,s,y] +
          reduced_point_load_N", i ,"[c,s,y] +
          reduced_septic_load_N", i ,"[c,s,y] -
          riparian_reduction_load_N", i ,"[c,s,y];\n 
          
      subject to calc_red_storage_load_N",i,"{c in comid_N",i,", s in seasons, y in years}:\n
      reduced_storage_load_N",i,"[c,s,y] =\n
        if transfer_coefficients_N",i,"[c,s,y,'notrib'] = 1 then\n 
          (no_storage_NPSload_N",i,"[c,s,y] * transfer_coefficients_N",i,"[c,s,y,'coeff']) / (1 - transfer_coefficients_N",i,"[c,s,y,'coeff'])\n
        else if transfer_coefficients_N",i,"[c,s,y,'notrib'] = 0 && s = first(seasons) then\n
          if y = first(years) then
            baseloads_N",i,"[c,s,y,'storage']\n
          else if y != first(years) then\n
            (no_storage_NPSload_N",i,"[c,4,y-1] 
            * transfer_coefficients_N",i,"[c,s,y,'coeff']) / (1 - transfer_coefficients_N",i,"[c,s,y,'coeff'])\n
        else if transfer_coefficients_N",i,"[c,s,y,'notrib'] = 0 && s != first(seasons) then\n
          (no_storage_NPSload_N",i,"[c,s-1,y] 
* transfer_coefficients_N",i,"[c,s,y,'coeff']) / (1 - transfer_coefficients_N",i,"[c,s,y,'coeff']);\n
    
    subject to calc_inc_storage_load_N", i, "{c in comid_N", i, ", s in seasons, y in years}:\n
      inc_storage_load_N", i, "[c,s,y] =\n
        reduced_storage_load_N", i ,"[c,s,y] + 
        reduced_ag_load_N", i ,"[c,s,y] + 
        reduced_urb_load_N", i ,"[c,s,y] +   
        reduced_road_load_N", i ,"[c,s,y] +
        reduced_point_load_N", i ,"[c,s,y] +
        reduced_septic_load_N", i ,"[c,s,y] -
        riparian_reduction_load_N", i ,"[c,s,y];\n
    "
            ),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n", 
            append = T   
          ) 
        } 
      } 
    ) 
     #equation version without other load component
print("Basing TN incremental equations only on actionable load components")
  }else{
    print("Basing TN incremental equations on all load components")
  }
  
  if(actionable_load == FALSE){ #@HP TN with actionable load = false updated
    invisible(
      if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(param_loads_lim_tn)) %do% {
          
          # CTC Note: transferred from dynamic LIS RBEROST
          # ND 6/2/26 Redefined septicpoint_reduction_term to replace septic_reduction_term
          
          # Create conditional septic terms for complex equation
          if(length(Septic_BMPs) > 0) {
            septic_point_term <- paste0("((baseloads_N", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])")
            septicpoint_reduction_term <- paste0("(baseloads_N", i, "[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e])")
          } else {
            septic_point_term <- "0"
            septicpoint_reduction_term <- "0"
            #        septicpoint_reduction_term <- paste0("(baseloads_N", i, "[c,s,y,'point'] * -1 * sum{p in point_bmp}(point_effic_N[c,p] * point_dec[c,p]))")
          }
    cat(
      paste0(          
          "subject to calc_red_ag_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
      reduced_ag_load_N", i ,"[c,s,y] =\n
      baseloads_N", i ,"[c,s,y,'ag'] * (1 - sum {a in ag_bmp} (ag_effic_N[c,a] * ag_frac[c,a]));\n
  
    subject to calc_red_graz_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
      reduced_graz_load_N", i ,"[c,s,y] =\n
      baseloads_N", i ,"[c,s,y,'graz'] * (1 - sum {g in graz_bmp} (graz_effic_N[c,g] * graz_frac[c,g]));\n
  
    subject to calc_red_urb_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
      reduced_urb_load_N", i ,"[c,s,y] =\n
      baseloads_N", i ,"[c,s,y,'urban'] * (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} (urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u]));\n
  
    subject to calc_red_road_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
      reduced_road_load_N", i ,"[c,s,y] =\n
      baseloads_N", i ,"[c,s,y,'road'] * (1 - sum{h in road_soil_groups, r in road_bmp} (road_effic_N[c,s,y,h,r] * road_frac[c,h,r]));\n 
    
    subject to calc_red_point_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
        reduced_point_load_N", i ,"[c,s,y] = \n 
        (baseloads_N", i ,"[c,s,y,'point'] + ", septic_point_term, ")  * (1 - sum{p in point_bmp} (point_effic_N[c,p] * point_dec[c,p]));\n
          
    subject to calc_red_septic_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
        reduced_septic_load_N", i ,"[c,s,y] =\n", 
          septicpoint_reduction_term,";\n
          
    subject to calc_riparian_reduction_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:
          riparian_reduction_load_N", i ,"[c,s,y] =  
          sum {b in ripbuf_bmp}\n
        ripbuf_length[c,b] * riparianremoval_N",i,"[c,s,y,b];\n
        # *\n
        #   (
        #     reduced_ag_load_N",i,"[c,s,y] / riparianload_N",i,"[c,s,y,'riparian']\n
        #     +\n
        #       (1 - sum {d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}\n
        #        urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u])\n
        #     * (riparianload_N",i,"[c,s,y,'riparian'] - baseloads_N",i,"[c,s,y,'ag'])\n
        #     / riparianload_N",i,"[c,s,y,'riparian']\n
        #   );\n
  
      subject to calc_no_storage_NPSload_N",i,"{c in comid_N", i, ", s in seasons, y in years}:\n
          no_storage_NPSload_N",i,"[c,s,y] =
          baseloads_N", i, "[c,s,y,'other'] +
          reduced_ag_load_N", i ,"[c,s,y] + 
          reduced_urb_load_N", i ,"[c,s,y] +   
          reduced_road_load_N", i ,"[c,s,y] +
          reduced_septic_load_N", i ,"[c,s,y] -
          riparian_reduction_load_N", i ,"[c,s,y];\n 
          
      subject to calc_red_storage_load_N", i ,"{c in comid_N", i ,", s in seasons, y in years}:\n
        reduced_storage_load_N", i ,"[c,s,y] = \n
          if transfer_coefficients_N", i ,"[c,s,y,'notrib'] == 1\n
          else if s=first(seasons)\n 
            then if y = first(years)\n 
            then {(no_storage_NPSload_N", i ,"[c,s,y] * transfer_coefficients_N", i ,"[c,s,y,'coeff'])/(1-transfer_coefficients_N", i ,"[c,s,y,'coeff'])}\n
            else {baseloads_N", i ,"[c,s,y,'storage']}\n
          else{(no_storage_NPSload_N", i ,"[c,4,y-1] * transfer_coefficients_N", i ,"[c,s,y,'coeff'])}\n
          else {(no_storage_NPSload_N", i ,"[c,s-1,y] * transfer_coefficients_N", i ,"[c,s,y,'coeff'])};\n
    
    subject to calc_inc_storage_load_N", i, "{c in comid_N", i, ", s in seasons, y in years}:\n
      inc_storage_load_N", i, "[c,s,y] =\n
        baseloads_N", i, "[c,s,y,'other'] +
        reduced_storage_load_N", i ,"[c,s,y] + 
        reduced_ag_load_N", i ,"[c,s,y] + 
        reduced_urb_load_N", i ,"[c,s,y] +   
        reduced_road_load_N", i ,"[c,s,y] +
        reduced_point_load_N", i ,"[c,s,y] +
        reduced_septic_load_N", i ,"[c,s,y] -
        riparian_reduction_load_N", i ,"[c,s,y];\n
    "
          ),
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T       
    )
      }
        }
    ) 
  } 
  
  # TP
  
  if(actionable_load == TRUE){ #@HP TP with actionable load = true updated
    invisible(
      if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(param_loads_lim_tp)) %do% {
          
          # ND 6/2/26 Redefined septicpoint_reduction_term to replace septic_reduction_term
          
          # Create conditional septic terms for complex equation
          if(length(Septic_BMPs) > 0) {
            septic_point_term <- paste0("((baseloads_P", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_P[c,'Sewer_convert'])")
            septicpoint_reduction_term <- paste0("(baseloads_P", i, "[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_P", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_P[c,e])")
          } else {
            septic_point_term <- "0"
            septicpoint_reduction_term <- "0"
            #        septicpoint_reduction_term <- paste0("(baseloads_P", i, "[c,s,y,'point'] * -1 * sum{p in point_bmp}(point_effic_P[c,p] * point_dec[c,p])))")
          }        
          # CTC Note: inserted ", septic_terms, " - \n based on LIS arrangement
          # ND 6/6/26 Added calculation of total NPS load (incl other but no PS,storage)        
    cat(
      paste0(
              "subject to calc_red_ag_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
      reduced_ag_load_P", i ,"[c,s,y] =\n
      baseloads_P", i ,"[c,s,y,'ag'] * (1 - (sum {a in ag_bmp} ag_effic_P[c,a] * ag_frac[c,a]));\n
  
    subject to calc_red_graz_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
      reduced_graz_load_P", i ,"[c,s,y] =\n
      baseloads_P", i ,"[c,s,y,'graz'] * (1 - (sum {g in graz_bmp} graz_effic_P[c,g] * graz_frac[c,g]));\n
  
    subject to calc_red_urb_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
      reduced_urb_load_P", i ,"[c,s,y] =\n
      baseloads_P", i ,"[c,s,y,'urban'] * (1 - (sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} urban_effic_P[c,s,y,d,n,u] * urban_frac[c,d,n,u]));\n
      
    subject to road_removal_cap_P",i,"{c in comid_P",i,", s in seasons, y in years}:
      sum {h in road_soil_groups, r in road_bmp}
        road_effic_P[c,s,y,h,r] * road_frac[c,h,r] <= 1;\n
  
    subject to calc_red_road_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
      reduced_road_load_P", i ,"[c,s,y] =\n
      baseloads_P", i ,"[c,s,y,'road'] * (1 - (sum{h in road_soil_groups, r in road_bmp} road_effic_P[c,s,y,h,r] * road_frac[c,h,r]));\n 
    
    subject to calc_red_point_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
        reduced_point_load_P", i ,"[c,s,y] = \n 
        (baseloads_P", i ,"[c,s,y,'point'] + ", septic_point_term, ")  * (1 - (sum{p in point_bmp} point_effic_P[c,p] * point_dec[c,p]));\n
          
    subject to calc_red_septic_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
        reduced_septic_load_P", i ,"[c,s,y] =\n", 
              septicpoint_reduction_term,";\n
          
    subject to calc_riparian_reduction_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:
          riparian_reduction_load_P", i ,"[c,s,y] =  
          sum {b in ripbuf_bmp} ripbuf_length[c,b] * riparianremoval_P",i,"[c,s,y,b];\n
        # *\n
        #   (
        #     reduced_ag_load_P",i,"[c,s,y] / riparianload_P",i,"[c,s,y,'riparian']\n
        #     +\n
        #       (1 - sum {d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}\n
        #        urban_effic_P[c,s,y,d,n,u] * urban_frac[c,d,n,u])\n
        #     * (riparianload_P",i,"[c,s,y,'riparian'] - baseloads_P",i,"[c,s,y,'ag'])\n
        #     / riparianload_P",i,"[c,s,y,'riparian']\n
        #   );\n
  
      subject to calc_no_storage_NPSload_P",i,"{c in comid_P", i, ", s in seasons, y in years}:\n
          no_storage_NPSload_P",i,"[c,s,y] =
          baseloads_P", i, "[c,s,y,'other'] +
          reduced_ag_load_P", i ,"[c,s,y] + 
          reduced_urb_load_P", i ,"[c,s,y] +   
          reduced_road_load_P", i ,"[c,s,y] +
          reduced_septic_load_P", i ,"[c,s,y] -
          riparian_reduction_load_P", i ,"[c,s,y];\n 
          
      subject to calc_red_storage_load_P",i,"{c in comid_P",i,", s in seasons, y in years}:\n
      reduced_storage_load_P",i,"[c,s,y] =\n
        if transfer_coefficients_P",i,"[c,s,y,'notrib'] = 1 then\n 
          (no_storage_NPSload_P",i,"[c,s,y] * transfer_coefficients_P",i,"[c,s,y,'coeff']) / (1 - transfer_coefficients_P",i,"[c,s,y,'coeff'])\n
        else if transfer_coefficients_P",i,"[c,s,y,'notrib'] = 0 and s = first(seasons) then\n
          if y = first(years) then
            baseloads_P",i,"[c,s,y,'storage']\n
          else\n
            (no_storage_NPSload_P",i,"[c,4,y-1] + reduced_storage_load_P",i,"[c,4,y-1]) 
            * transfer_coefficients_P",i,"[c,s,y,'coeff']\n
        else 
          (no_storage_NPSload_P",i,"[c,s-1,y] + reduced_storage_load_P",i,"[c,s-1,y])
              * transfer_coefficients_P",i,"[c,s,y,'coeff'];\n
    
    subject to calc_inc_storage_load_P", i, "{c in comid_P", i, ", s in seasons, y in years}:\n
      inc_storage_load_P", i, "[c,s,y] =\n
        reduced_storage_load_P", i ,"[c,s,y] + 
        reduced_ag_load_P", i ,"[c,s,y] + 
        reduced_urb_load_P", i ,"[c,s,y] +   
        reduced_road_load_P", i ,"[c,s,y] +
        reduced_point_load_P", i ,"[c,s,y] +
        reduced_septic_load_P", i ,"[c,s,y] -
        riparian_reduction_load_P", i ,"[c,s,y];\n
    "
            ),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n", 
            append = T   
    ) 
        }
      } 
    ) 
    #equation version without other load component
print("Basing TP incremental equations only on actionable load components")
  }else{
    print("Basing TP incremental equations on all load components")
  }
  
  if(actionable_load == FALSE){ #@HP TP with actionable load = false updated
    invisible(
      if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(param_loads_lim_tp)) %do% {
          
          # CTC Note: transferred from dynamic LIS RBEROST
          # ND 6/2/26 Redefined septicpoint_reduction_term to replace septic_reduction_term
          
          # Create conditional septic terms for complex equation
          if(length(Septic_BMPs) > 0) {
            septic_point_term <- paste0("((baseloads_P", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_P[c,'Sewer_convert'])")
            septicpoint_reduction_term <- paste0("(baseloads_P", i, "[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_P", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_P[c,e])")
          } else {
            septic_point_term <- "0"
            septicpoint_reduction_term <- "0"
            #        septicpoint_reduction_term <- paste0("(baseloads_P", i, "[c,s,y,'point'] * -1 * sum{p in point_bmp}(point_effic_P[c,p] * point_dec[c,p]))")
          }
          
          cat(
            paste0(
          "subject to calc_red_ag_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
      reduced_ag_load_P", i ,"[c,s,y] =\n
      baseloads_P", i ,"[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a]));\n
  
    subject to calc_red_graz_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
      reduced_graz_load_P", i ,"[c,s,y] =\n
      baseloads_P", i ,"[c,s,y,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_P[c,g] * graz_frac[c,g]));\n
  
    subject to calc_red_urb_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
      reduced_urb_load_P", i ,"[c,s,y] =\n
      baseloads_P", i ,"[c,s,y,'urban'] * (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}(urban_effic_P[c,s,y,d,n,u] * urban_frac[c,d,n,u]));\n
  
    subject to calc_red_road_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
      reduced_road_load_P", i ,"[c,s,y] =\n
      baseloads_P", i ,"[c,s,y,'road'] * (1 - sum{h in road_soil_groups, r in road_bmp}(road_effic_P[c,s,y,h,r] * road_frac[c,h,r]));\n 
    
    subject to calc_red_point_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
        reduced_point_load_P", i ,"[c,s,y] = \n 
        (baseloads_P", i ,"[c,s,y,'point'] + ", septic_point_term, ")  * (1 - sum{p in point_bmp}(point_effic_P[c,p] * point_dec[c,p]));\n
          
    subject to calc_red_septic_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
        reduced_septic_load_P", i ,"[c,s,y] =\n", 
          septicpoint_reduction_term,";\n
          
    subject to calc_riparian_reduction_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:
          riparian_reduction_load_P", i ,"[c,s,y] =  
          sum {b in ripbuf_bmp}\n
        ripbuf_length[c,b] * riparianremoval_P",i,"[c,s,y,b] *\n
          (
            reduced_ag_load_P",i,"[c,s,y] / riparianload_P",i,"[c,s,y,'riparian']\n
            +\n
              (1 - sum {d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}\n
               urban_effic_P[c,s,y,d,n,u] * urban_frac[c,d,n,u])\n
            * (riparianload_P",i,"[c,s,y,'riparian'] - baseloads_P",i,"[c,s,y,'ag'])\n
            / riparianload_P",i,"[c,s,y,'riparian']\n
          );\n
  
      subject to calc_no_storage_NPSload_P",i,"{c in comid_P", i, ", s in seasons, y in years}:\n
          no_storage_NPSload_P",i,"[c,s,y] =
          baseloads_P", i, "[c,s,y,'other'] +
          reduced_ag_load_P", i ,"[c,s,y] + 
          reduced_urb_load_P", i ,"[c,s,y] +   
          reduced_road_load_P", i ,"[c,s,y] +
          reduced_point_load_P", i ,"[c,s,y] +
          reduced_septic_load_P", i ,"[c,s,y] -
          riparian_reduction_load_P", i ,"[c,s,y];\n 
          
      subject to calc_red_storage_load_P", i ,"{c in comid_P", i ,", s in seasons, y in years}:\n
        reduced_storage_load_P", i ,"[c,s,y] = \n
          if transfer_coefficients_P", i ,"[c,s,y,'notrib'] == 1\n
          else if s=first(seasons)\n 
            then if y = first(years)\n 
            then {(no_storage_NPSload_P", i ,"[c,s,y] * transfer_coefficients_P", i ,"[c,s,y,'coeff'])/(1-transfer_coefficients_P", i ,"[c,s,y,'coeff'])}\n
            else {baseloads_P", i ,"[c,s,y,'storage']}\n
          else{(no_storage_NPSload_P", i ,"[c,4,y-1] * transfer_coefficients_P", i ,"[c,s,y,'coeff'])}\n
          else {(no_storage_NPSload_P", i ,"[c,s-1,y] * transfer_coefficients_P", i ,"[c,s,y,'coeff'])};\n
    
    subject to calc_inc_storage_load_P", i, "{c in comid_P", i, ", s in seasons, y in years}:\n
      inc_storage_load_P", i, "[c,s,y] =\n
        baseloads_P", i, "[c,s,y,'other'] +
        reduced_storage_load_P", i ,"[c,s,y] + 
        reduced_ag_load_P", i ,"[c,s,y] + 
        reduced_urb_load_P", i ,"[c,s,y] +   
        reduced_road_load_P", i ,"[c,s,y] +
        reduced_point_load_P", i ,"[c,s,y] +
        reduced_septic_load_P", i ,"[c,s,y] -
        riparian_reduction_load_P", i ,"[c,s,y];\n
    "
          ),
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T       
    )
}
  }
) 
}
  
  invisible(if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      if (nrow(param_loads_lim_tn[[i]]) > 0) {
        cat(
          paste0(
            "subject to total_loads_N",
            i,
            "{s in seasons, y in years}:\n",
            "sum{c in comid_N",
            i,
            "} (inc_storage_load_N",
            i,
            "[c,s,y])",

           " <= loads_lim_N",
           i,
           "[s,y,'limits'];\n"
#            " >= 0; \n"
          ),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  })


  invisible(if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      if (nrow(param_loads_lim_tp[[i]]) > 0) {
        cat(
          paste0(
            "subject to total_loads_P",
            i,
            "{s in seasons, y in years}:\n",
            "sum{c in comid_P",
            i,
            "} (inc_storage_load_P",
            i,
            "[c,s,y])",
            " <= loads_lim_P",
            i,
            "[s,y,'limits'];\n"
          ),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  })

  
  ### Riparian Load Equations -----
  # ND 6/19/26: limit removal to riparian load remaining after upland ag and urban bmp reductions 
  invisible(
    if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        cat(
          paste0(
            "subject to riparian_loads_N", i, " {c in comid_N", i, ", s in seasons, y in years}:\n",
            "sum {b in ripbuf_bmp} ripbuf_length[c, b] * riparianremoval_N", i,"[c,s,y,b] <= \n 
            (baseloads_N", i,"[c,s,y,'ag'] \n 
            * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) \n
            + (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} \n
              (urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u]))\n 
            * (riparianload_N",i,"[c,s,y,'riparian'] - baseloads_N", i,"[c,s,y,'ag']));\n"),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  )
 
  
# ND 6/19/26: limit removal to riparian load adjusted by upland ag bmp effects  
  invisible(
    if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tp)) %do% {
        cat(
          paste0(
            "subject to riparian_loads_P", i, " {c in comid_P", i, ", s in seasons, y in years}:\n",
            "sum {b in ripbuf_bmp} ripbuf_length[c, b] * riparianremoval_P", i,"[c,s,y,b] <= \n 
            (baseloads_P", i,"[c,s,y,'ag'] \n 
            * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) \n
            + (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} \n
              (urban_effic_P[c,s,y,d,n,u] * urban_frac[c,d,n,u]))\n 
            * (riparianload_P",i,"[c,s,y,'riparian'] - baseloads_P", i,"[c,s,y,'ag']));\n"),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  )
  
  

  cat(
    "subject to urban_frac_min2 {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}:
urban_frac[c,d,n,u] >= urban_frac_min[u] * urban_bmp_implementationpotential[c,d,n,u] * urbanBMP_bin[c,u];
    subject to ag_frac_min2 {c in comid_all, a in ag_bmp}:
ag_frac[c,a] >= ag_frac_min[a] * ag_bmp_implementationpotential[c,a] * agBMP_bin[c,a];
    subject to graz_frac_min2{c in comid_all,g in graz_bmp}:
graz_frac[c,g] >= graz_frac_min[g] * grazBMP_bin[c,g];

#    subject to road_frac_min2{c in comid_all, h in road_soil_groups,r in road_bmp}:
# road_frac[c,h,r] >= road_frac_min[r] * roadBMP_bin[c,r];

# ND replacement
  subject to road_frac_min2 {c in comid_all, r in road_bmp}:
sum {h in road_soil_groups} road_frac[c,h,r] >= road_frac_min[r] * roadBMP_bin[c,r];

# ND added Aggregate min area per BMP (recommended) 
  subject to road_treat_min {c in comid_all, r in road_bmp}:
sum {h in road_soil_groups} road_frac[c,h,r] * road_area[c,h,'road'] >= roadBMP_minarea * roadBMP_bin[c,r];
    
subject to ag_treat_min {c in comid_all, a in ag_bmp}:
ag_frac[c,a] * area[c,'ag'] >= agBMP_minarea * agBMP_bin[c,a];

subject to graz_treat_min {c in comid_all, g in graz_bmp}:
graz_frac[c,g] * area[c,'graz'] >= grazBMP_minarea * grazBMP_bin[c,g];

subject to urban_treat_min {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}:
urban_frac[c,d,n,u] * urban_area[c,d,n,'urban'] >= urbanBMP_minarea * urbanBMP_bin[c,u];
  
subject to ag_frac_const {c in comid_all, a in ag_bmp}:
ag_frac[c,a] <= agBMP_bin[c,a];

subject to graz_frac_const {c in comid_all, g in graz_bmp}:
graz_frac[c,g] <= grazBMP_bin[c,g];
  
subject to urban_frac_const {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}:
urban_frac[c,d,n,u] <= urbanBMP_bin[c,u];

#ND added missing 6/3/26;
subject to road_frac_const {c in comid_all, h in road_soil_groups, r in road_bmp}:
    road_frac[c,h,r] <= roadBMP_bin[c,r];

subject to ag_frac_limit {c in comid_all}: 
sum {a in ag_bmp} ag_frac[c,a] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to graz_frac_limit {c in comid_all}: 
sum {g in graz_bmp} graz_frac[c,g] <= 1;
# prevents multiple BMPs being implemented on the same area

#subject to urban_frac_limit {c in comid_all}: 
#sum {u in urban_bmp, d in urban_land_uses, n in urban_soil_groups} urban_frac[c,d,n,u] <= 1;
# prevents multiple BMPs being implemented on the same area

# ND substitute - drop
# Per-subarea (d,n) constraint
 subject to urban_frac_limit2 {c in comid_all, d in urban_land_uses, n in urban_soil_groups}:
  sum {u in urban_bmp} urban_frac[c,d,n,u] <= 1;

subject to road_frac_limit {c in comid_all}: 
sum {r in road_bmp, h in road_soil_groups} road_frac[c,h,r] <= 1;
# prevents multiple BMPs being implemented on the same area

# ND substitute - try dropping this because not updated during bootstraps
# Per-soil-group constraint
# subject to road_frac_limit2 {c in comid_all, h in road_soil_groups}:
#  sum {r in road_bmp} road_frac[c,h,r] <= 1;
# prevents multiple BMPs being implemented on the same sub-area

subject to point_dec_limit {c in comid_all}: 
sum {p in point_bmp} point_dec[c,p] <= 1;
# limits each WWTP to one upgrade

subject to total_banks {c in comid_all}:
sum {b in ripbuf_bmp} ripbuf_length[c,b] <=  
   unbuffered_banklength[c,'Forested_Buffer']; #ND 7/8/2026 changed from max
  
",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  
  if(!grepl("''", paste0(pervbmp_urban_vec_comma, collapse = ""))){
    cat(
      "
subject to roads_and_parkinglots {c in comid_all}:
sum{up in urban_pervbmp, d in urban_land_uses, n in urban_soil_groups} urban_frac[c,d,n,up] <= max {up in urban_pervbmp, d in urban_land_uses, n in urban_soil_groups} 
   urban_bmp_implementationpotential[c,d,n,up];
",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n", 
      append = T
    )
  }
  
  if(!grepl("''", paste0(tilebmp_ag_vec_comma, collapse = ""))){
    cat(
      "
subject to tiledrains {c in comid_all}:
sum{agti in ag_tilebmp} ag_frac[c, agti] <= max {agti in ag_tilebmp}
   ag_bmp_implementationpotential[c,agti];
",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
  
  
  print(
    paste(
      "RBEROST has finished writing AMPL scripts without uncertainty at",
      Sys.time()
    )
  )

#### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  
  ### Septic Load Reduction Constraints
  if(length(Septic_BMPs) > 0) {
    invisible(
      if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(param_loads_lim_tn)) %do% {
          cat(
            paste0(
              "subject to septic_reduction_limit_N", i, " {c in comid_N", i, ", s in seasons, y in years}:\n",
              "sum {e in septic_bmp} (baseloads_N", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e] <= baseloads_N", i, "[c,s,y,'septic'];\n"
            ),
            file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
            sep = "\n", 
            append = T
          )
        }
      }
    )
  }
  
#### end edits ####
  ### I. Develop command file ------ ND Moved to after section III to add missing def
# ND 7/3/26 Added to avoid appending to old .amp file
  write( "# Tier 1 Command AMPL File", file = paste(OutPath,"STcommand_seasonal.amp",sep = ""))
  cat(
    "#WMOST Optimization Screening Tool AMPL command file
option presolve_warnings 10000;
# Enable AMPL automatic scaling
option scale_option 1;  # Enable automatic scaling for unit scaling sensitivity
option scale_tolerance 1e-12;  # Relaxed from baseline 1e-12
# option cplex_options 'feasibility 1e-1'; # Relaxed from baseline 1e-6
option data_warn 0;
# option cplex_options 'absmipgap 1.0';
# explore infeasibility
# option cplex_options 'conflict';
# option cplex_options 'emphasis numerical 1';
solve;

param urban_frac_by_comid_bmp {c in comid_all, u in urban_bmp};

# Now compute derived reporting parameters, using the current values of variables
# ND 7/21/26 Added if then to cover zero denominator
for {c in comid_all, u in urban_bmp} {
  let urban_frac_by_comid_bmp[c,u] :=
    if sum {d in urban_land_uses, n in urban_soil_groups} urban_area[c,d,n,'urban'] > 0 then  
    (sum {d in urban_land_uses, n in urban_soil_groups}
       urban_frac[c,d,n,u] * urban_area[c,d,n,'urban']) /
    (sum {d in urban_land_uses, n in urban_soil_groups}
       urban_area[c,d,n,'urban'] + 1e-10)
    else 0;
}

display urban_frac_by_comid_bmp;

param road_frac_by_comid_bmp {c in comid_all, r in road_bmp};

# ND 8/6/26 Moved if statement after let
for {c in comid_all, r in road_bmp} {
  let road_frac_by_comid_bmp[c,r] :=
   if sum {h in road_soil_groups} road_area[c,h,'road'] > 0 then  
    (sum {h in road_soil_groups}
       road_frac[c,h,r] * road_area[c,h,'road']) /
    (sum {h in road_soil_groups}
       road_area[c,h,'road'] + 1e-10)
  else 0;
}
# ND 8/10/26 fixed sep
display road_frac_by_comid_bmp;",
           file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
       append = TRUE
     )

# Declarations (as you already had)
   if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
     cat(
"param reduced_urban_load {c in comid_all, s in seasons, y in years};\n

let {c in comid_all, s in seasons, y in years} reduced_urban_load[c,s,y] :=
    baseloads_N1[c,s,y,'urban']
    * max(0, 1
            - sum {d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}
                (urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u]));

display reduced_urban_load;

param reduced_road_load  {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_road_load[c,s,y] :=
    baseloads_N1[c,s,y,'road']
    * max(0, 1
            - sum {h in road_soil_groups, r in road_bmp}
                (road_effic_N[c,s,y,h,r] * road_frac[c,h,r]));

display reduced_road_load;

param reduced_ag_load    {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_ag_load[c,s,y] :=
    baseloads_N1[c,s,y,'ag']
    * max(0, 1
            - sum {a in ag_bmp} (ag_effic_N[c,a] * ag_frac[c,a]));

display reduced_ag_load;

param reduced_graz_load  {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_graz_load[c,s,y] :=
    baseloads_N1[c,s,y,'graz']
    * max(0, 1
            - sum {g in graz_bmp} (graz_effic_N[c,g] * graz_frac[c,g]));

display reduced_graz_load;

param reduced_storage_load {c in comid_all, s in seasons, y in years};

let {c in comid_all} reduced_storage_load[c, 2, 7] := no_storage_NPSload_N1[c, 1, 7] * transfer_coefficients_N1[c, 2, 7, 'coeff'];

display {c in comid_all} reduced_storage_load[c, 2, 7];


let {c in comid_all} reduced_storage_load[c, 3, 7] := no_storage_NPSload_N1[c, 2, 7] * transfer_coefficients_N1[c, 3, 7, 'coeff'];

display {c in comid_all} reduced_storage_load[c, 3, 7];

param reduced_other_load {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_other_load[c,s,y] :=
    baseloads_N1[c,s,y,'other'];

display reduced_other_load;

param riparian_load_reduction {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} riparian_load_reduction[c,s,y] :=
    if riparianload_N1[c,s,y,'riparian'] > 0 then
    sum {b in ripbuf_bmp}
	ripbuf_length[c,b] * riparianremoval_N1[c,s,y,b] * ((1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a]) * 	baseloads_N1[c,s,y,'ag']/riparianload_N1[c,s,y,'riparian']) + 
        (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}(urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u]) * 
        (riparianload_N1[c,s,y,'riparian'] - baseloads_N1[c,s,y,'ag'])/riparianload_N1[c,s,y,'riparian']))
        else 0;
        
display riparian_load_reduction;",
           file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
       append = TRUE
     )
   }

  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "param reduced_urban_load {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_urban_load[c,s,y] :=
    baseloads_P1[c,s,y,'urban']
    * max(0, 1
            - sum {d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}
                (urban_effic_P[c,s,y,d,n,u] * urban_frac[c,d,n,u]));

display reduced_urban_load;

param reduced_road_load  {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_road_load[c,s,y] :=
    baseloads_P1[c,s,y,'road']
    * max(0, 1
            - sum {h in road_soil_groups, r in road_bmp}
                (road_effic_P[c,s,y,h,r] * road_frac[c,h,r]));

display reduced_road_load;

param reduced_ag_load    {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_ag_load[c,s,y] :=
    baseloads_P1[c,s,y,'ag']
    * max(0, 1
            - sum {a in ag_bmp} (ag_effic_P[c,a] * ag_frac[c,a]));

display reduced_ag_load;

param reduced_graz_load  {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_graz_load[c,s,y] :=
    baseloads_P1[c,s,y,'graz']
    * max(0, 1
            - sum {g in graz_bmp} (graz_effic_P[c,g] * graz_frac[c,g]));

display reduced_graz_load;

param reduced_storage_load {c in comid_all, s in seasons, y in years};

let {c in comid_all} reduced_storage_load[c, 2, 7] := no_storage_NPSload_P1[c, 1, 7] * transfer_coefficients_P1[c, 2, 7, 'coeff'];

display {c in comid_all} reduced_storage_load[c, 2, 7];


let {c in comid_all} reduced_storage_load[c, 3, 7] := no_storage_NPSload_P1[c, 2, 7] * transfer_coefficients_P1[c, 3, 7, 'coeff'];

display {c in comid_all} reduced_storage_load[c, 3, 7];

param reduced_other_load {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} reduced_other_load[c,s,y] :=
    baseloads_P1[c,s,y,'other'];

display reduced_other_load;

param riparian_load_reduction {c in comid_all, s in seasons, y in years};

let {c in comid_all, s in seasons, y in years} riparian_load_reduction[c,s,y] :=
    sum {b in ripbuf_bmp}
	ripbuf_length[c,b] * riparianremoval_P1[c,s,y,b] * ((1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a]) * 	baseloads_P1[c,s,y,'ag']/riparianload_P1[c,s,y,'riparian']) + 
        (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}(urban_effic_P[c,s,y,d,n,u] * urban_frac[c,d,n,u]) * 
        (riparianload_P1[c,s,y,'riparian'] - baseloads_P1[c,s,y,'ag'])/riparianload_P1[c,s,y,'riparian']));

display riparian_load_reduction;",
      file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
      append = TRUE
    )
  }
  
  cat(
"option display_precision 10;
option presolve_eps 1.23e-14;
display solve_result_num, solve_result;
display cost.result;
display cost;
option display_1col 10000000000;
option display_width 100000000000;
option display_transpose -10000;
option omit_zero_rows 1;
option omit_zero_cols 1;
option display_1col 0;
display point_dec;
display urban_frac;
display road_frac;
display urban_frac_by_comid_bmp;
display road_frac_by_comid_bmp;
display ag_frac;
display graz_frac;
display ripbuf_length;
;\n",
file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
append = TRUE)  
#sep = "\n"
 
  
  #### CTC edited: 2/5/2026 - transferred from dynamic LIS RBEROST ####
  # ND  edited to include P results 
  # If septic data is available, add septic-specific commands
  if(length(Septic_BMPs) > 0 & septic_data_available & "TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "display septic_frac;\n",
      file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
      append = TRUE
    )
  }
  if(length(Septic_BMPs) > 0 & septic_data_available & "TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "param reduced_septic_load_N1{c in comid_all, s in seasons, y in years};\n
    
    let {c in comid_all, s in seasons, y in years} reduced_septic_load_N1[c,s,y] := \n
      ((baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * 	septic_effic_N[c,'Sewer_convert']) * (1 - sum{p in point_bmp}(point_effic_N[c,p] * point_dec[c,p])) + 
      (baseloads_N1[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N1[c,s,y,'septic']/
                                                               septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e]);
    display reduced_septic_load_N1;",
    file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
    append = TRUE
  )
  }
  if(length(Septic_BMPs) > 0 & septic_data_available & "TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "param reduced_septic_load_P1{c in comid_all, s in seasons, y in years};\n
    
    let {c in comid_all, s in seasons, y in years} reduced_septic_load_P1 [c,s,y] := \n
      ((baseloads_P1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * 	septic_effic_P[c,'Sewer_convert']) * (1 - sum{p in point_bmp}(point_effic_N[c,p] * point_dec[c,p])) + 
      (baseloads_P1[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_P1[c,s,y,'septic']/
                                                               septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_P[c,e]);
    display reduced_septic_load_P1;",
      file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
      append = TRUE
    )
}
#   # ND 8/3/26 Added display of min_total_load_P if P included in analysis;
#   if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
#     cat(
#       "display min_total_load_P;\n",
#       file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
#       append = TRUE
#     )
#   }
#   
#   #### end edits ####
# 
# # ND 8/3/26 Add display of N load components only if N included in analysis
#   if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
#     cat(   
#     "display min_total_load2\n;
#     display reduced_ag_load_N1;
#     display reduced_urb_load_N1;   
#     display reduced_road_load_N1;
#     display riparian_reduction_load_N1;",
#           file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
#       append = TRUE
#     )
#   }