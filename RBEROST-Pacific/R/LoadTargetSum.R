# LoadTargetSum.R
# This extracts cumulative loads from subwatershed outlets both total and by source for comparison with targets
# N. Detenbeck
# 11/22/25

library(readr)

# Import loading targets
UserSpecs_loadingtargets <- read_csv("RBEROST-Pacific/Preprocessing/Inputs/01_UserSpecs_loadingtargets_orig.csv")

# Import terminal COMIDs for subwatersheds
Preprocessing_Terminal_COMID <- read_csv("RBEROST-Pacific/Preprocessing/Inputs/01_Preprocessing_Terminal_COMID.csv")

# Import Puget Sound SPARROW total loads and source loads at subwatershed mouths
predict_puget_tn <- read_csv("RBEROST-Pacific/Preprocessing/Inputs/predict_puget_tn.csv")
predict_puget_tp <- read_csv("RBEROST-Pacific/Preprocessing/Inputs/predict_puget_tp.csv")

# Filter SPARROW loads for subwatershed outlets
predict_puget_tn_outlets <- Preprocessing_Terminal_COMID %>%
  left_join(predict_puget_tn,by = 'comid')
predict_puget_tp_outlets <- Preprocessing_Terminal_COMID %>%
  left_join(predict_puget_tp,by = 'comid')

# sum loads by subwatershed
library(data.table)
setDT(predict_puget_tn_outlets) # Convert data frame to data.table by reference

result_datatable_tn <- predict_puget_tn_outlets[, lapply(.SD, sum, na.rm = TRUE), by = .(watershed_name, year), 
                        .SDcols = c("PLOAD_TOTAL", "PLOAD_FOREIGN", "PLOAD_POINT1_KG","PLOAD_CAFOS","PLOAD_SEPTIC",
                                    "PLOAD_AG_KG", "PLOAD_ATM_KG","PLOAD_URB_KM2","PLOAD_NFIX_M2","PLOAD_STORAGE")]
                                                                                                                         )]
setDT(predict_puget_tp_outlets) # Convert data frame to data.table by reference

result_datatable_tp <- predict_puget_tp_outlets[, lapply(.SD, sum, na.rm = TRUE), by = .(watershed_name, year), 
                                             .SDcols = c("PLOAD_TOTAL", "PLOAD_FOREIGN", "PLOAD_POINT1_KG","PLOAD_NATP_MT","PLOAD_CAFOS","PLOAD_SEPTIC",
                                                         "PLOAD_AG_KG","PLOAD_URB_KM2","PLOAD_STORAGE")]
result_datatable_tn <- result_datatable_tn %>% 
  mutate(PLOAD_OTHER = PLOAD_FOREIGN + PLOAD_SEPTIC + PLOAD_ATM_KG + PLOAD_NFIX_M2 + PLOAD_STORAGE) %>% 
  mutate(PLOAD_treatable = PLOAD_TOTAL - PLOAD_OTHER)

result_datatable_tp <- result_datatable_tp %>% 
  mutate(PLOAD_OTHER = PLOAD_FOREIGN + PLOAD_SEPTIC + PLOAD_NATP_MT + PLOAD_STORAGE) %>% 
  mutate(PLOAD_treatable = PLOAD_TOTAL - PLOAD_OTHER)

# join with target loads for comparison
result_datatable_tn_wtarget <- result_datatable_tn %>%
  left_join(UserSpecs_loadingtargets,by = "watershed_name")  %>%
  mutate(TN_annualtargetall = PLOAD_TOTAL * (1-TN_perc_reduction_target))  %>%
  mutate(TN_annualtargettreatable = (PLOAD_treatable * (1-TN_perc_reduction_target)) + PLOAD_OTHER) 

result_datatable_tp_wtarget <- result_datatable_tp %>%
  left_join(UserSpecs_loadingtargets,by = "watershed_name")  %>%
  mutate(TP_annualtargetall = adjusted_annual_target_kgperyr)  %>%
  mutate(TP_annualtargettreatable = adjusted_annual_target_kgperyr + PLOAD_OTHER) 
  # NEED TO CHECK ICF CODE TO CONFIRM HOW THIS IS HANDLED#

# add test of whether load target is already met (target > total load)
# add test of whether load target can not be met (target < other load)
result_datatable_tn_wtarget <- result_datatable_tn_wtarget %>%
  mutate(origtargetmet = (TN_annualtargetall > PLOAD_TOTAL)) %>%
  mutate(treatabletargetmet = (TN_annualtargettreatable > PLOAD_TOTAL)) %>%
  mutate(origtargetcantmeet = (TN_annualtargetall < PLOAD_OTHER))  %>%
  mutate(adjtargetcantmeet = (TN_annualtargettreatable < PLOAD_OTHER))

# check to see if storage is on the order of 25-29%
result_datatable_tn_wtarget <- result_datatable_tn_wtarget %>%
  mutate(frSTORAGELOAD = PLOAD_STORAGE/PLOAD_TOTAL)

# add test of whether load target is already met (target > total load)
# add test of whether load target can not be met (target < other load)
result_datatable_tp_wtarget <- result_datatable_tp_wtarget %>%
  mutate(origtargetmet = (TP_annualtargetall > PLOAD_TOTAL)) %>%
  mutate(treatabletargetmet = (TP_annualtargettreatable > PLOAD_TOTAL)) %>%
  mutate(origtargetcantmeet = (TP_annualtargetall < PLOAD_OTHER))  %>%
  mutate(adjtargetcantmeet = (TP_annualtargettreatable < PLOAD_OTHER))

### check to make sure that point sources and foreign loads are not being included in storage component

# There are no cases where the original target was met before bmps could be applied.
# There are no cases where the adjusted target accounting for treatable loads only was met before bmps could
# be applied
  
