###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Calculate updated cost and efficiency data ####
# BY: Kelly-Anne Moffa
# DATE:  12/04/2023 ####
# UPDATED: 05/26/26
# BY: Naomi Detenbeck
# Simplified interpretation of prediction equation results
# Cost calculations based on long-term precipitation and runoff not seasonal values
# consistent with data used in regressions (costs don't vary by seasonxyear)
# Increased threshold for cutting off extreme costs/efficiency
# Updated cost basis to 2025USD
###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~

## This code now gets sourced through the 01_Preprocessing Code

# 1. Setup -----

packages <- c(
  "sf", "sp", "dplyr", "data.table", "ggplot2", "purrr", "openxlsx", "stringr", "tidyr", "foreign"
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

sf::sf_use_s2(FALSE)

### User defined list of BMPs -----
user_specs_BMPs <- fread(
  paste(InPath,"01_UserSpecs_BMPs_wseptic.csv",sep=""), data.table = FALSE 
) #*# Updated to $2026 for ag bmps, urban, wastewater and septic costs updated below to $2025

#Pull out urban BMPs and make into list to filter future datasets by
urban_BMP <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "urban")

relevant_BMP <- urban_BMP %>%
  as_tibble() %>%
  dplyr::select(BMP) %>%
  pull() %>%
  unique()

# 2. Load Data -----


### Read in HRU files with cost coefficients -----
all_HRUs_withcosts <- read_csv(paste0(InPath, "allHRUs_withcosts.csv")) ##now contains min costs, ag in $2026

bmp_count <- all_HRUs_withcosts %>%
  group_by(BMP.Type) %>%
  tally()

### Read in efficiencies -----

## NEW EFFIC DATA
n_effic_tmp <- read.xlsx("./Data/Bmpnefficsumyrxssnxlu.xls.xlsx", startRow = 3)
p_effic_tmp <- read.xlsx("./Data/Bmppefficsumyrxssnxlu.xls.xlsx", startRow = 3)

#### Update BMP names to only have underscores, no spaces or special characters
n_effic_tmp$BMP <- gsub(" |/", "_", n_effic_tmp$BMP)
p_effic_tmp$BMP <- gsub(" |/", "_", p_effic_tmp$BMP)

effic_tmp <- n_effic_tmp %>%
  select(-c(`_FREQ_`, `_TYPE_`)) %>%
  merge(., p_effic_tmp) %>%
  select(-c(`_FREQ_`, `_TYPE_`))

effic_19_20 <- effic_tmp %>%
  filter(year %in% c(2004, 2006)) %>%
  mutate(year = case_when(
    year == 2004 ~ 2019,
    year == 2006 ~ 2020
  ))

effic <- effic_tmp %>%
  filter(year >= 2005) %>%
  bind_rows(., effic_19_20) %>% #*#KM: now effic has complete coverage of years that matches SPARROW data
  filter(BMP %in% relevant_BMP)


# 3. Combine Data Frames -----

### Removing instances of NAs for intercepts (no intercept is equivalent to 0 intercept) -----
combined_costs <- all_HRUs_withcosts %>% mutate(
  across(contains("Intercept"), ~ case_when(is.na(.) ~ 0, T ~ .)))

### Assign instances of Grass Swale BMP with Model 0 fixed cost based on WERF tool-----
combined_costs <- combined_costs %>% mutate(om_ModelLabel = case_when(
  BMP.Type == "Grass_Swale_w_D" ~ "MODEL0",
  TRUE ~ om_ModelLabel
))

# ND Drop this section because costs not varying by year
### Create average precip across model years 
# combined_costs_avg <- combined_costs %>%
#   filter(Year %in% year_filter) %>% ##year filter assigned in preprocessing code
#   group_by(BMP.Type, Land_Use, HSG, COMID, cap_ModelLabel, om_ModelLabel, min_cap_ModelLabel, min_om_ModelLabel) %>%
#   summarize(across(everything(), ~ mean(.)),
#             .groups = "drop") %>%
#   select(-Year) %>%
#   rename(AnAvPpt_in = AnPpt_in)
combined_costs_avg <- combined_costs  

# 4. Calculate Costs ------

allHRUs <- combined_costs_avg %>%
  mutate(AnAvPpt_in = AnPpt_in) %>%
  mutate(
    o_capital_2020USD = case_when(
      bestmodel_capcost == 0 ~ mean_capcost,
#      is.na(bestAIC_capcost) | is.na(cap_AIC1) | is.na(cap_AIC2) | is.na(cap_AIC3) | is.na(cap_AIC4) ~ mean_capcost,
      cap_ModelLabel == "MODEL0" ~ cap_Intercept,
      PrecOrder_capcost==0 & Ro2dOrder_capcost==0 ~ mean_capcost,
      cap_ModelLabel == "MODEL1" ~ cap_Intercept + (Annav_capcost_coeff * AnAvPpt_in),
      cap_ModelLabel == "MODEL2" ~ cap_Intercept + (ro2d_capcost_coeff * ro2d_in),
      cap_ModelLabel == "MODEL3" ~ cap_Intercept + (Annav_capcost_coeff * AnAvPpt_in) + (Annav2_capcost_coeff * (AnAvPpt_in)^2),
      cap_ModelLabel == "MODEL4" ~ cap_Intercept + (ro2d2_capcost_coeff * (ro2d_in)^2) + (ro2d_capcost_coeff * ro2d_in),
      TRUE ~ mean_capcost),
    m_capital_2020USD = case_when(
      min_cap_bestmodel_capcost == 0 ~ min_cap_mean_capcost,
      is.na(min_cap_bestAIC_capcost) | is.na(min_cap_AIC1) | is.na(min_cap_AIC2) | is.na(min_cap_AIC3) | is.na(min_cap_AIC4) ~ min_cap_mean_capcost,
      min_cap_ModelLabel == "MODEL0" ~ min_cap_Intercept,
      min_cap_PrecOrder_capcost==0 & min_cap_Ro2dOrder_capcost==0 ~ min_cap_mean_capcost,
      min_cap_ModelLabel == "MODEL1" ~ min_cap_Intercept + (min_cap_Annav_capcost_coeff * AnAvPpt_in),
      min_cap_ModelLabel == "MODEL2" ~ min_cap_Intercept + (min_cap_ro2d_capcost_coeff * ro2d_in),
      min_cap_ModelLabel == "MODEL3" ~ min_cap_Intercept + (min_cap_Annav2_capcost_coeff * (AnAvPpt_in)^2 + min_cap_Annav_capcost_coeff * AnAvPpt_in),
      min_cap_ModelLabel == "MODEL4" ~ min_cap_Intercept + (min_cap_ro2d2_capcost_coeff * (ro2d_in)^2 + min_cap_ro2d_capcost_coeff * ro2d_in),
      TRUE ~ min_cap_mean_capcost),
    o_operations_2020USD = case_when(
      bestmodel_omcost == 0 ~ mean_omcost,      
      BMP.Type == "Grass_Swale_w_D" & om_ModelLabel == "MODEL0" ~ mean_omcost,
      om_ModelLabel == "MODEL0" ~ om_Intercept,
      PrecOrder_omcost==0 & Ro2dOrder_omcost==0 ~ mean_omcost,
      om_ModelLabel == "MODEL1" ~ om_Intercept + (Annav_omcost_coeff * AnAvPpt_in),
      om_ModelLabel == "MODEL2" ~ om_Intercept + (ro2d_omcost_coeff * ro2d_in),
      om_ModelLabel == "MODEL3" ~ om_Intercept + (Annav2_omcost_coeff * (AnAvPpt_in)^2) + (Annav_omcost_coeff * AnAvPpt_in),
      om_ModelLabel == "MODEL4" ~ om_Intercept + (ro2d2_omcost_coeff * (ro2d_in)^2) + (ro2d_omcost_coeff * ro2d_in),
      TRUE ~ mean_omcost),
    m_operations_2020USD = case_when(
      bestmodel_omcost == 0 ~ min_om_mean_omcost,
      min_om_ModelLabel == "MODEL0" ~ min_om_Intercept,
      min_om_PrecOrder_omcost==0 & min_om_Ro2dOrder_omcost==0 ~ min_om_mean_omcost,
      min_om_ModelLabel == "MODEL1" ~ min_om_Intercept + (min_om_Annav_omcost_coeff * AnAvPpt_in),
      min_om_ModelLabel == "MODEL2" ~ min_om_Intercept + (min_om_ro2d_omcost_coeff * ro2d_in),
      min_om_ModelLabel == "MODEL3" ~ min_om_Intercept + (min_om_Annav2_omcost_coeff * (AnAvPpt_in)^2) + (min_om_Annav_omcost_coeff * AnAvPpt_in),
      min_om_ModelLabel == "MODEL4" ~ min_om_Intercept + (min_om_ro2d2_omcost_coeff * (ro2d_in)^2) + (min_om_ro2d_omcost_coeff * ro2d_in),
      TRUE ~ min_om_mean_omcost
    ))


### Decide whether to use standard costs or minimum costs -----
allHRUs_costs <- allHRUs %>% 
  mutate(
    capital_2020USD = case_when(
      is.na(m_capital_2020USD) ~ o_capital_2020USD,
      o_capital_2020USD >= m_capital_2020USD ~ o_capital_2020USD,
      m_capital_2020USD > o_capital_2020USD ~ m_capital_2020USD,
    ),
    operations_2020USD = case_when(
      is.na(m_operations_2020USD) ~ o_operations_2020USD,
      o_operations_2020USD >= m_operations_2020USD & o_operations_2020USD > 0 ~ o_operations_2020USD,
      m_operations_2020USD > o_operations_2020USD & m_operations_2020USD > 0 ~ m_operations_2020USD,
      m_operations_2020USD < 0 ~ min_om_mean_omcost
    ),
    cap_rmse = case_when(
      is.na(m_capital_2020USD) ~ cap_rmse,
      o_capital_2020USD >= m_capital_2020USD ~ cap_rmse,
      m_capital_2020USD > o_capital_2020USD ~ min_cap_rmse
    ),
    om_rmse = case_when(
      is.na(m_operations_2020USD) ~ om_rmse,
      o_operations_2020USD >= m_operations_2020USD ~ om_rmse,
      m_operations_2020USD > o_operations_2020USD ~ min_om_min_om_rmse
    )
  ) %>%
  select(-c(min_cap_rmse, min_om_min_om_rmse))

#* Assign Porous pavement BMP with HSG A a capital cost of $0, according to assumption set by Naomi
#* ND Not sure where this came from (WA restriction on porous pavement for "Type A" soils <> HSG A?) 
#* ND With updated inputs code above now selects for average cost for porous pavement with HSG A
allHRUs_costs <- allHRUs_costs %>% 
#   mutate(capital_2020USD = case_when(
#   BMP.Type == "Porous_Pavement_w_UD" & HSG == "A" ~ 0, 
#   TRUE ~ capital_2020USD
# )) %>%
  select(BMP.Type:COMID, Area, AnAvPpt_in, ro2d_in, land_val_acre, capital_2020USD, operations_2020USD, cap_rmse, om_rmse)


# # 5. Add in Rain Garden Costs -----
# As of 4/30/2025 - No longer considering rain gardens since not recommended by WA for WQ treatment

# ND: Only remaining problem is infiltration basins with negative O&M costs

# 6. Substitute Costs for remaining BMPs with neg costs -----
# ND: No longer necessary

# ### Read in substitution files -----
# inf_basin_capcosts <- read.xlsx("./Data/Inbncapcostparam_statswq12.xlsx", sheet = "#LN00065", startRow = 3) %>%
#   dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, mean_capcost, min_capcost, max_capcost, cap_rmse = Root.mean.squared.error,
#                 Annav_capcost_coeff = AnnavPrecipin, ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2, ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4,
#                 bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)
# 
# inf_basin_capcosts$BMP.Type <- gsub(" ", "_", inf_basin_capcosts$BMP.Type)
# inf_basin_capcosts$BMP.Type <- gsub("/", "_", inf_basin_capcosts$BMP.Type)
# 
# inf_basin_omcosts <- read.xlsx("./Data/InfBasinOMmincostequations_Models1and2.xls.xlsx", sheet = "#LN02535", startRow = 3) %>%
#   dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Annav_omcost_coeff = AnnavPrecipin, om_rmse = Root.mean.squared.error,
#                 ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2, ro2d2_omcost_coeff = ro2d_in2, AIC1:AIC4,
#                 bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel)
# 
# inf_basin_omcosts$BMP.Type <- gsub(" ", "_", inf_basin_omcosts$BMP.Type)
# inf_basin_omcosts$BMP.Type <- gsub("/", "_", inf_basin_omcosts$BMP.Type)
# 
# #* ND Instead read in table of porous pavement costs constant by HRU x HSG, don't use regressions
# #porpav_capcosts <- read.xlsx("./Data/Capcostparamppwid_statswq2.xlsx", sheet = "#LN00058", startRow = 3) %>%
# #  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, mean_capcost, min_capcost, max_capcost, cap_rmse = Root.mean.squared.error,
# #                Annav_capcost_coeff = AnnavPrecipin, ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2, ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4,
# #                bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)
# 
# #porpav_capcosts$BMP.Type <- gsub(" ", "_", porpav_capcosts$BMP.Type)
# #porpav_capcosts$BMP.Type <- gsub("/", "_", porpav_capcosts$BMP.Type)
# 
# #porpav_omcosts <- read.xlsx("./Data/Omcostparamppwid_statswq2.xlsx", sheet = "#LN00061", startRow = 3) %>%
# #  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, mean_omcost, min_omcost, max_omcost, om_rmse = Root.mean.squared.error,
# #                Annav_omcost_coeff = AnnavPrecipin, ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2, ro2d2_omcost_coeff = ro2d_in2, 
# #                bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel, PrecOrder_omcost = PrecOrder, Ro2dOrder_omcost = Ro2dOrder)
# 
# #porpav_omcosts$BMP.Type <- gsub(" ", "_", porpav_omcosts$BMP.Type)
# #porpav_omcosts$BMP.Type <- gsub("/", "_", porpav_omcosts$BMP.Type)
# 
# 
# ### Pull out parts of final all hru to merge -----
# 
# ##### Fixing Infiltration Basin Cap Costs  -----
# 
# inf_neg <- allHRUs_costs %>% filter(BMP.Type == "Infiltration_Basin")
# 
# inf_neg_cap_sub <- merge(inf_neg, inf_basin_capcosts, by = c("BMP.Type", "Land_Use", "HSG"))
# 
# inf_cap_cost <- inf_neg_cap_sub %>%
#   mutate(
#     new_cap_cost = case_when(
#       ModelLabel == "MODEL1" ~ (Annav_capcost_coeff * AnAvPpt_in),
#       ModelLabel == "MODEL2" ~ (ro2d_capcost_coeff * ro2d_in),
#       TRUE ~ mean_capcost
#     ),
#     cap_rmse.y = as.numeric(cap_rmse.y)
#   ) %>%
#   filter(ModelLabel == "MODEL1")
# 
# 
# inf_cost <- inf_cap_cost %>%
#   mutate(
#     cap_rmse = case_when(capital_2020USD < 0 ~ cap_rmse.y, TRUE ~ cap_rmse.x),
#     capital_2020USD = case_when(capital_2020USD < 0 ~ new_cap_cost, TRUE ~ capital_2020USD)
#   ) %>%
#   select(BMP.Type:operations_2020USD, om_rmse, cap_rmse)
# 
# no_inf_costs <- allHRUs_costs %>%
#   filter(!BMP.Type == "Infiltration_Basin")
# 
# all_HRU_temp <- rbind(no_inf_costs, inf_cost)
# 
# ##### Fixing Infiltration Basin OM Costs -----
# 
# inf_pos <- all_HRU_temp %>% filter(BMP.Type == "Infiltration_Basin") %>%
#   filter(operations_2020USD >= 0)
# inf_neg <- all_HRU_temp %>% filter(BMP.Type == "Infiltration_Basin") %>%
#   filter(operations_2020USD < 0)
# 
# inf_neg_om_sub <- merge(inf_neg, inf_basin_omcosts, by = c("BMP.Type", "Land_Use", "HSG"))
# 
# inf_om_cost_temp <- inf_neg_om_sub %>%
#   mutate(
#     new_om_cost = case_when(
#       ModelLabel == "MODEL1" ~ (Annav_omcost_coeff * AnAvPpt_in),
#       ModelLabel == "MODEL2" ~ (ro2d_omcost_coeff * ro2d_in),
#       TRUE ~ operations_2020USD
#       ),
#     om_rmse.y = as.numeric(om_rmse.y)
#     )
# 
# inf_om_cost <- inf_om_cost_temp %>%
#   group_by(BMP.Type, Land_Use, HSG, COMID) %>% 
#   slice_max(new_om_cost, n = 1) %>%
#   ungroup()
# 
# inf_cost_temp <- inf_om_cost %>%
#   mutate(
#     om_rmse = case_when(operations_2020USD < 0 ~ om_rmse.y, TRUE ~ om_rmse.x),
#     operations_2020USD = case_when(operations_2020USD < 0 ~ new_om_cost, TRUE ~ operations_2020USD)
#   ) %>%
#   select(BMP.Type:operations_2020USD, om_rmse, cap_rmse)
# 
# inf_cost <- rbind(inf_pos, inf_cost_temp)
# 
# no_inf_costs <- all_HRU_temp %>%
#   filter(!BMP.Type == "Infiltration_Basin")
# 
# all_HRU_temp <- rbind(no_inf_costs, inf_cost)
# 
# ##### Fixing Porous Pavement Capital Costs -----
# #* ND Instead read in table of porous pavement costs constant by HRU x HSG, don't use regressions
# 
# #pp_pos <- all_HRU_temp %>%
# #  filter(BMP.Type == "Porous_Pavement_w_UD") %>%
# #  filter(capital_2020USD >= 0)
# 
# #pp_neg <- all_HRU_temp %>%
# #  filter(BMP.Type == "Porous_Pavement_w_UD") %>%
# #  filter(capital_2020USD < 0)
# 
# #pp_neg_cap_sub <- merge(pp_neg, porpav_capcosts, by = c("BMP.Type", "Land_Use", "HSG"))
# 
# #pp_cap_cost_temp <- pp_neg_cap_sub %>%
# #  mutate(
# #    new_cap_cost = case_when(
# #      ModelLabel == "MODEL1" ~ (Annav_capcost_coeff * AnAvPpt_in),
# #      ModelLabel == "MODEL2" ~ (ro2d_capcost_coeff * ro2d_in),
# #      TRUE ~ mean_capcost
# #      ),
# #    cap_rmse.y = as.numeric(cap_rmse.y)
# #    )
# 
# #pp_cap_cost <- pp_cap_cost_temp %>%
# #  group_by(BMP.Type, Land_Use, HSG, COMID) %>% 
# #  slice_max(new_cap_cost, n = 1) %>%
# #  ungroup()
# 
# #pp_cost_temp <- pp_cap_cost %>%
# #  mutate(
# #    cap_rmse = case_when(capital_2020USD < 0 ~ cap_rmse.y, TRUE ~ cap_rmse.x),
# #    capital_2020USD = case_when(capital_2020USD < 0 ~ new_cap_cost, TRUE ~ capital_2020USD)
# #  ) %>%
# #  select(BMP.Type:operations_2020USD, om_rmse, cap_rmse)
# 
# #pp_cost <- rbind(pp_pos, pp_cost_temp)
# 
# #no_pp_costs <- all_HRU_temp %>%
# #  filter(!BMP.Type == "Porous_Pavement_w_UD")
# 
# #all_HRU_temp <- rbind(no_pp_costs, pp_cost)
# 
# 
# ##### Fixing Porous Pavement OM Costs -----
# 
# #pp_pos <- all_HRU_temp %>% filter(BMP.Type == "Porous_Pavement_w_UD") %>%
# #  filter(operations_2020USD >= 0)
# 
# #pp_neg <- all_HRU_temp %>% filter(BMP.Type == "Porous_Pavement_w_UD") %>%
# #  filter(operations_2020USD < 0 | is.na(operations_2020USD))
# 
# #pp_neg_om_sub <- merge(pp_neg, porpav_omcosts, by = c("BMP.Type", "Land_Use", "HSG"))
# 
# #pp_om_cost_temp <- pp_neg_om_sub %>%
# #  mutate(
# #    new_om_cost = case_when(
# #      ModelLabel == "MODEL1" ~ (Annav_omcost_coeff * AnAvPpt_in),
# #      ModelLabel == "MODEL2" ~ (ro2d_omcost_coeff * ro2d_in),
# #      TRUE ~ mean_omcost
# #      ),
# #    om_rmse.y = as.numeric(om_rmse.y)
# #    )
# 
# #pp_om_cost <- pp_om_cost_temp %>%
# #  group_by(BMP.Type, Land_Use, HSG, COMID) %>% 
# #  slice_max(new_om_cost, n = 1) %>%
# #  ungroup()
# 
# #pp_cost_temp <- pp_om_cost %>%
# #  mutate(
# #    om_rmse = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ om_rmse.y, TRUE ~ om_rmse.x),
# #    operations_2020USD = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ new_om_cost, TRUE ~ operations_2020USD)
# #  ) %>%
# #  select(BMP.Type:operations_2020USD, om_rmse, cap_rmse)
# 
# #pp_cost <- rbind(pp_pos, pp_cost_temp)
# 
# #no_pp_costs <- all_HRU_temp %>%
# #  filter(!BMP.Type == "Porous_Pavement_w_UD")

# START HERE

# all_HRU <- rbind(no_pp_costs, pp_cost)

all_HRU <- allHRUs_costs %>%
  mutate(capital_2025USD = capital_2020USD * 1.48,
         operations_2025USD = operations_2020USD * 1.48,
         cap_rmse2025 = cap_rmse * 1.48,
         om_rmse2025 = om_rmse * 1.48)

all_HRU_effic <- merge(all_HRU, effic, by.x = c("BMP.Type", "Land_Use"), by.y = c("BMP", "Land_Use"))

all_HRU_effic <- all_HRU_effic %>%
  mutate(
    effic_N = case_when(is.na(mean_efficN) ~ 0, TRUE ~ mean_efficN),
    effic_P = case_when(is.na(mean_efficP) ~ 0, TRUE ~ mean_efficP)
  ) %>% #@# Where we have no data, assigns efficiency of 0 so RBEROST is detered from selecting
  select(BMP.Type:season, std_efficN, n_efficN, effic_N, std_efficP, effic_P, n_efficP)

qa <- all_HRU %>%
  group_by(COMID) %>%
  mutate(count = n_distinct(HSG))

## print effic separately

# 7. Initial QA -----

cost_capqa <- all_HRU_effic %>%
  filter(capital_2020USD < 0 | is.na(capital_2020USD))

cost_omqa <- all_HRU_effic %>%
  filter(operations_2020USD < 0 | is.na(operations_2020USD))

### Costs -----
## Uncomment below to see histogram of costs
hist(all_HRU_effic$capital_2020USD)
 hist(all_HRU_effic$operations_2020USD)
 
summary(all_HRU_effic$capital_2020USD)
summary(all_HRU_effic$operations_2020USD)

om_qa <- all_HRU_effic %>%
   filter(is.na(operations_2020USD))

om_count <- all_HRU_effic %>%
  group_by(BMP.Type) %>%
  tally()

### Efficiencies -----
## Uncomment below to see histogram of efficiencies
 hist(all_HRU_effic$effic_N)
 hist(all_HRU_effic$effic_P)
 
 summary(all_HRU_effic$effic_N)
 summary(all_HRU_effic$effic_P)

wetpond_qa <- all_HRU_effic %>% filter(BMP.Type == "Wet_Pond") 
summary(wetpond_qa$effic_N)


# 8. Implementing Threshold Values & Subsequent QA -----
# ND: Revisit need for threshold values after cost corrections

### Combined Threshold Values (EPA preferred) -----

all_HRU_ce <- all_HRU_effic %>% mutate(cap_p = capital_2020USD/(effic_P*100),
                                 cap_n = capital_2020USD/(effic_N*100),
                                 op_p = operations_2020USD/(effic_P*100),
                                 op_n = operations_2020USD/(effic_N*100))
# Try 99%ile threshold instead of 75%ile
cap_p_thresh <- quantile(all_HRU_ce$cap_p, 0.99, na.rm = T)
cap_n_thresh <- quantile(all_HRU_ce$cap_n, 0.99, na.rm = T)
op_p_thresh <- quantile(all_HRU_ce$op_p, 0.99, na.rm = T)
op_n_thresh <- quantile(all_HRU_ce$op_n, 0.99, na.rm = T)

### Applying thresholds defined aboved -----
filter_qa <- all_HRU_ce %>% filter(cap_n <= cap_n_thresh & cap_p <= cap_p_thresh & op_n <= op_n_thresh & op_p <= op_p_thresh)

### New QA check  -----
dropped <- all_HRU_effic %>%
  select(BMP.Type, Land_Use, HSG, year, season, COMID) %>%
  anti_join(., filter_qa, by = c("BMP.Type", "Land_Use", "HSG", "year", "season", "COMID")) %>%
  unique() %>%
  group_by(COMID, BMP.Type, Land_Use, HSG) %>%
  tally()

final_drop <- dropped %>%
  filter(n == 64) %>% ## Only drop BMP x COMID combinations that are dropped for all seasons and years (16 years x 4 seasons = 64). Prevents BMPs from being able to be selected in one year and not the other.
  select(-n)

final_combo_data <- all_HRU_effic %>%
  anti_join(., final_drop, by = c("COMID", "BMP.Type", "Land_Use", "HSG"))

# 1.046980229 annual inflation value from 2020 to 2021 from the Bureau of Labor Statistics
inflate_2021 <- 1.046980229
inflate_2025 <- 1.48 # inflation value from 2020 to 2025


cost_data <- final_combo_data %>%
  select(BMP.Type:om_rmse) %>%
  unique() %>%
  rename(comid = COMID, BMP = BMP.Type) %>%
  # Replace NA HSG values with placeholder to prevent model errors
  mutate(HSG = ifelse(is.na(HSG), "NA", HSG))

final_cost_data <- cost_data %>%
  mutate(
    capital_2025USD = capital_2020USD * inflate_2025,
    operations_2025USD = operations_2020USD * inflate_2025,
    cap_rmse = cap_rmse * inflate_2025,
    om_rmse = om_rmse * inflate_2025
  ) %>%
  select(-c(capital_2020USD, operations_2020USD)) %>%
  # Replace NA costs with very high value to deter model from selecting
  mutate(
    capital_2025USD = ifelse(is.na(capital_2025USD), 1e15, capital_2025USD),
    operations_2025USD = ifelse(is.na(operations_2025USD), 1e15, operations_2025USD)
  )

qa_cost <- final_cost_data %>%
  filter(is.na(capital_2025USD) | is.na(operations_2025USD))

final_effic_data <- final_combo_data %>%
  select(BMP.Type, Land_Use, HSG, year:n_efficP) %>%
  unique() %>%
  rename(BMP = BMP.Type) %>%
  # Replace NA HSG values with placeholder to prevent model errors
  mutate(HSG = ifelse(is.na(HSG), "NA", HSG)) %>%
  # Replace NA efficiencies with 0 to deter model from selecting
  mutate(
    effic_N = ifelse(is.na(effic_N), 0, effic_N),
    effic_P = ifelse(is.na(effic_P), 0, effic_P)
  ) %>%
  mutate(quarter = case_when(season == "WTR" ~ 1, season == "SPR" ~ 2, season == "SMR" ~ 3, T ~ 4)) %>%
  select(-season)

qa_effic <- final_effic_data %>%
  filter(is.na(effic_N) | is.na(effic_P))

### Other QA ----

# thresh_qa <- filter_qa %>% group_by(BMP.Type) %>% tally()
# hru_thresh <- filter_qa %>% mutate(index = paste0(COMID, "&", Land_Use, "&", HSG))
# length(unique(hru_thresh$index))
# 
# summary(filter_qa$capital_2020USD)
# summary(filter_qa$operations_2020USD)
# summary(filter_qa$effic_N)
# summary(filter_qa$effic_P)
# 
# effic_qa <- filter_qa %>% filter(effic_N < 0.05)
# effic_qaP <- filter_qa %>% filter(effic_P < 0.05)
# 
# ##### Cost comparisons -----
# hist(filter_qa$capital_2020USD)
# hist(filter_qa$operations_2020USD)
# 
# summary(filter_qa$capital_2020USD)
# summary(filter_qa$operations_2020USD)
# 
# ##### Efficiency comparisons -----
# hist(filter_qa$effic_N)
# hist(filter_qa$effic_P)
# 
# summary(filter_qa$effic_N)
# summary(filter_qa$effic_P)
# 
# 
# ### Individual Threshold Values -----
# 
 p_thresh <- 0.05
 n_thresh <- 0.05
 capital_thresh <- quantile(all_HRU$capital_2020USD, 0.99, na.rm = T)
 operations_thresh <- quantile(all_HRU$operations_2020USD, 0.99, na.rm = T)
 
 effic_drop <- all_HRU_effic %>% filter(effic_N < 0.05 & effic_P < 0.05)
 
 bound_HRU_temp <- anti_join(all_HRU, effic_drop, by = c("BMP.Type", "Land_Use", "HSG", "COMID"))
 
 all_HRU_bounded <- bound_HRU_temp %>% filter(capital_2020USD <= capital_thresh & operations_2020USD <= operations_thresh)
# 
# thresh_qa <- all_HRU_bounded %>% group_by(BMP.Type) %>% tally()
# 
# effic_qa_old <- all_HRU_bounded %>% filter(effic_N < 0.05)
# effic_qaP_old <- all_HRU_bounded %>% filter(effic_P < 0.05)
# 
# ##### Cost comparisons -----
# hist(all_HRU_bounded$capital_2020USD)
# hist(all_HRU_bounded$operations_2020USD)
# 
# summary(all_HRU_bounded$capital_2020USD)
# summary(all_HRU_bounded$operations_2020USD)
# 
# ##### Efficiency comparisons -----
# hist(all_HRU_bounded$effic_N)
# hist(all_HRU_bounded$effic_P)
# 
# summary(all_HRU_bounded$effic_N)
# summary(all_HRU_bounded$effic_P)
# 
# 
# ### Applying only cost thresholds from individual threshold values -----
# trial_thresh <- all_HRU %>% filter(capital_2020USD <= capital_thresh, operations_2020USD <= operations_thresh)
# dropped2 <- anti_join(all_HRU, trial_thresh, by = c("BMP.Type", "Land_Use", "HSG", "COMID"))
# dropped_qa2 <- dropped2 %>% group_by(BMP.Type) %>% tally()
# summary(dropped2$capital_2020USD)
# summary(dropped2$operations_2020USD)
# 
# cost_thresh_qa <- trial_thresh %>% group_by(BMP.Type) %>% tally()
# 
# ### Compare COMID, Land Use, and HSG combinations -----
# # Comparing comid x land use x HSG combinations and what is dropped with the application of thresholds (compare to hru_thresh from combined thresholds)
# hru_indexed <- all_HRU %>% mutate(index = paste0(COMID, "&", Land_Use, "&", HSG))
# length(unique(hru_indexed$index))
# 
# hru_trimmed <- all_HRU_bounded %>% mutate(index = paste0(COMID, "&", Land_Use, "&", HSG))
# length(unique(hru_trimmed$index))
# 
# hru_trim2 <- trial_thresh %>% mutate(index = paste0(COMID, "&", Land_Use, "&", HSG))
# length(unique(hru_trim2$index))
# 
# 
# 
# ##### Cost comparison to WMOST technical papers -----
# #Combined thresholds
# cost_comp <- filter_qa %>% mutate(capital_2020USD = capital_2020USD/(43560*0.08), operations_2020USD = operations_2020USD/(43560*0.08))
# summary(cost_comp$capital_2020USD)
# summary(cost_comp$operations_2020USD)
# 
# cost_comp_qa <- cost_comp %>% group_by(BMP.Type) %>% summarise(capital_2020USD = mean(capital_2020USD), operations_2020USD = mean(operations_2020USD))
# 
# ##### Cost comparison to WMOST technical papers -----
# #Individual thresholds
 # ND: converts costs/acre impervious area to cost per square foot of bmp designed for 1" runoff (New England)
 # ND: However design depths for PS vary spatially with 90%ile of ro2d 0.4 - 4.02 in 
 # ND: In addition PS costs are per acre developed land, not per acre impervious area
 # ND: More appropriate comparison would be for LC = UIDU only
 # ND: Also, complexity factors from SWC may be greater than complexity factors used by Opti-Tool (2-3)
# cost_comp_old <- all_HRU_bounded %>% mutate(capital_2020USD = capital_2020USD/(43560*0.08), operations_2020USD = operations_2020USD/(43560*0.08))
# summary(cost_comp_old$capital_2020USD)
# summary(cost_comp_old$operations_2020USD)

summary(RO2d_in$ro2d_in) # median ro2d_in = 1.42;
 
 cost_comp_oldUIDU <- all_HRU_bounded %>% 
   filter(Land_Use == 'UIDU' )  %>%
   mutate(capital_2020USD = capital_2020USD/(43560*0.08*1.42), operations_2020USD = operations_2020USD/(43560*0.08*1.42))
 summary(cost_comp_oldUIDU$capital_2020USD)
 summary(cost_comp_oldUIDU$operations_2020USD)
  comp_old_qaUIDU <- cost_comp_oldUIDU %>% group_by(BMP.Type) %>% summarise(capital_2020USD = mean(capital_2020USD), operations_2020USD = mean(operations_2020USD))

# ND: average costs are comparable to those in Opti-Tool w median sizing requirements (w/out correction for Region)
# except for Infiltration Chamber and Infiltration Trench and Porous Pavement w SI (higher)
  
# 9. Print Final Dataset -----

#### Write out dataset -----
# Choose data set to write depending on which threshold method EPA moves forward with (currently using combined thresholds)
# final_data <- filter_qa %>% select(BMP.Type:HSG, Area, effic_N, effic_P, std_efficN, std_efficP, n_efficN, n_efficP,
#                                    capital_2020USD, cap_rmse, operations_2020USD, om_rmse) %>% rename(comid = COMID, BMP = BMP.Type)

write.csv(final_cost_data, paste0(InPath,"UrbanBMPData.csv"), row.names = F)
write.csv(final_effic_data, paste0(InPath,"UrbanBMP_Effic_Data.csv"), row.names = F)

## Summary stats
 summary_table <- filter_qa %>%
   group_by(BMP.Type, Land_Use, HSG) %>%
   summarize(avg_efficN = mean(effic_N),
             avg_efficP = mean(effic_P),
             avg_cap = mean(capital_2020USD),
             avg_om = mean(operations_2020USD))
 write.csv(summary_table, "./Data/BMP_cost_summary_2025.csv", row.names = F)
