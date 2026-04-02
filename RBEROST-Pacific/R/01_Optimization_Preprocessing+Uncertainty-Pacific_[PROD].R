### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# PURPOSE: Generate Uncertainty Estimates in the AMPL Model
# BY: Cathy Chamberlin, Kelly-Anne Moffa (ICF), Hunter Parker (ICF), Sam Ennett (ICF)
# DATE: 4/1/21
# UPDATED DATE: June 2024
# EPA updates by Craig Connolly, Naomi Detenbeck
# Last updated March 2026
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Based on: RBEROST-Northeast preprocessor with uncertainty.
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


# Notes, Introduction, and Set up -----

# General Notes:
# This step generates the uncertainty estimates used in the AMPL model. 
# It is designed to be run after the Preprocessor, and will amend the AMPL files.
# This Preprocessor is sourced through the RunRBEROST-Pacific.Rmd file.
# Installation of necessary packages is done through this RunRBEROST-Pacific.Rmd file as well.
# If more advanced R users would like to assess different locations/data, they would need to search for and update lines of code that end in "#*#"
# Author notes are indicated by "#@# Note:"

### ### ### ### ### ### ### ### ### 
# PART 1: PREPROCESSOR STEP ------
### ### ### ### ### ### ### ### ###

### I. Run Preprocessor without uncertainty ------
source(paste0(working_dir,"./RBEROST-Pacific/R/01_Optimization_Preprocessing-Pacific_V2_[PROD].R", local = TRUE))

print(
  paste(
    "RBEROST is now creating AMPL files with uncertainty analysis at",
    Sys.time()
  )
)

### II. Define scenario increment ------
scenarioincrement <- 1 - scenariostepchange

### ### ### ### ### ### ### ### ### 
# PART 2: ADJUST & PREPROCESS DATA INPUTS TO INCLUDE UNCERTAINTY ------
### ### ### ### ### ### ### ### ### 

### I. SPARROW N uncertainty ------

temp_inc_tn <- sparrow_cons_out_tn %>% 
  select(c("comid", "year", "season", all_of(tn_cols), all_of(se_tn_cols))) %>%
  filter(year %in% year_filter)

temp_inc_tn$in_total <- with(temp_inc_tn, PLOAD_INC_TOTAL)
temp_inc_tn$sin_total <- with(temp_inc_tn, SE_PLOAD_INC_TOTAL)

temp_inc_tn$in_ag <- with(temp_inc_tn, PLOAD_INC_AG_KG + PLOAD_INC_CAFOS) #*#

temp_inc_tn$sin_ag <- temp_inc_tn %>%
  rowwise() %>% 
  mutate(
    sin_ag = my_propogateerror(
      vals = list(
        c(PLOAD_INC_AG_KG, SE_PLOAD_INC_AG_KG), c(PLOAD_INC_CAFOS, SE_PLOAD_INC_CAFOS)
      ), 
      method = "addsub"
    )
  ) %>%
  ungroup() %>%
  .$sin_ag

temp_inc_tn$in_urb <- with(temp_inc_tn, PLOAD_INC_URB_KM2)
temp_inc_tn$sin_urb <- with(temp_inc_tn, SE_PLOAD_INC_URB_KM2)

# ND Added septic 3/1/2026
temp_inc_tn$in_septic <- with(temp_inc_tn, PLOAD_INC_SEPTIC)
temp_inc_tn$sin_septic <- with(temp_inc_tn, SE_PLOAD_INC_SEPTIC)

temp_inc_tn$in_road <- with(temp_inc_tn, PLOAD_INC_ATM_KG) #*#
temp_inc_tn$sin_road <- with(temp_inc_tn, SE_PLOAD_INC_ATM_KG) #*#

temp_inc_tn$in_poin <- with(temp_inc_tn, PLOAD_INC_POINT1_KG)
temp_inc_tn$sin_poin <- with(temp_inc_tn, SE_PLOAD_INC_POINT1_KG)

temp_inc_tn$in_graz <- 0 #*# KM: Maintaining so structure is present if loads become significant in future SPARROW models
temp_inc_tn$sin_graz <- 0

temp_inc_tn_rev <- temp_inc_tn

# temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, PLOAD_INC_FOREIGN + PLOAD_INC_SEPTIC + PLOAD_INC_NFIX_M2) #*# 
temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, PLOAD_INC_FOREIGN + PLOAD_INC_NFIX_M2) # CTC edited: 2/6/2025
temp_inc_tn_rev$sin_other <- temp_inc_tn_rev %>%
  rowwise() %>% 
  mutate(
    sin_other = my_propogateerror(
      vals = list(
        # c(PLOAD_INC_FOREIGN, SE_PLOAD_INC_FOREIGN), c(PLOAD_INC_SEPTIC, SE_PLOAD_INC_SEPTIC), c(PLOAD_INC_NFIX_M2, SE_PLOAD_INC_NFIX_M2)
        c(PLOAD_INC_FOREIGN, SE_PLOAD_INC_FOREIGN), c(PLOAD_INC_NFIX_M2, SE_PLOAD_INC_NFIX_M2) # CTC edited: 2/6/2026
      ), 
      method = "addsub"
    )
  ) %>%
  ungroup() %>%
  .$sin_other

temp_inc_tn_rev$in_storage <- with(temp_inc_tn_rev, PLOAD_INC_STORAGE) #*#
temp_inc_tn_rev$sin_storage <- with(temp_inc_tn_rev, SE_PLOAD_INC_STORAGE) #*#

inc_tn <- temp_inc_tn_rev %>% 
  select(
    c(
      "comid", "year", "season", "in_total", "in_ag", "in_urb", "in_road", "in_poin", "in_graz", "in_septic", "in_storage", "in_other", # CTC edited: 2/6/2026
      "sin_total", "sin_ag", "sin_urb", "sin_road", "sin_poin", "sin_graz", "sin_storage", "sin_septic", "sin_other"
      )
    ) %>%
  mutate(across(.cols = -comid, .fns = ~replace_na(., replace = 0)))

### III. Specify P loads ------

temp_inc_tp <- sparrow_cons_out_tp %>% 
  select(c("comid", "year", "season"), all_of(tp_cols), all_of(se_tp_cols)) %>%
  filter(year %in% year_filter)

temp_inc_tp$ip_total <- with(temp_inc_tp, PLOAD_INC_TOTAL)
temp_inc_tp$sip_total <- with(temp_inc_tp, SE_PLOAD_INC_TOTAL)

temp_inc_tp$ip_ag <- with(temp_inc_tp, PLOAD_INC_AG_KG + PLOAD_INC_CAFOS) #*#
temp_inc_tp$sip_ag <- temp_inc_tp %>%
  rowwise() %>% 
  mutate(
    sip_ag = my_propogateerror(
      vals = list(
        c(PLOAD_INC_AG_KG, SE_PLOAD_INC_AG_KG), c(PLOAD_INC_CAFOS, SE_PLOAD_INC_CAFOS)
      ), 
      method = "addsub"
    )
  ) %>%
  ungroup() %>%
  .$sip_ag

temp_inc_tp$ip_urb <- with(temp_inc_tp, PLOAD_INC_URB_KM2)
temp_inc_tp$sip_urb <- with(temp_inc_tp, SE_PLOAD_INC_URB_KM2)

# ND Added septic 3/1/2026
temp_inc_tp$in_septic <- with(temp_inc_tp, PLOAD_INC_SEPTIC)
temp_inc_tp$sin_septic <- with(temp_inc_tp, SE_PLOAD_INC_SEPTIC)

temp_inc_tp$ip_poin <- with(temp_inc_tp, PLOAD_INC_POINT1_KG)
temp_inc_tp$sip_poin <- with(temp_inc_tp, SE_PLOAD_INC_POINT1_KG)

temp_inc_tp$ip_other <- with(temp_inc_tp, PLOAD_INC_FOREIGN+PLOAD_INC_NATP_MT)
temp_inc_tp$sip_other <- temp_inc_tp %>%
  rowwise() %>% 
  mutate(
    sip_other = my_propogateerror(
      vals = list(
        c(PLOAD_INC_FOREIGN, SE_PLOAD_INC_FOREIGN), c(PLOAD_INC_NATP_MT, SE_PLOAD_INC_NATP_MT)
      ), 
      method = "addsub"
    )
  ) %>%
  ungroup() %>%
  .$sip_other

temp_inc_tp$ip_storage <- with(temp_inc_tp, PLOAD_INC_STORAGE)
temp_inc_tp$sip_storage <- with(temp_inc_tp, SE_PLOAD_INC_STORAGE)

temp_inc_tp$ip_road <- 0 #*# KM: Maintaining, but setting to 0 for consistency with TN model
temp_inc_tp$sip_road <- 0 #*# KM: Maintaining, but setting to 0 for consistency with TN model
temp_inc_tp$ip_graz <- 0 #*# KM: Maintaining so structure is present if loads become significant in future SPARROW models
temp_inc_tp$sip_graz <- 0 #*# KM: Maintaining so structure is present if loads become significant in future SPARROW models

inc_tp <- temp_inc_tp %>% 
  select(
    c(
      "comid", "year", "season", "ip_total", "ip_ag", "ip_urb", "ip_road", "ip_poin", "ip_graz", "ip_storage", "ip_other", 
      "sip_total", "sip_ag", "sip_urb", "sip_road", "sip_poin", "sip_graz", "sip_storage", "sip_other"
    )
  ) %>%
  mutate(across(.cols = -comid, .fns = ~replace_na(., replace = 0)))

### IV. N point source loading ------
inc_tn_rev <- inc_tn

### V. P point source loading ------
inc_tp_rev <- inc_tp

## propagate error in transfer coefficient
### Separate by season
sparrow_cons_out_tn_s <- inc_tn_rev %>% 
  arrange(comid, year, season) %>%
  select(comid, year, season, in_total, in_storage, sin_total, sin_storage)

### Compute transfer coefficients
sm_N <- data.frame(
  sparrow_cons_out_tn_s$comid,
  sparrow_cons_out_tn_s$year,
  sparrow_cons_out_tn_s$season,
  # Calculate transfer coefficients between seasons
  ifelse(
    sparrow_cons_out_tn_s$in_total == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tn_s$in_total, default = 0) == 0,
      sparrow_cons_out_tn_s$in_storage / lead(sparrow_cons_out_tn_s$in_total, default = 1),
      sparrow_cons_out_tn_s$in_storage / sparrow_cons_out_tn_s$in_total
    )
  ),
  # Calculate transfer coefficients standard error between seasons
  ifelse(
    sparrow_cons_out_tn_s$sin_total == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tn_s$sin_total, default = 0) == 0,
      my_propogateerror(
        vals = list(
          c(sparrow_cons_out_tn_s$in_storage, sparrow_cons_out_tn_s$sin_storage), 
          c(lead(sparrow_cons_out_tn_s$in_total, default = 1), lead(sparrow_cons_out_tn_s$sin_total, default = 1))
        ), 
        method = "div"
      ),
      my_propogateerror(
        vals = list(
          c(sparrow_cons_out_tn_s$in_storage, sparrow_cons_out_tn_s$sin_storage), 
          c(sparrow_cons_out_tn_s$in_total, sparrow_cons_out_tn_s$sin_total)
        ), 
        method = "div"
      )
    )
  )
) %>%
  rename(comid = 1,
         year = 2,
         season = 3,
         coeff = 4,
         coeff_se = 5) %>%
  mutate(across(.cols = -c(comid, year, season), .fns = ~replace_na(., replace = 0)))

### Overwrite year 20 transfer coefficients, no Y21S1 to compare to
sm_N$coeff <- ifelse(sm_N$year == 20 & sm_N$season == 4, NA, sm_N$coeff)
sm_N$coeff_se <- ifelse(sm_N$year == 20 & sm_N$season == 4, NA, sm_N$coeff_se)


#
#
## Prep TP Data by Season ---------
sparrow_cons_out_tp_s <- inc_tp_rev %>% 
  arrange(comid, year, season) %>%
  select(comid, year, season, ip_total, ip_storage, sip_total, sip_storage)

### Compute transfer coefficients
sm_P <- data.frame(
  sparrow_cons_out_tp_s$comid,
  sparrow_cons_out_tp_s$year,
  sparrow_cons_out_tp_s$season,
  # Calculate transfer coefficients between seasons
  ifelse(
    sparrow_cons_out_tp_s$ip_total == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tp_s$ip_total, default = 0) == 0,
      sparrow_cons_out_tp_s$ip_storage / lead(sparrow_cons_out_tp_s$ip_total, default = 1),
      sparrow_cons_out_tp_s$ip_storage / sparrow_cons_out_tp_s$ip_total
    )
  ),
  # Calculate transfer coefficients standard error between seasons
  ifelse(
    sparrow_cons_out_tp_s$sip_total == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tp_s$sip_total, default = 0) == 0,
      my_propogateerror(
        vals = list(
          c(sparrow_cons_out_tp_s$ip_storage, sparrow_cons_out_tp_s$sip_storage), 
          c(lead(sparrow_cons_out_tp_s$ip_total, default = 1), lead(sparrow_cons_out_tp_s$sip_total, default = 1))
        ), 
        method = "div"
      ),
      my_propogateerror(
        vals = list(
          c(sparrow_cons_out_tp_s$ip_storage, sparrow_cons_out_tp_s$sip_storage), 
          c(sparrow_cons_out_tp_s$ip_total, sparrow_cons_out_tp_s$sip_total)
        ), 
        method = "div"
      )
    )
  )
) %>%
  rename(comid = 1,
         year = 2,
         season = 3,
         coeff = 4,
         coeff_se = 5) %>%
  mutate(across(.cols = -c(comid, year, season), .fns = ~replace_na(., replace = 0)))

### Overwrite year 20 transfer coefficients, no Y21S1 to compare to
sm_P$coeff <- ifelse(sm_P$year == 20 & sm_P$season == 4, NA, sm_P$coeff)
sm_P$coeff_se <- ifelse(sm_P$year == 20 & sm_P$season == 4, NA, sm_P$coeff_se)

# # Transfer coefficient threshold
if (use_threshold) {
  sm_N[,4][sm_N[,4] > threshold] <- 0
  sm_P[,4][sm_P[,4] > threshold] <- 0
}

temp_sm_N_dat_se <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_sm_N_dat_tmp <- sm_N[sm_N$comid %in% watershed_comid$catchment_comid,] %>%
    arrange(comid) %>%
    select(-coeff) %>%
    rename(coeff = coeff_se)
  
}

temp_sm_P_dat_se <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_sm_P_dat_tmp <- sm_P[sm_P$comid %in% watershed_comid$catchment_comid,] %>%
    arrange(comid) %>%
    select(-coeff) %>%
    rename(coeff = coeff_se)
}

# (SC for StreamCat)
temp_sm_N_dat_SC_se <- foreach(i = 1:length(temp_sm_N_dat_se)) %do% {
  temp_sm_N_dat_se[[i]][
    temp_sm_N_dat_se[[i]]$comid %in% streamcat_subset_tn[[i]]$comid,
  ]
}

temp_sm_N_dat_other_se <- foreach(i = 1:length(temp_sm_N_dat_se)) %do% {
  temp_sm_N_dat_se[[i]][
    !(temp_sm_N_dat_se[[i]]$comid %in% streamcat_subset_tn[[i]]$comid),
  ]
}

# (SC for StreamCat)
temp_sm_P_dat_SC_se <- foreach(i = 1:length(temp_sm_P_dat_se)) %do% {
  temp_sm_P_dat_se[[i]][
    temp_sm_P_dat_se[[i]]$comid %in% streamcat_subset_tp[[i]]$comid,
  ]
}

temp_sm_P_dat_other_se <- foreach(i = 1:length(temp_sm_P_dat_se)) %do% {
  temp_sm_P_dat_se[[i]][
    !(temp_sm_P_dat_se[[i]]$comid %in% streamcat_subset_tp[[i]]$comid),
  ]
}

## Format transfer coefficients -----
if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  sm_N_dat_se <- foreach(i = 1:length(temp_sm_N_dat_SC_se)) %do% {
    if (!is.null(temp_sm_N_dat_SC_se[[i]]) && nrow(temp_sm_N_dat_SC_se[[i]]) > 0) {
      temp_sm_N_dat_opt <- temp_sm_N_dat_SC_se[[i]]
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
        select(c('comid_form', season, year, 'coeff'))
    } else {
      NULL
    }
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  sm_P_dat_se <- foreach(i = 1:length(temp_sm_P_dat_SC_se)) %do% {
    if (!is.null(temp_sm_P_dat_SC_se[[i]]) && nrow(temp_sm_P_dat_SC_se[[i]]) > 0) {
      temp_sm_P_dat_opt <- temp_sm_P_dat_SC[[i]]
      temp_sm_P_dat_opt$comid_form <- paste0(
        "'", temp_sm_P_dat_opt$comid, "'"
      )
      temp_sm_P_dat_opt %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(c('comid_form', season, year, 'coeff'))
    } else {
      NULL  # Skip empty data frames
    }
  }
}


### V. Delivery fractions ------

# Adjust delivery fraction to terminal COMID

##### a. Propagate error in delivery fraction ------
### Adjust delivery fraction to pore point
invisible(
max_delfrac_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% watershed_comid$catchment_comid)  %>% 
    select(c("comid", "year", "season", "DEL_FRAC")) %>% 
    summarize(DEL_FRAC = max(DEL_FRAC))
  
}
)
max_se_delfrac_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% watershed_comid$catchment_comid)  %>% 
    select(c("comid", "year", "season", "DEL_FRAC")) %>%
    mutate(SE_DEL_FRAC = 0) %>% #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
    summarize(SE_DEL_FRAC = mean(SE_DEL_FRAC, na.rm = T))
  
}

temp_delfrac_rev_tn <- foreach(i = 1:length(target_selection)) %do% { 
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% watershed_comid$catchment_comid)  %>% 
    select(c("comid", "year", "season", "DEL_FRAC")) %>%
    mutate(SE_DEL_FRAC = 0) #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
  
}

invisible(
  foreach(i = 1:length(target_selection)) %do% {
    
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], DEL_FRAC / max_delfrac_tn[[i]]$DEL_FRAC
    )
    temp_delfrac_rev_tn[[i]]$se_delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]],
      (DEL_FRAC / max_se_delfrac_tn[[i]]$SE_DEL_FRAC) *
        sqrt(
          (SE_DEL_FRAC / DEL_FRAC) ^ 2 +
            (max_se_delfrac_tn[[i]]$SE_DEL_FRAC / max_delfrac_tn[[i]]$DEL_FRAC) ^ 2
        )
    )
    
  }
)

delfrac_rev_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  temp_delfrac_rev_tn[[i]] %>%
    select(c("comid", "year", "season", "delfrac_rev", "se_delfrac_rev")) %>%
    mutate(se_delfrac_rev = case_when(is.na(se_delfrac_rev) ~ 0, T ~ se_delfrac_rev))
  
}

max_delfrac_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  if (nrow(watershed_comid) > 0) {
    
    sparrow_cons_out_tp %>% 
      filter(comid %in% watershed_comid$catchment_comid)  %>% 
      select(c("comid", "year", "season", "DEL_FRAC")) %>%
      mutate(SE_DEL_FRAC = 0) %>% #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
      summarize(DEL_FRAC = max(DEL_FRAC, na.rm = T))
    
  }
  
  else {
    NULL
  }
  
}

max_se_delfrac_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  if (nrow(watershed_comid) > 0) {
    
    sparrow_cons_out_tp %>% 
      filter(comid %in% watershed_comid$catchment_comid)  %>% 
      select(c("comid", "year", "season", "DEL_FRAC")) %>%
      mutate(SE_DEL_FRAC = 0) %>% #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
      summarize(SE_DEL_FRAC = mean(SE_DEL_FRAC, na.rm = T))
    
  }
  
  else {
    NULL
  }
  
}


temp_delfrac_rev_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tp %>% 
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(c("comid", "year", "season", "DEL_FRAC")) %>%
    mutate(SE_DEL_FRAC = 0) #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
  
}

invisible(
  foreach(i = 1:length(target_selection)) %do% {
    
    if(nrow(temp_delfrac_rev_tp[[i]]) > 0) {
      
      temp_delfrac_rev_tp[[i]]$delfrac_rev <- with(
        temp_delfrac_rev_tp[[i]], DEL_FRAC / max_delfrac_tp[[i]]$DEL_FRAC
      )
      temp_delfrac_rev_tp[[i]]$se_delfrac_rev <- with(
        temp_delfrac_rev_tp[[i]],
        (DEL_FRAC / max_se_delfrac_tp[[i]]$SE_DEL_FRAC) *
          sqrt(
            (SE_DEL_FRAC / DEL_FRAC) ^ 2 +
              (max_se_delfrac_tp[[i]]$SE_DEL_FRAC / max_delfrac_tp[[i]]$DEL_FRAC) ^ 2
          )
      )
    }
    
  }
)

delfrac_rev_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  if(nrow(temp_delfrac_rev_tp[[i]]) > 0) {
    
    temp_delfrac_rev_tp[[i]] %>% select(c("comid", "year", "season", "delfrac_rev", "se_delfrac_rev")) %>%
      mutate(season = as.numeric(season)) %>%
      mutate(se_delfrac_rev = case_when(is.na(se_delfrac_rev) ~ 0, T ~ se_delfrac_rev))
    
  }
  
  else {
    NULL
  }
  
}


##### b. Uncertainty in delivered baseloads ------

# Multiply all loads by revised del_frac 

temp_inc_tn_dat <- foreach(i = 1:length(delfrac_rev_tn)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_inc_tn_dat_tmp <- merge(
    inc_tn_rev[inc_tn_rev$comid %in% watershed_comid$catchment_comid,],
    delfrac_rev_tn[[i]],
    by = c("comid", "season", "year"),
    all.x = TRUE
  )
  
  
  temp_inc_tn_dat_tmp[c("in_poin","in_urb","in_road","in_ag","in_graz", "in_other","in_septic", "in_storage")] <- # CTC edited: 2/6/2026
    temp_inc_tn_dat_tmp[c("in_poin","in_urb","in_road","in_ag","in_graz", "in_other","in_septic", "in_storage")] *
    temp_inc_tn_dat_tmp[["delfrac_rev"]]
  
  #*# SE Create new columns for the standard errors ----------------------------
  temp_inc_tn_dat_tmp[c("sin_poin","sin_urb","sin_road","sin_ag","sin_graz", "sin_other","sin_septic", "sin_storage")] <- # CTC edited: 2/6/2026
    temp_inc_tn_dat_tmp[c("in_poin","in_urb","in_road","in_ag","in_graz", "in_other","in_septic", "in_storage")] *
    temp_inc_tn_dat_tmp[["delfrac_rev"]] *
    sqrt(
      (temp_inc_tn_dat_tmp[c("sin_poin","sin_urb","sin_road","sin_ag","sin_graz", "sin_other", "sin_septic", "sin_storage")] / temp_inc_tn_dat_tmp[c("in_poin","in_urb","in_road","in_ag","in_graz", "in_other","in_septic","in_storage")]) ^ 2 + # CTC edited: 2/6/2026
        (
          temp_inc_tn_dat_tmp[["se_delfrac_rev"]] / 
            temp_inc_tn_dat_tmp[["delfrac_rev"]]
        ) ^ 2
    )
  
  temp_inc_tn_dat_tmp <- temp_inc_tn_dat_tmp[
    order(temp_inc_tn_dat_tmp$comid), 
  ]
  
  temp_inc_tn_dat_tmp
  
}

temp_inc_tp_dat <- foreach(i = 1:length(delfrac_rev_tp)) %do% {
  
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  if (nrow(watershed_comid) > 0) {
    temp_inc_tp_dat_tmp <- merge(
      inc_tp[inc_tp$comid %in% watershed_comid$catchment_comid, ],
      delfrac_rev_tp[[i]],
      by = c("comid", "season", "year"),
      all.x = TRUE
    )
    #* # SE: removed septic loads for now to get running code
    temp_inc_tp_dat_tmp[c("ip_poin", "ip_urb", "ip_road", "ip_ag", "ip_graz", "ip_other", "ip_storage")] <-
      temp_inc_tp_dat_tmp[c("ip_poin", "ip_urb", "ip_road", "ip_ag", "ip_graz", "ip_other", "ip_storage")] *
      temp_inc_tp_dat_tmp[["delfrac_rev"]]
    
    temp_inc_tp_dat_tmp[c("sip_poin", "sip_urb", "sip_road", "sip_ag", "sip_graz", "sip_other", "sip_storage")] <-
      temp_inc_tp_dat_tmp[c("ip_poin", "ip_urb", "ip_road", "ip_ag", "ip_graz", "ip_other", "ip_storage")] *
      temp_inc_tp_dat_tmp[["delfrac_rev"]] *
      sqrt(
        (temp_inc_tp_dat_tmp[c("sip_poin", "sip_urb", "sip_road", "sip_ag", "sip_graz", "sip_other", "sip_storage")] / temp_inc_tp_dat_tmp[c("ip_poin", "ip_urb", "ip_road", "ip_ag", "ip_graz", "ip_other", "ip_storage")])^2 +
          (
            temp_inc_tp_dat_tmp[["se_delfrac_rev"]] /
              temp_inc_tp_dat_tmp[["delfrac_rev"]]
          )^2
      )
    
    temp_inc_tp_dat_tmp <- temp_inc_tp_dat_tmp[
      order(temp_inc_tp_dat_tmp$comid),
    ]
    
    temp_inc_tp_dat_tmp
  }
  
}


### VI. Uncertainty in runoff coefficients ------

# Calculate runoff coefficient for urban area

# Specify column for impervious dataset

temp_runoffcoeff <- imperv %>%
  select(c("comid", "PctImp2011Cat")) %>%
  mutate(cv_numpixels_nlcd = 0.17) %>%
  #@# Note: Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.) The PctImp2011Cat came from NLCD, possibly the impervious dataset, not the larger categorical one. I'm assuming the accuracy is the same regardless. https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/Documentation/Metadata/ImperviousSurfaces2016.html
  mutate(PctImp2011Cat_se = PctImp2011Cat * cv_numpixels_nlcd) %>%
  as.data.frame()


temp_runoffcoeff$runoffcoeff <- with(
  temp_runoffcoeff, 0.05 + 0.009 * PctImp2011Cat
) #*#

#@# Note: This equation comes from Schueler 1987, Table A6 & Figure 1-2. 
Schueler.table <- data.frame(
  PercentImpervious = c(
    41, 38, 24, 33, 33, 19, 29, 29, 76, 20, 22, 38, 29, 50, 57, 21, 18, 37, 37, 22, 17, 27, 21, 34, 58, 81, 23, 5, 6, 69, 99, 91, 69, 21, 99, 90, 4, 1, 11, 7, 55, 34, 90, 22
  ),
  Mean = c(
    0.35, 0.18, 0.16, 0.46, 0.25, 0.19, 0.47, 0.24, 0.56, 0.24, 0.17, 0.22, 0.2, 0.37, 0.43, 0.17, 0.18, 0.24, 0.37, 0.99, 0.19, 0.11, 0.26, 0.28, 0.73, 0.82, 0.28, 0.11, 0.02, 0.65, 0.98, 0.99, 0.9, 0.21, 0.92, 0.74, 0.08, 0.12, 0.05, 0.08, 0.57, 0.47, 0.75, 0.42
  )
)

temp_runoffcoeff$runoffcoeff_se <- (predict(
  with(Schueler.table, lm(Mean ~ PercentImpervious)),
  newdata = data.frame(PercentImpervious = temp_runoffcoeff$PctImp2011Cat),
  se.fit = TRUE
)$se.fit) / sqrt(nrow(Schueler.table))

runoffcoeff <- temp_runoffcoeff %>%
  select(c("comid", "runoffcoeff", "runoffcoeff_se"))
runoffcoeff <- distinct(runoffcoeff) # Removes duplicates

### VII. Format loads and coefficients ------

##### a. Format baseline loading se -----
if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  inc_tn_dat_se <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
    if (!is.null(temp_inc_tn_dat[[i]]) && nrow(temp_inc_tn_dat[[i]]) > 0) {
      temp_inc_tn_dat_opt <- temp_inc_tn_dat[[i]]
      temp_inc_tn_dat_opt$comid_form <- paste0(
        "'", temp_inc_tn_dat_opt$comid, "'"
      )
      temp_inc_tn_dat_opt %>% 
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(c("comid_form", "season", "year", "sin_poin","sin_urb","sin_road","sin_ag","sin_septic","sin_graz", "sin_other","sin_storage")) %>%
        mutate(across(.cols = -comid_form, .fns = ~replace_na(., 0)))
    }
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  inc_tp_dat_se <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
    if (!is.null(temp_inc_tp_dat[[i]]) && nrow(temp_inc_tp_dat[[i]]) > 0) {
      temp_inc_tp_dat_opt <- as.data.frame(temp_inc_tp_dat[[i]])
      temp_inc_tp_dat_opt$comid_form <- paste0("'", temp_inc_tp_dat_opt$comid, "'")
      temp_inc_tp_dat_opt %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(c("comid_form", "season", "year", "sip_poin", "sip_urb", "sip_road", "sip_ag", "sip_graz", "sip_other", "sip_storage")) %>%
        mutate(across(.cols = -comid_form, .fns = ~replace_na(., 0)))
    }
  }
}

##### b. Format runoff coefficient -----

temp_runoffcoeff_dat <- runoffcoeff[
  runoffcoeff$comid %in% streamcat_subset_all$comid,
]
temp_runoffcoeff_dat <- temp_runoffcoeff_dat[order(temp_runoffcoeff_dat$comid),]
temp_runoffcoeff_dat$comid_form <- paste0("'", temp_runoffcoeff_dat$comid, "'")
runoffcoeff_dat_se <- temp_runoffcoeff_dat %>% 
  select(c("comid_form", "runoffcoeff_se"))


### IX. Riparian Buffers ------

##### a. Uncertainty in riparian loadings -----
riparian.loadings_tn <- foreach(i = seq_along(target_selection)) %do% {
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  riparian.loadings %>%
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, season, year, N_riparian_kgyr, N_riparian_kgyr_se) %>%
    right_join(., temp_inc_tn_dat[[i]] %>%
                 filter(comid %in% watershed_comid$catchment_comid) %>%
                 select(comid, season, year, in_total, sin_total), by = c("comid", "season", "year")) %>%
    mutate(
      N_riparian_kgyr = case_when(
        N_riparian_kgyr > in_total ~ in_total,
        is.na(N_riparian_kgyr) ~ 0,
        TRUE ~ N_riparian_kgyr
      ),
      N_riparian_kgyr_se = case_when(
        N_riparian_kgyr > in_total ~ sin_total,
        is.na(N_riparian_kgyr_se) ~ 0,
        TRUE ~ N_riparian_kgyr_se
      )
    ) %>% 
    select(-c(in_total, sin_total))
}

riparian.loadings_tp <- foreach(i = seq_along(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  if(nrow(watershed_comid) > 0 & !is.null(watershed_comid)) {
    riparian.loadings %>%
      filter(comid %in% watershed_comid$catchment_comid) %>%
      select(comid, season, year, P_riparian_kgyr, P_riparian_kgyr_se) %>%
      right_join(., temp_inc_tp_dat[[i]] %>%
                   mutate(season = as.numeric(season)) %>%
                   filter(comid %in% watershed_comid$catchment_comid) %>%
                   select(comid, season, year, ip_total, sip_total), by = c("comid", "season", "year")) %>%
      mutate(
        P_riparian_kgyr = case_when(
          P_riparian_kgyr > ip_total ~ ip_total,
          is.na(P_riparian_kgyr) ~ 0,
          TRUE ~ P_riparian_kgyr
        ),
        P_riparian_kgyr_se = case_when(
          P_riparian_kgyr > ip_total ~ sip_total,
          is.na(P_riparian_kgyr_se) ~ 0,
          TRUE ~ P_riparian_kgyr_se
        )
      ) %>% 
      select(-ip_total, -sip_total)
  }
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  
  temp_riparian_tn_dat <- foreach(i = seq_along(target_selection)) %do% {
    
    temp_riparian_tn_dat_tmp <- merge(
      riparian.loadings_tn[[i]],
      delfrac_rev_tn[[i]],
      by = c("comid", "season", "year"),
      all = TRUE
    ) %>%
      rowwise() %>%
      mutate(
        in_riparian = N_riparian_kgyr * delfrac_rev,
        sin_riparian = case_when(
          N_riparian_kgyr > 0 & delfrac_rev > 0 ~ my_propogateerror(
            vals = list(c(N_riparian_kgyr, N_riparian_kgyr_se), c(delfrac_rev, se_delfrac_rev)), 
            method = "mult"
          ),
          N_riparian_kgyr <= 0 | delfrac_rev <= 0 ~ 0
        )
      ) %>%
      arrange(comid, season) %>%
      select(comid, season, year, in_riparian, sin_riparian) %>%
      ungroup()
    
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  temp_riparian_tp_dat <- foreach(i = seq_along(target_selection)) %do% {
    
    if(!is.null(delfrac_rev_tp[[i]]) && nrow(delfrac_rev_tp[[i]]) > 0 && 
       !is.null(riparian.loadings_tp[[i]]) && nrow(riparian.loadings_tp[[i]]) > 0) {
      temp_riparian_tp_dat_tmp <- merge(
        riparian.loadings_tp[[i]],
        delfrac_rev_tp[[i]],
        by = c("comid", "season", "year"),
        all = TRUE
      ) %>%
        rowwise() %>%
        mutate(
          ip_riparian = P_riparian_kgyr * delfrac_rev,
          sip_riparian = case_when(
            P_riparian_kgyr > 0 & delfrac_rev > 0 ~ my_propogateerror(
              vals = list(c(P_riparian_kgyr, P_riparian_kgyr_se), c(delfrac_rev, se_delfrac_rev)), 
              method = "mult"
            ),
            P_riparian_kgyr <= 0 | delfrac_rev <= 0 ~ 0
          )
        ) %>%
        arrange(comid, season) %>%
        select(comid, season, year, ip_riparian, sip_riparian) %>%
        ungroup()
    }
  }
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  riparian_tn_dat <- foreach(i = 1:length(temp_riparian_tn_dat)) %do% {
    
    temp_riparian_tn_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      merge(., year_xwalk, by = "year") %>%
      select(-year) %>%
      rename(year = model_year) %>%
      select(comid_form, season, year, in_riparian, sin_riparian)
    
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  riparian_tp_dat <- foreach(i = 1:length(temp_riparian_tp_dat)) %do% {
    
    if (!is.null(temp_riparian_tp_dat[[i]]) && nrow(temp_riparian_tp_dat[[i]]) > 0 ) {
      temp_riparian_tp_dat[[i]] %>%
        filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        merge(., year_xwalk, by = "year") %>%
        select(-year) %>%
        rename(year = model_year) %>%
        select(comid_form, season, year, ip_riparian, sip_riparian)
    } 
  }
}

##### b. Updated riparian buffer removal efficiencies with uncertainty -----
if(length(RiparianBuffer_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
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
          filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
          select(
            -contains("P_"),
            -!contains(short.form.bmp.name),
            -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
            -!contains("uncertainty"),
            comid
          ) %>%
          rename_with(.fn = ~"Curve.Form", .cols = contains("N")) %>%
          mutate(
            x = RiparianBuffer_Widths[j],
            expression = strsplit(
              str_extract(
                mapply(
                  gsub,
                  pattern = "x",
                  replacement = x,
                  x = mapply(
                    gsub,
                    pattern = "'",
                    replacement = "",
                    x = Curve.Form
                  )
                ),
                pattern = "(?<=c[(]).*(?=[)])"
              ),
              ","
            )
          ) %>%
          rowwise() %>%
          mutate(
            iter = 1,
            effic = list(
              lapply(X = lapply(X = expression, FUN = my_parse), FUN = eval)
            ),
            effic_mean = suppressWarnings(mean(unlist(effic))),
            effic_sd = sd(unlist(effic))
          ) %>%
          select(comid, effic_mean, effic_sd) %>%
          rename_with(
            .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
            .cols = contains("effic")
          ) %>%
          ungroup()
      }
      
      riparian_buffer_efficiencies_N_tmp %>%
        mutate(
          across(
            .cols = contains(RiparianBuffer_BMPs),
            .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
          
        )
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
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
          filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
          select(
            -contains("N_"),
            -!contains(short.form.bmp.name),
            -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
            -!contains("uncertainty"),
            comid
          ) %>%
          rename_with(.fn = ~"Curve.Form", .cols = contains("P")) %>%
          mutate(
            x = RiparianBuffer_Widths[j],
            expression = strsplit(
              str_extract(
                mapply(
                  gsub,
                  pattern = "x", 
                  replacement = x,
                  x = mapply(
                    gsub,
                    pattern = "'", 
                    replacement = "", 
                    x = Curve.Form
                  )
                ), 
                pattern = "(?<=c[(]).*(?=[)])"
              ), 
              ","
            )
          ) %>%
          rowwise() %>%
          mutate(
            iter = 1,
            effic = list(
              lapply(X = lapply(X = expression, FUN = my_parse), FUN = eval)
            ),
            effic_mean = suppressWarnings(mean(unlist(effic))),
            effic_sd = sd(unlist(effic))
          ) %>%
          select(comid, effic_mean, effic_sd) %>%
          rename_with(
            .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
            .cols = contains("effic")
          ) %>%
          ungroup()
      }
      
      riparian_buffer_efficiencies_P_tmp %>%
        mutate(
          across(
            .cols = contains(RiparianBuffer_BMPs),
            .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
          
        )
    }
  }
  
  invisible(
    foreach(i = 1:length(riparian_buffer_efficiencies_N)) %do% {
      riparian_buffer_efficiencies_N[[i]] <- riparian_buffer_efficiencies_N[[i]] %>%
        mutate(across(contains("sd"), list(se = ~ . / sqrt(10)))) %>%
        select(
          comid, contains(c("Grassed_Buffer", "Forested_Buffer"))
        ) %>%
        rename_with(
          .fn = ~gsub(., pattern = "sd_se", replacement = "se"), 
          .cols = contains("sd_se")
        )
    }
  )
  
  
  invisible(
    foreach(i = 1:length(riparian_buffer_efficiencies_P)) %do% {
      riparian_buffer_efficiencies_P[[i]] <- riparian_buffer_efficiencies_P[[i]] %>%
        mutate(across(contains("sd"), list(se = ~ . / sqrt(10)))) %>%
        select(
          comid, contains(c("Grassed_Buffer", "Forested_Buffer"))
        ) %>%
        rename_with(
          .fn = ~gsub(., pattern = "sd_se", replacement = "se"), 
          .cols = contains("sd_se")
        )
    }
  )
  
  ## uncertainty in loading per bankft ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_loadingperbankft_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      tmp1 <- left_join(
        riparian.loadings_tn[[i]], riparian.existingbuffer, by = "comid"
      ) %>%
        select(
          comid, 
          season,
          year,
          N_riparian_kgyr, 
          N_riparian_kgyr_se, 
          totalbanklength_ft, 
          totalbanklength_ft_se
        ) %>%
        filter(!is.na(totalbanklength_ft)) %>%
        rowwise() %>%
        mutate(
          loading_per_bankft_kg_ftyr = N_riparian_kgyr / totalbanklength_ft,
          loading_per_bankft_kg_ftyr_se = if(
            N_riparian_kgyr <= 0
          ) {0} else {
            my_propogateerror(
              vals = list(
                c(N_riparian_kgyr, N_riparian_kgyr_se), 
                c(totalbanklength_ft, totalbanklength_ft_se)
              ), 
              method = "div"
            )
          }
        ) %>%
        ungroup()
      tmp1
      
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_loadingperbankft_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      if (length(riparian.loadings_tp[[i]]) > 0) { 
        tmp1 <- left_join(
          riparian.loadings_tp[[i]], riparian.existingbuffer, by = "comid"
        ) %>%
          select(
            comid, 
            season,
            year,
            P_riparian_kgyr, 
            P_riparian_kgyr_se, 
            totalbanklength_ft, 
            totalbanklength_ft_se
          ) %>%
          filter(!is.na(totalbanklength_ft)) %>%
          rowwise() %>%
          mutate(
            loading_per_bankft_kg_ftyr = P_riparian_kgyr / totalbanklength_ft,
            loading_per_bankft_kg_ftyr_se = if(
              P_riparian_kgyr == 0
            ) {0} else {
              my_propogateerror(
                vals = list(
                  c(P_riparian_kgyr, P_riparian_kgyr_se), 
                  c(totalbanklength_ft, totalbanklength_ft_se)
                ), 
                method = "div"
              )
            }
          ) %>%
          ungroup()
        
        tmp1
      }
      
    }
  }
  
  ## riparianbufferremoval un-revised uncertainty ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_removal_N_tmp <- foreach(i = 1:length(riparian_loadingperbankft_N)) %do% {
      
      tmp2 <- riparian_loadingperbankft_N[[i]]%>%
        left_join(., riparian_buffer_efficiencies_N[[i]], by = "comid") %>%
        pivot_longer(
          cols = contains(RiparianBuffer_BMPs), 
          names_to = c("RiparianBMP", NA, "ValueType"), 
          names_sep = "_",
          values_to = "Value"
        ) %>%
        pivot_wider(
          id_cols = c(
            "comid",
            "season",
            "year",
            "RiparianBMP", 
            'loading_per_bankft_kg_ftyr', 
            "loading_per_bankft_kg_ftyr_se"
          ), 
          names_from = "ValueType", 
          values_from = "Value"
        ) %>%
        mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
        rowwise() %>%
        mutate(
          removal = case_when(
            effic == -999 ~ -999, effic >= 0 ~ (effic  * loading_per_bankft_kg_ftyr)
          ),
          removal_se = case_when(
            removal == -999 ~ 0, 
            removal >= 0 ~ if(
              loading_per_bankft_kg_ftyr == 0 | effic == 0
            ) {0} else {
              my_propogateerror(
                vals = list(
                  c(effic, se), 
                  c(loading_per_bankft_kg_ftyr, loading_per_bankft_kg_ftyr_se)
                ),
                method = "mult"
              )
            }
          )
        ) %>%
        ungroup()
      
      tmp2 
      
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_removal_P_tmp <- foreach(i = 1:length(riparian_loadingperbankft_P)) %do% {
      
      if (length(riparian_loadingperbankft_P[[i]]) > 0) { 
        
        tmp2 <- riparian_loadingperbankft_P[[i]]%>%
          left_join(., riparian_buffer_efficiencies_P[[i]], by = "comid") %>%
          pivot_longer(
            cols = contains(RiparianBuffer_BMPs), 
            names_to = c("RiparianBMP", NA, "ValueType"), 
            names_sep = "_",
            values_to = "Value"
          ) %>%
          pivot_wider(
            id_cols = c(
              "comid",
              "season",
              "year",
              "RiparianBMP", 
              'loading_per_bankft_kg_ftyr', 
              "loading_per_bankft_kg_ftyr_se"
            ), 
            names_from = "ValueType", 
            values_from = "Value"
          ) %>%
          mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
          rowwise() %>%
          mutate(
            removal = case_when(
              effic == -999 ~ -999, effic >= 0 ~ (effic  * loading_per_bankft_kg_ftyr)
            ),
            removal_se = case_when(
              removal == -999 ~ 0, 
              removal >= 0 ~ if(
                loading_per_bankft_kg_ftyr == 0 | effic == 0
              ) {0} else {
                my_propogateerror(
                  vals = list(
                    c(effic, se), 
                    c(loading_per_bankft_kg_ftyr, loading_per_bankft_kg_ftyr_se)
                  ),
                  method = "mult"
                )
              }
            )
          ) %>%
          ungroup()
        
        tmp2 
      }
      
    }
  }
  
  ## riparian buffer removal ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_removal_N <- foreach(
      i = 1:length(riparian_buffer_removal_N_tmp)
    ) %do% {
      riparian_buffer_removal_N_tmp[[i]] %>%
        pivot_wider(
          id_cols = c("comid", "season", "year"), 
          names_from = "RiparianBMP", 
          values_from = c("removal", "removal_se")
        )%>%
        rename_with(
          .fn = ~gsub(., pattern = "removal_se", replacement = "se"), 
          .cols = contains("removal_se")
        )
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_removal_P <- foreach(
      i = 1:length(riparian_buffer_removal_P_tmp)
    ) %do% {
      if (length(riparian_buffer_removal_P_tmp[[i]]) > 0) {
        riparian_buffer_removal_P_tmp[[i]] %>%
          pivot_wider(
            id_cols = c("comid", "season", "year"), 
            names_from = "RiparianBMP", 
            values_from = c("removal", "removal_se")
          )%>%
          rename_with(
            .fn = ~gsub(., pattern = "removal_se", replacement = "se"), 
            .cols = contains("removal_se")
          )
      } 
      
    }
  }
  
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  riparian_tn_removal_se <- foreach(i = 1:length(riparian_buffer_removal_N)) %do% {
    
    # Create complete structure with all COMIDs, seasons, and years
    complete_riparian_tn_se <- expand.grid(
      comid = streamcat_subset_tn[[i]]$comid,
      season = unique(sparrow_in_DYN$season),
      year = unique(year_xwalk$model_year),
      stringsAsFactors = FALSE
    )
    
    tmp1 <- riparian_buffer_removal_N[[i]] %>%
      merge(., year_xwalk, by = "year") %>%
      select(-year) %>%
      rename(year = model_year) %>%
      merge(complete_riparian_tn_se, by = c("comid", "season", "year"), all.y = TRUE) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, season, year, contains("se_")) %>%
      rename_with(.fn = ~gsub("se_", "", .), .cols = -c(comid_form, season, year)) %>%
      rename_with(.fn = ~paste0(., "_Buffer"), .cols = -c(comid_form, season, year)) %>%
      mutate(across(.cols = any_of(RiparianBuffer_BMPs), .fns = ~case_when(is.na(.) ~ 0, . == -999 ~ 0, . >= 0 ~ .)))
    
    tmp1
  }
    
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_tp_removal_se <- foreach(i = 1:length(riparian_buffer_removal_P)) %do% {
      if (length(riparian_buffer_removal_P[[i]]) > 0) {
        # Create complete structure with all COMIDs, seasons, and years
        complete_riparian_tp_se <- expand.grid(
          comid = streamcat_subset_tp[[i]]$comid,
          season = unique(sparrow_in_DYN$season),
          year = unique(year_xwalk$model_year),
          stringsAsFactors = FALSE
        )
        
        tmp1 <- riparian_buffer_removal_P[[i]] %>%
          merge(., year_xwalk, by = "year") %>%
          select(-year) %>%
          rename(year = model_year) %>%
          merge(complete_riparian_tp_se, by = c("comid", "season", "year"), all.y = TRUE) %>%
          mutate(comid_form = paste0("'", comid, "'")) %>%
          select(comid_form, season, year, contains("se_")) %>%
          rename_with(.fn = ~gsub("se_", "", .), .cols = -c(comid_form, season, year)) %>%
          rename_with(.fn = ~paste0(., "_Buffer"), .cols = -c(comid_form, season, year)) %>%
          mutate(across(any_of(RiparianBuffer_BMPs), ~case_when(is.na(.) ~ 0, . == -999 ~ 0, . >= 0 ~ .)))
        tmp1
      }
    }
  }
}


##### e.Point Source BMPs effic ------
if(length(Point_BMPs) > 0) {
  temp_point_effic <- fread(
    paste(InPath, "WWTP_RemovalEffic.csv", sep = "")
  ) #*#
  
  temp_bmp_effic_point_se <- merge(
    temp_point_effic, 
    point_comid,
    by = "COMID",
    all.x = TRUE
  ) %>%
    select(COMID, State, Plant_Name, NPDES_ID, contains("_se")) %>%
    pivot_longer(
      cols = -c(COMID, State, Plant_Name, NPDES_ID), 
      names_to = c("BMP", "Nutrient"), 
      names_pattern = "(.*)_(TN|TP)_Efficiency",
      values_to = "Efficiency_se"
    )
  
  temp_bmp_effic_point_selected <- temp_bmp_effic_point_se %>% 
    slice(
      which(
        unlist(
          map(
            .x = str_split(temp_bmp_effic_point_se$BMP, "_"), 
            .f = ~(all(.x %in% UserSelected_pointBMPs$BMP)))
        )
      )
    ) 
  
  point_effic_se <- temp_bmp_effic_point_selected %>%
    mutate(across(contains("Efficiency_se"), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
  
  point_effic_bycomid_tn_se <- point_effic_se %>% 
    filter(Nutrient == "TN") %>%
    select(comid = COMID, BMP, effic_se = Efficiency_se) #%>%
    # mutate(comid_form = paste0("'", comid, "'")) %>%
    # select(comid_form, BMP, effic_se) %>%
    # pivot_wider(., id_cols = comid_form, names_from = BMP, values_from = effic_se)
  
  point_effic_bycomid_tp_se <- point_effic_se %>% 
    filter(Nutrient == "TP") %>%
    select(comid = COMID, BMP, effic_se = Efficiency_se) #%>%
    # mutate(comid_form = paste0("'", comid, "'")) %>%
    # select(comid_form, BMP, effic_se) %>%
    # pivot_wider(., id_cols = comid_form, names_from = BMP, values_from = effic_se)
  
} else {
  point_effic_bycomid_tn_se <- data.frame(
    comid = NA, BMP = NA, effic_se = NA
  )
  point_effic_bycomid_tp_se <- data.frame(
    comid = NA, BMP = NA, effic_se = NA
  )
}

# Format point source BMP efficiency data

if(length(Point_BMPs) > 0) {
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    temp_point_effic_dat_tn_se <- point_effic_bycomid_tn_se %>%
      filter(comid %in% unlist(streamcat_subset_tn)) %>% 
      pivot_wider(id_cols = comid, names_from = BMP, values_from = effic_se) %>%
      select(comid, any_of(Point_BMPs)) # forces the order
    
    temp_point_effic_other_tn_se <- data.frame(
      comid = unlist(
        streamcat_subset_tn, use.names = FALSE
      )[
        !(
          unlist(streamcat_subset_tn, use.names = FALSE) %in% 
            temp_point_effic_dat_tn_se$comid
        )
      ]
    )
    
    temp_point_effic_other_tn_se[2:(1+length(Point_BMPs))] <- 0
    names(temp_point_effic_other_tn_se) <- c("comid", Point_BMPs)
    
    if(nrow(temp_point_effic_dat_tn_se) > 0) {
      temp_point_effic_dat_rev_tn_se <- rbind(
        temp_point_effic_dat_tn_se, temp_point_effic_other_tn_se
      ) %>% 
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    } else {
      temp_point_effic_dat_rev_tn_se <- temp_point_effic_other_tn_se %>% 
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    }
    
    point_effic_dat_tn_se <- temp_point_effic_dat_rev_tn_se %>% 
      select(comid_form, all_of(Point_BMPs)) %>%
      unique() # function used to ensure duplicates don't creep in
  }
  
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    
    temp_point_effic_dat_tp_se <- point_effic_bycomid_tp_se %>%
      filter(comid %in% unlist(streamcat_subset_tp)) %>% 
      pivot_wider(id_cols = comid, names_from = BMP, values_from = effic_se) %>%
      select(comid, any_of(Point_BMPs)) # forces the order
    
    temp_point_effic_other_tp_se <- data.frame(
      comid = unlist(
        streamcat_subset_tp, use.names = FALSE
      )[
        !(
          unlist(streamcat_subset_tp, use.names = FALSE) %in% 
            temp_point_effic_dat_tp_se$comid
        )
      ]
    )
    
    temp_point_effic_other_tp_se[2:(1+length(Point_BMPs))] <- 0
    names(temp_point_effic_other_tp_se) <- c("comid", Point_BMPs)
    
    if(nrow(temp_point_effic_dat_tp_se) > 0) {
      temp_point_effic_dat_rev_tp_se <- rbind(
        temp_point_effic_dat_tp_se, temp_point_effic_other_tp_se
      ) %>% 
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    } else {
      temp_point_effic_dat_rev_tp_se <- temp_point_effic_other_tp_se %>% 
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    }
    
    point_effic_dat_tp_se <- temp_point_effic_dat_rev_tp_se %>% 
      select(comid_form, all_of(Point_BMPs)) %>%
      unique() # function used to ensure duplicates don't creep in
  }
}

### X. Area uncertainty -----

##### a. Urban area uncertainty -----

# Select urban area and incremental area from SPARROW input data

temp_sparrow_area <- sparrow_in_DYN %>%
  select(comid, IncAreaKm2, urb) %>%
  mutate(cv_numpixels_nlcd = 0.17) %>% #@# Note: Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)
  mutate(cv_km2_nlcd = cv_numpixels_nlcd) %>%
  mutate(urban_km2_se = IncAreaKm2 * (urb / 100) * cv_km2_nlcd) %>%
  mutate(urban_ac_se = urban_km2_se/km2_to_ac) 

cv_numpixels_nlcd <- 0.17 #@# Note: Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)

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

distinct_area <- as.data.frame(distinct_area) %>%
  mutate(across(urban_ac:graz_ac, ~ drop_units(.))) %>%
  mutate(urban_ac_se = urban_ac * cv_numpixels_nlcd,
         ag_ac_se = ag_ac_bmpadj * cv_numpixels_nlcd,
         graz_ac_se = graz_ac * cv_numpixels_nlcd)

area_dat <- distinct_area[distinct_area$comid %in% reaches_all$comid,]

area_dat_se <- area_dat %>%
  mutate(comid_form = paste0("'", comid, "'")) %>%
  select(comid_form, ag_ac = ag_ac_bmpadj, graz_ac, ag_ac_se, graz_ac_se)

# Calculate road area (percent impervious)
road_area_tmp <- road_area[road_area$comid %in% reaches_all$comid,]
road_area_se_temp <- road_area_tmp %>%
  select(comid, HSG, road_ac) %>%
  mutate(road_ac = drop_units(road_ac)) %>%
  distinct() %>%
  mutate(road_ac_se = road_ac * cv_numpixels_nlcd)

# Create complete structure with all HSG combinations
road_hsg_all <- unique(road_area_se_temp$HSG)

complete_road_se <- data.frame(
  comid = unique(reaches_all$comid)
) %>%
  crossing(., HSG = road_hsg_all)

# Merge with actual SE data and fill missing combinations with 0
road_area_se <- road_area_se_temp %>%
  merge(complete_road_se, by = c("comid", "HSG"), all = TRUE) %>%
  mutate(road_ac_se = case_when(is.na(road_ac_se) ~ 0, TRUE ~ road_ac_se)) %>%
  arrange(comid, HSG) %>%
  mutate(comid_form = paste0("'", comid, "'"),
         HSG = paste0("'", HSG, "'")) %>%
  select(comid_form, HSG, road_ac_se)


urban_areas_se <- urban_areas %>%
  mutate(urban_ac_se = urban_ac * cv_numpixels_nlcd)

temp_urban_area_se <- urban_areas_se[urban_areas_se$comid %in% streamcat_subset_all$comid,]

# Create complete structure with all urban land use and HSG combinations
urban_land_use_all <- unique(temp_urban_area_se$Land_Use)
urban_hsg_all <- unique(temp_urban_area_se$HSG)

complete_urban_se <- data.frame(
  comid = unique(streamcat_subset_all$comid)
) %>%
  crossing(., Land_Use = urban_land_use_all) %>%
  crossing(., HSG = urban_hsg_all)

# Merge with actual SE data and fill missing combinations with 0
temp_urban_area_se_full <- temp_urban_area_se %>%
  merge(complete_urban_se, by = c("comid", "Land_Use", "HSG"), all = TRUE) %>%
  mutate(urban_ac_se = case_when(is.na(urban_ac_se) ~ 0, TRUE ~ urban_ac_se)) %>%
  arrange(comid, Land_Use, HSG)

temp_urban_area_se_full <- temp_urban_area_se_full %>%
  mutate(comid_form = paste0("'", comid, "'"),
         HSG = paste0("'", HSG, "'"),
         Land_Use = paste0("'", Land_Use, "'"))

urban_area_dat_se <- temp_urban_area_se_full %>% select(comid_form, Land_Use, HSG, urban_ac_se)

### XI. Updating standard errors of costs -----

##### a. Find standard error of AG & riparian costs over years -----
ag_costs_yearly <- read.csv(file = paste0(InPath, "EQIPcosts_overyears.csv"))

names(ag_costs_yearly)[1] <- "BMP_Category"

ag_costs_yearly <- ag_costs_yearly %>%
  mutate(BMP = case_when(BMP == "Grade_Stabilize" ~ "Grade_Stabilization", TRUE ~ BMP))

bmp_costs_se <- ag_costs_yearly %>%
  pivot_longer(
    cols = -c(BMP_Category, BMP, capital_units, operations_units), 
    names_to = c("costtype", "State", "Year"), 
    names_sep = "_", 
    values_to = "Value"
  ) %>%
  group_by(
    BMP_Category, BMP, State, costtype, capital_units, operations_units
  ) %>%
  summarize(mean = mean(Value), se = (sd(Value) / sqrt(4)), .groups = "keep") %>%
  pivot_wider(
    id_cols = c(BMP_Category, BMP, capital_units, operations_units), 
    names_from = c(costtype, State), 
    values_from = se
  ) %>%
  ungroup()

## adjust ag & riparian units ####

temp_bmp_costs <- merge(
  user_specs_BMPs %>%
    filter(BMP_Selection == "X") %>%
    select(
      BMP_Category, BMP, contains(c("capital", "operations")), UserSpec_width_ft
    ),
  bmp_costs_se,
  by = c("BMP_Category", "BMP"),
  suffixes = c("_Estimate", "_se")
) %>%
  mutate(capital_WA_Estimate = as.numeric(capital_WA_Estimate), operations_WA_Estimate = as.numeric(operations_WA_Estimate))


# Convert area-specific costs to costs per acre for ag BMPs and to costs per linear foot for Riparian BMPs


temp_bmp_costs_rev <- temp_bmp_costs %>%
  mutate(
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~replace_na(., NA_real_)
    ),
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          capital_units_Estimate == "ft2" ~ . * ft2_to_ac,
          capital_units_Estimate == "km2" ~ . * km2_to_ac,
          capital_units_Estimate == "yd2" ~ . * yd2_to_ac,
          capital_units_Estimate == "ac" ~ .
        ),
        BMP_Category == "ag" ~ case_when(
          capital_units_se == "ft2" ~ . * ft2_to_ac,
          capital_units_se == "km2" ~ . * km2_to_ac,
          capital_units_se == "yd2" ~ . * yd2_to_ac,
          capital_units_se == "ac" ~ .
        ),
        BMP_Category == "graz" ~ case_when(
          capital_units_Estimate == "ft2" ~ . * ft2_to_ac,
          capital_units_Estimate == "km2" ~ . * km2_to_ac,
          capital_units_Estimate == "yd2" ~ . * yd2_to_ac,
          capital_units_Estimate == "ac" ~ .
        ),
        BMP_Category == "graz" ~ case_when(
          capital_units_se == "ft2" ~ . * ft2_to_ac,
          capital_units_se == "km2" ~ . * km2_to_ac,
          capital_units_se == "yd2" ~ . * yd2_to_ac,
          capital_units_se == "ac" ~ .
        ),
        BMP_Category == "ripbuf" ~ case_when(
          capital_units_Estimate == "ft2" ~ . * UserSpec_width_ft,
          capital_units_Estimate == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units_Estimate == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units_Estimate == "ac" ~ . / ft2_to_ac * UserSpec_width_ft
        ),
        BMP_Category == "ripbuf" ~ case_when(
          capital_units_se == "ft2" ~ . * UserSpec_width_ft,
          capital_units_se == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units_se == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units_se == "ac" ~ . / ft2_to_ac * UserSpec_width_ft
        ),
      ), 
      .names = "{.col}_rev"
    )
  ) %>%
  rename_with(., .fn = ~gsub("_Estimate", "", .), .cols = everything())


## annualize costs ####
bmp_costs <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev") & !contains("_se")
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

bmp_costs_se <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev") & contains("_se")
    )
  ) %>%
  rename(
    category = BMP_Category,
    bmp = BMP
  ) %>%
  rename_at(
    vars(contains(c("capital_", "operations_"))), list( ~ gsub("_se_rev", "", .))
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

## Separate parameters for ag_capital_se and ag_operations_se and format costs data ####


if(length(Ag_BMPs) > 0) {
  temp_bmp_costs_ag_se <- merge(
    bmp_costs_se %>%
      filter(category == "ag") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  )  %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))#*#
  
  if(
    any(is.na(temp_bmp_costs_ag_se$capital)) | 
    any(is.na(temp_bmp_costs_ag_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for ag ",
        if(
          any(is.na(temp_bmp_costs_ag_se$capital)) & 
          any(is.na(temp_bmp_costs_ag_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ag_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ag_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ag_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `EQIPcosts_overyears.csv`"
      )
    )
  }
  bmp_costs_ag_capital_se <- reshape2::dcast(
    temp_bmp_costs_ag_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ag_operations_se <- reshape2::dcast(
    temp_bmp_costs_ag_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  bmp_costs_ag_capital_rev_se <- bmp_costs_ag_capital_se[
    order(bmp_costs_ag_capital$comid),
  ]
  ag_costs_cap_dat_se <- bmp_costs_ag_capital_rev_se[
    ,names(bmp_costs_ag_capital_rev_se) != "comid"
  ]
  ag_costs_cap_dat_se <- ag_costs_cap_dat_se %>% select(comid_form, everything())
  
  bmp_costs_ag_operations_rev_se <- bmp_costs_ag_operations_se[
    order(bmp_costs_ag_operations_se$comid),
  ]
  ag_costs_op_dat_se <- bmp_costs_ag_operations_rev_se[ 
    ,names(bmp_costs_ag_operations_rev_se) != "comid"
  ]
  ag_costs_op_dat_se <- ag_costs_op_dat_se %>% select(comid_form, everything())
}

if(length(Graz_BMPs) > 0) {
  temp_bmp_costs_graz_se <- merge(
    bmp_costs_se %>%
      filter(category == "graz") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  )  %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))#*#
  
  if(
    any(is.na(temp_bmp_costs_graz_se$capital)) | 
    any(is.na(temp_bmp_costs_graz_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for graz ",
        if(
          any(is.na(temp_bmp_costs_graz_se$capital)) & 
          any(is.na(temp_bmp_costs_graz_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_graz_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_graz_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_graz_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `EQIPcosts_overyears.csv`"
      )
    )
  }
  bmp_costs_graz_capital_se <- reshape2::dcast(
    temp_bmp_costs_graz_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_graz_operations_se <- reshape2::dcast(
    temp_bmp_costs_graz_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  bmp_costs_graz_capital_rev_se <- bmp_costs_graz_capital_se[
    order(bmp_costs_graz_capital$comid),
  ]
  graz_costs_cap_dat_se <- bmp_costs_graz_capital_rev_se[
    ,names(bmp_costs_graz_capital_rev_se) != "comid"
  ]
  graz_costs_cap_dat_se <- graz_costs_cap_dat_se %>% select(comid_form, everything())
  
  bmp_costs_graz_operations_rev_se <- bmp_costs_graz_operations_se[
    order(bmp_costs_graz_operations_se$comid),
  ]
  graz_costs_op_dat_se <- bmp_costs_graz_operations_rev_se[ 
    ,names(bmp_costs_graz_operations_rev_se) != "comid"
  ]
  graz_costs_op_dat_se <- graz_costs_op_dat_se %>% select(comid_form, everything())
}



## Format riparian costs ####
if(length(RiparianBuffer_BMPs) > 0) {
  temp_bmp_costs_ripbuf_se <- merge(
    bmp_costs_se %>%
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
    any(is.na(temp_bmp_costs_ripbuf_se$capital)) | 
    any(is.na(temp_bmp_costs_ripbuf_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for Riparian Buffer ",
        if(
          any(is.na(temp_bmp_costs_ripbuf_se$capital)) & 
          any(is.na(temp_bmp_costs_ripbuf_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ripbuf_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ripbuf_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ripbuf_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `EQIPcosts_overyears.csv`"
      )
    )
  }
  
  bmp_costs_ripbuf_capital_se <- reshape2::dcast(
    temp_bmp_costs_ripbuf_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ripbuf_operations_se <- reshape2::dcast(
    temp_bmp_costs_ripbuf_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  
  bmp_costs_ripbuf_capital_rev_se <- bmp_costs_ripbuf_capital_se[
    order(bmp_costs_ripbuf_capital_se$comid),
  ]
  ripbuf_costs_cap_dat_se <- bmp_costs_ripbuf_capital_rev_se[
    ,names(bmp_costs_ripbuf_capital_rev_se) != "comid"
  ]
  ripbuf_costs_cap_dat_se <- ripbuf_costs_cap_dat_se %>% 
    select(comid_form, everything())
  
  bmp_costs_ripbuf_operations_rev_se <- bmp_costs_ripbuf_operations_se[
    order(bmp_costs_ripbuf_operations_se$comid),
  ]
  ripbuf_costs_op_dat_se <- bmp_costs_ripbuf_operations_rev_se[ 
    ,names(bmp_costs_ripbuf_operations_rev_se) != "comid"
  ]
  ripbuf_costs_op_dat_se <- ripbuf_costs_op_dat_se %>% select(comid_form, everything())
}

### point cost uncertainty

if(
  "point" %in% 
  (user_specs_BMPs %>% filter(BMP_Selection == "X") %>% .$BMP_Category)
) {
  point_comid <- fread(paste(InPath, "WWTP_COMIDs.csv", sep="")) #*#
  # Note to user: Can update this file if more WWTPs desired for analysis; must also update UserSpecs_BMPs.csv with costs
  
  bmp_costs_ptsrc <- fread(paste(InPath, "WWTP_Costs.csv", sep=""))
  
  temp_bmp_costs_point_se <- merge(
    bmp_costs_ptsrc, 
    point_comid,
    by = "COMID",
    all.x = TRUE
  ) %>%
    select(COMID, State, Plant_Name, NPDES_ID, contains("_se")) %>%
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
  
  temp_bmp_costs_point_selected <- temp_bmp_costs_point_se %>% 
    slice(
      which(
        unlist(
          map(
            .x = str_split(temp_bmp_costs_point_se$BMP, "_"), 
            .f = ~(all(.x %in% UserSelected_pointBMPs$BMP)))
        )
      )
    ) %>% # Annualize capital costs based on planning horizon and interest rate
    mutate(
      Cost = case_when(
        CostType == "Capital" ~ (
          Cost * (
            interest_rate * ((1 + interest_rate) ^ horizon) / 
              (((1 + interest_rate) ^ horizon) - 1)
          )
        ),
        CostType == "OM" ~ Cost
      )
    )
  
  
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
    
    bmp_costs_point_se <- temp_bmp_costs_point_selected
  } else {
    print(
      paste0(
        "Based on User Selections, no WWTP upgrade BMPs will be considered. ",
        "The BMPs the User has selected may only be implemented in conjunction with unselected BMPs."
      )
    )
    
    bmp_costs_point_se <- data.frame(
      COMID = NA, State = NA, Plant_Name = NA, NPDES_ID = NA, BMP = NA, 
      CostType = NA, Costunits = NA, Cost = NA
    )
  }
  
} else {
  bmp_costs_point_se <- data.frame(
    COMID = NA, State = NA, Plant_Name = NA, NPDES_ID = NA, BMP = NA, 
    CostType = NA, Costunits = NA, Cost = NA
  )
} 

# Format point source BMP costs data
if (length(Point_BMPs) > 0) {
  temp_point_costs_dat_se <- bmp_costs_point_se[
    bmp_costs_point_se$COMID %in% streamcat_subset_all$comid,
  ] %>% 
    select(comid = COMID, State, BMP, CostType, Cost)
  
  temp_point_costs_capital_se <- temp_point_costs_dat_se %>%
    filter(CostType == "Capital") %>%
    pivot_wider(id_cols = c(comid, State), names_from = BMP, values_from = Cost) %>%
    mutate(across(everything(), .fns = ~replace_na(., 0))) %>% # Replace NA values with 0 to avoid adding costs to model
    select(comid, State, any_of(Point_BMPs)) # selection forces the order
  
  temp_point_costs_operations_se <- temp_point_costs_dat_se %>%
    filter(CostType == "OM") %>%
    pivot_wider(id_cols = c(comid, State), names_from = BMP, values_from = Cost) %>%
    mutate(across(everything(), .fns = ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    select(comid, State, any_of(Point_BMPs)) # selection forces the order
  
  temp_point_costs_other_se <- as.data.frame(
    streamcat_subset_all[
      !(streamcat_subset_all$comid %in% temp_point_costs_dat_se$comid),
    ]
  )
  names(temp_point_costs_other_se) <- "comid"
  temp_point_costs_other_se_rev <- merge(
    temp_point_costs_other_se, COMID_State, by = c("comid"), all.x = TRUE
  )
  
  temp_point_costs_other_se_rev[3:(2+length(Point_BMPs))] <- 0
  names(temp_point_costs_other_se_rev) <- c(
    "comid", "State", Point_BMPs)
  
  if(nrow(temp_point_costs_capital_se) > 0) {
    temp_point_capital_costs_dat_se_rev <- rbind(
      temp_point_costs_capital_se, temp_point_costs_other_se_rev
    )
  } else {
    temp_point_capital_costs_dat_se_rev <- temp_point_costs_other_se_rev
  }
  
  temp_point_capital_costs_dat_se_rev <- temp_point_capital_costs_dat_se_rev[
    order(temp_point_capital_costs_dat_se_rev$comid),
  ]
  temp_point_capital_costs_dat_se_rev$comid_form <- paste0(
    "'", temp_point_capital_costs_dat_se_rev$comid, "'"
  )
  point_costs_capital_dat_se <- temp_point_capital_costs_dat_se_rev %>% 
    select(comid_form, all_of(Point_BMPs))
  
  if(nrow(temp_point_costs_operations_se) > 0) {
    temp_point_operations_costs_dat_se_rev <- rbind(
      temp_point_costs_operations_se, temp_point_costs_other_se_rev
    )
  } else {
    temp_point_operations_costs_dat_se_rev <- temp_point_costs_other_se_rev
  }
  
  temp_point_operations_costs_dat_se_rev <- temp_point_operations_costs_dat_se_rev[
    order(temp_point_operations_costs_dat_se_rev$comid),
  ]
  temp_point_operations_costs_dat_se_rev$comid_form <- paste0(
    "'", temp_point_operations_costs_dat_se_rev$comid, "'"
  )
  point_costs_operations_dat_se <- temp_point_operations_costs_dat_se_rev %>% 
    select(comid_form, all_of(Point_BMPs))
} else {
  point_costs_capital_dat_se <- data.frame(
    comid = NA, State = NA, bmp = NA
  )
  point_costs_operations_dat_se <- data.frame(
    comid = NA, State = NA, bmp = NA
  )
}


### Ag and Graz BMPs ----
# Read in efficiency data for Fert_20 and Manure_Injection BMPs
temp_ag_effic_fert_man <- fread(
  paste(InPath, "AgBMPEffic_nonACRE.csv", sep = "")
) #*#
temp_ag_effic_fert_man_cast_tn <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "N_Efficiency_se"
) #*#
temp_ag_effic_fert_man_cast_tp <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "P_Efficiency_se"
) #*#

#### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####

## Format Septic BMP Costs ----
if(length(Septic_BMPs) > 0){
  
#  septic_bmp_costs_temp_se <- user_specs_BMPs %>%
  #    filter(BMP_Selection == "X" & BMP_Category == "septic") %>%
  #    select(BMP_Category, BMP, capital_WA:operations_WA) %>%
  #    unique() %>%
  #    pivot_longer(
  #      cols = -c(BMP_Category, BMP), 
  #      names_to = c("costtype", "State"), 
  #      names_sep = "_", 
  #      values_to = "Value"
  #    ) %>%
  #    group_by(
  #      BMP_Category, BMP, costtype
  #    ) %>%
  #    mutate(Value = as.numeric(Value)) %>% # ND added 3/1/2026
  #    mutate(mean = mean(Value, na.rm = T), se = (sd(Value, na.rm = T) / sqrt(4))) %>%
  #    pivot_wider(
  #      id_cols = c(BMP_Category, BMP), 
  #      names_from = c(costtype, State), 
  #      values_from = se
  #    ) %>%
  #    ungroup() %>%
  #    group_by(BMP_Category, BMP)%>%
  #    summarise(
  #      cap_cost = mean(capital_WA, na.rm = TRUE),
  #      op_cost = mean(operations_WA, na.rm = TRUE),
  #      .groups = "drop"
  #    )%>%
  #    merge(., user_specs_BMPs%>%
  #            filter(BMP_Selection == "X" & BMP_Category == "septic")%>%
  #            select(BMP_Category, BMP, capital_WA, operations_WA), by = c("BMP_Category", "BMP")
  #    )%>%
  #    mutate(
  #     cap_check = ifelse(cap_cost == capital_WA, 0, 1),
  #      op_check = ifelse(op_cost == operations_WA, 0, 1)
  #    )%>%
  #    select(-capital_WA, -operations_WA, -cap_check, -op_check)
  
  # ND substitute 3/1/2026
  library(dplyr)
  library(tidyr)
  
  septic_bmp_costs_temp_se <- user_specs_BMPs %>%
    filter(BMP_Selection == "X", BMP_Category == "septic") %>%
    select(
      BMP_Category, BMP,
      matches("^(capital|operations)_[A-Za-z]{2}$")  # safer than a range
    ) %>%
    distinct() %>%  # replaces unique()
    pivot_longer(
      cols = -c(BMP_Category, BMP),
      names_to = c("costtype", "State"),
      names_sep = "_",
      values_to = "Value"
    ) %>%
    mutate(Value = as.numeric(Value)) %>%
    group_by(BMP_Category, BMP, costtype) %>%
    summarise(
      mean = mean(Value, na.rm = TRUE),
      se   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from  = costtype,
      values_from = c(mean, se),
      names_glue  = "{costtype}_{.value}"  # e.g., capital_mean, capital_se
    ) %>%
    rename(cap_cost = capital_mean, op_cost = operations_mean)
  ##### end edit
  
  sep_convert_bmp_costs_temp <- septic.conversion %>%
    select(-parcels, -efficiency_per_parcel) %>%
    mutate(
      BMP = "Sewer_convert"
    ) %>%
    merge(., septic_bmp_costs_temp_se%>%select(BMP, cap_cost, op_cost), by = c("BMP")
    ) %>%
    select(comid, everything())
  
  sep_upgrade_bmp_costs_temp <- septic.upgrade%>%
    rename(BMP = SepticUpgradeClass)%>%
    mutate(
      BMP = paste0("class_", BMP, "_upgrade")
    )%>%
    select(-parcels, -efficiency_per_parcel)%>%
    merge(., septic_bmp_costs_temp_se %>% select(BMP, cap_cost, op_cost), by = c("BMP")
    )%>%
    select(comid, everything())
  
  septic_costs_temp_se <- rbind(sep_convert_bmp_costs_temp, sep_upgrade_bmp_costs_temp)
  
  septic_cap_costs_se <- septic_costs_temp_se %>%
    select(comid, BMP, cap_cost) %>%
    mutate(comid = as.character(comid)) %>% # ND add
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
  
  septic_op_costs_se <- septic_costs_temp_se %>%
    select(comid, BMP, op_cost)%>%
    mutate(comid = as.character(comid))%>%
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
  
  # replace NAs in the septic_bmp_costs dataframes with 0
  septic_cap_costs_se[is.na(septic_cap_costs_se)] <- 0
  septic_op_costs_se[is.na(septic_op_costs_se)] <- 0
} else {
  septic_cap_costs_se <- data.frame(
    comid_form = NA, none = NA
  )
  septic_op_costs_se <- data.frame(
    comid_form = NA, none = NA
  )
}

#### end edits ####

## ACRES data - choose ####

temp_acre <- if(AgBMPcomparison == "No Practice") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareNoPractice.csv"))
} else if(AgBMPcomparison == "Baseline") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareBaseline.csv"))
} else {
  stop(
    'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
  )
}#*#

# Read in efficiency data for ACRE database BMPs
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

temp_acre_cast_tn_se <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTN_Effic_se"
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

temp_acre_cast_tn_HUC8_se <- temp_acre_cast_tn_se %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tn_HUC10_se <- temp_acre_cast_tn_se %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tn_HUC12_se <- temp_acre_cast_tn_se %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

ACRE_BMPs <- names(temp_acre_cast_tn_se[, -c(1:3)])

reaches_huc12_tn <- sparrow_in_DYN %>% 
  filter(comid %in% reaches_TN_target$catchment_comid) %>% 
  select(comid, year, season, HUC12 = HUC_12_Rev) %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "year", "season", "HUC12")],
  temp_acre_cast_tn_HUC12_se[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "year", "season", "HUC10")],
  temp_acre_cast_tn_HUC10_se[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "year", "season", "HUC8")],
  temp_acre_cast_tn_HUC8_se[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tn_se <- merge(
  merge(
    merge(
      reaches_huc12_tn, 
      temp_acre_reaches_HUC12_tn_se, 
      by = c("comid", "year", "season", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tn_se,
    by = c("comid", "year", "season", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tn_se,
  by = c("comid", "year", "season", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tn_se[ , ACRE_BMPs] <- NA

acre_reaches_tn_se <- temp_acre_reaches_tn_se %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, year, season, all_of(ACRE_BMPs))

temp_acre_cast_tp_se <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTP_Effic_se"
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

temp_acre_cast_tp_HUC8_se <- temp_acre_cast_tp_se %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tp_HUC10_se <- temp_acre_cast_tp_se %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tp_HUC12_se <- temp_acre_cast_tp_se %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

reaches_huc12_tp <- sparrow_in_DYN %>% 
  filter(comid %in% reaches_TP_target$catchment_comid) %>% 
  select(comid, year, season, HUC12 = HUC_12_Rev)  %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "year", "season", "HUC12")],
  temp_acre_cast_tp_HUC12_se[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "year", "season", "HUC10")],
  temp_acre_cast_tp_HUC10_se[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "year", "season", "HUC8")],
  temp_acre_cast_tp_HUC8_se[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tp_se <- merge(
  merge(
    merge(
      reaches_huc12_tp, 
      temp_acre_reaches_HUC12_tp_se, 
      by = c("comid", "year", "season", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tp_se,
    by = c("comid", "year", "season", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tp_se,
  by = c("comid", "year", "season", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tp_se[ , ACRE_BMPs] <- NA

acre_reaches_tp_se <- temp_acre_reaches_tp_se %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC8")]]
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
    datacheck = map2(.x = N_Efficiency_se, .y = P_Efficiency_se, .f = ~sum(.x, .y))
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

ag_effic_bycomid_tn_se <- add_column(
  acre_reaches_tn_se, 
  temp_ag_effic_fert_man_cast_tn %>%
    filter(Category == "ag") %>%
    select(any_of(Ag_BMPs))
) %>% 
  select(comid, season, year, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.

ag_effic_bycomid_tp_se <- add_column(
  acre_reaches_tp_se, 
  temp_ag_effic_fert_man_cast_tp %>%
    filter(Category == "ag") %>%
    select(any_of(Ag_BMPs))
) %>% 
  select(comid, season, year, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.


# Format agricultural BMP efficiency data

if(length(Ag_BMPs) > 0) {
  temp_ag_effic_dat_tn_se <- ag_effic_bycomid_tn_se[
    ag_effic_bycomid_tn_se$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  ag_effic_dat_tn_se <- temp_ag_effic_dat_tn_se %>% 
    select(-comid) %>% 
    select(comid_form, everything())
  
  temp_ag_effic_dat_tp_se <- ag_effic_bycomid_tp_se[
    ag_effic_bycomid_tp_se$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  ag_effic_dat_tp_se <- temp_ag_effic_dat_tp_se %>% 
    select(-comid) %>% 
    select(comid_form, everything())
}

## format graz efficiency data
if(length(Graz_BMPs) > 0) {
  graz_effic_bycomid_tn_se <- data.frame(
    comid = sparrow_in_DYN %>% 
      filter(comid %in% reaches_TN_target$catchment_comid) %>% 
      .$comid
  ) %>%
    add_column(
      temp_ag_effic_fert_man_cast_tn %>%
        filter(Category == "graz") %>%
        select(any_of(Graz_BMPs))
    )
  
  graz_effic_bycomid_tp_se <- data.frame(
    comid = sparrow_in_DYN %>% 
      filter(comid %in% reaches_TP_target$catchment_comid) %>% 
      .$comid
  ) %>%
    add_column(
      temp_ag_effic_fert_man_cast_tp %>%
        filter(Category == "graz") %>%
        select(any_of(Graz_BMPs))
    )
  
  temp_graz_effic_dat_tn_se <- graz_effic_bycomid_tn_se[
    graz_effic_bycomid_tn_se$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  graz_effic_dat_tn_se <- temp_graz_effic_dat_tn_se %>% 
    unique() %>%
    select(-comid) %>% 
    select(comid_form, everything()) 
  
  temp_graz_effic_dat_tp_se <- graz_effic_bycomid_tp_se[
    graz_effic_bycomid_tp_se$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  graz_effic_dat_tp_se <- temp_graz_effic_dat_tp_se %>% 
    unique() %>%
    select(-comid) %>% 
    select(comid_form, everything())
}


## get urban efficiency ses ####
if(length(Urban_BMPs) > 0) {
  
  urban_effic_bycomid_tn_se <- urban_effic.n %>%
    select(comid, year, season, Land_Use, HSG, BMP, effic_N, std_efficN, n_efficN) %>%
    filter(BMP %in% Urban_BMPs,
           year %in% year_filter) %>%
    mutate(category = "urban",
           se_effic_N = std_efficN / (sqrt(n_efficN))) %>%
    select(
      category, comid, season, year, Land_Use, HSG, bmp = BMP, effic = effic_N, se = se_effic_N
    ) %>%
    unique() %>%
    pivot_wider(
      id_cols = c(comid, season, year, Land_Use, HSG), names_from = bmp, values_from = c(effic, se)
    ) %>%
    fill(any_of(contains(Urban_BMPs))) %>% 
    unnest(cols = 1:ncol(.)) %>%
    unique() %>%
    mutate(across(-c(comid, season, year, Land_Use, HSG), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
    rename_with(~ gsub("^effic_", "", .), starts_with("effic_"))
    
  
  if (length(urban_effic.p$comid > 0)) {
    urban_effic_bycomid_tp_se <- urban_effic.p %>%
      select(comid, season, year, Land_Use, HSG, BMP, effic_P, std_efficP, n_efficP) %>%
      filter(BMP %in% Urban_BMPs,
             year %in% year_filter) %>%
      mutate(category = "urban",
             se_effic_P = std_efficP / (sqrt(n_efficP))) %>%
      select(
        category, comid, season, year, Land_Use, HSG, bmp = BMP, effic = effic_P, se = se_effic_P
      ) %>%
      unique() %>%
      pivot_wider(
        id_cols = c(comid, season, year, Land_Use, HSG), names_from = bmp, values_from = c(effic, se)
      ) %>%
      fill(any_of(contains(Urban_BMPs))) %>%
      unnest(cols = 1:ncol(.)) %>%
      unique() %>%
      mutate(across(-c(comid, season, year, Land_Use, HSG), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
      rename_with(~ gsub("^effic_", "", .), starts_with("effic_"))
    } else {
    urban_effic_bycomid_tp_se <- urban_effic.p %>%
      select(comid, season, year, Land_Use, HSG)
  }
  
  ## Format urban BMP efficiency data ####
  
  temp_urban_effic_dat_tn_se <- urban_effic_bycomid_tn_se[
    urban_effic_bycomid_tn_se$comid %in% 
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
    select(comid_form, season, year, Land_Use, HSG, contains("se_")) %>%
    mutate(across(contains("se_"), ~ case_when(is.na(.) ~ 0, T ~ .))) %>%
    rename_at(vars(contains("se_")), list(~gsub("se_", "", .))) 
  
  urban_effic_dat_tn_se <- temp_urban_effic_dat_tn_se %>% 
    select(comid_form, season, year, Land_Use, HSG, everything()) 

  temp_urban_effic_dat_tp_se <- urban_effic_bycomid_tp_se[
    urban_effic_bycomid_tp_se$comid %in% 
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
    select(comid_form, season, year, Land_Use, HSG, contains("se_")) %>%
    mutate(across(contains("se_"), ~ case_when(is.na(.) ~ 0, T ~ .))) %>%
    rename_at(vars(contains("se_")), list(~gsub("se_", "", .))) 
  
  urban_effic_dat_tp_se <- temp_urban_effic_dat_tp_se %>%
    select(comid_form, season, year, Land_Use, HSG, everything())
  
  # Add in missing BMP columns for TN
  bmp_cols <- urban_effic_dat_tn_se %>%
    select(!comid_form)
  missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
  
  for (col in missing_columns) {
    if (nrow(urban_effic_dat_tn_se) > 0) {
      urban_effic_dat_tn_se[[col]] <- 0
    } else {
      urban_effic_dat_tn_se[[col]] <- vector("numeric", length = 0)
    }
  }
  
  # Add in missing BMP columns for TP    
  bmp_cols <- urban_effic_dat_tp_se %>%
    select(!comid_form)
  missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
  
  for (col in missing_columns) {
    if (nrow(urban_effic_dat_tp_se) > 0) {
      urban_effic_dat_tp_se[[col]] <- 0
    } else {
      urban_effic_dat_tp_se[[col]] <- vector("numeric", length = 0)
    }
  }
 
  
   ## Format urban BMP cost data #### 
  urban_cost_coeffs_se_tmp <- urban.dat %>%
    filter(comid %in% unlist(streamcat_subset_all)) %>%
    select(comid, BMP, Land_Use, HSG, capital_se = cap_rmse, operations_se = om_rmse)
  
  urban_cost_coeffs_se_tmp <- urban_cost_coeffs_se_tmp %>% 
    filter(BMP %in% Urban_BMPs) %>%
    # Annualize capital costs based on planning horizon and interest rate
    mutate(
      capital_se = (as.numeric(capital_se) * (
        interest_rate * ((1 + interest_rate) ^ horizon) / 
          (((1 + interest_rate) ^ horizon) - 1)
      )
      ),
      operations_se = as.numeric(operations_se)
    )

  urban_cost_coeffs_dat_se <- urban_cost_coeffs_se_tmp %>%
    merge(
      ., 
      streamcat_subset_all,
      by = c("comid")
    ) %>%
    unique() %>%
    merge(., complete_urban, by.x = c("comid", "Land_Use", "HSG"), by.y = c("comid", "urban_land_use", "urban_hsg"), all = T)
  
  urban_cost_coeffs_dat_se$comid_form  <- paste0("'", urban_cost_coeffs_dat_se$comid, "'") 
  urban_cost_coeffs_dat_se$Land_Use  <- paste0("'", urban_cost_coeffs_dat_se$Land_Use, "'") 
  urban_cost_coeffs_dat_se$HSG  <- paste0("'", urban_cost_coeffs_dat_se$HSG, "'") 
  
  urban_cost_coeffs_dat_mean_cap_se <- urban_cost_coeffs_dat_se %>%
    group_by(BMP) %>%
    summarise(mean_cap_cost_se = mean(capital_se, na.rm = T)) %>%
    pivot_wider(names_from = BMP, values_from = c(mean_cap_cost_se)) %>%
    mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%
    rename_with(~paste0(., "_mean"))
  
  urban_cost_coeffs_dat_mean_op_se <- urban_cost_coeffs_dat_se %>%
    group_by(BMP) %>%
    summarise(mean_op_cost_se = mean(operations_se, na.rm = T)) %>%
    pivot_wider(names_from = BMP, values_from = c(mean_op_cost_se)) %>%
    mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%
    rename_with(~paste0(., "_mean"))
  
  urban_costs_capital_se <- urban_cost_coeffs_dat_se %>% 
    as_tibble() %>%
    pivot_wider(id_cols = c(comid_form, Land_Use, HSG), names_from = BMP, values_from = capital_se) %>%
    cbind(., urban_cost_coeffs_dat_mean_cap_se) %>%
    # Replace NA values with the average cost within the model to avoid AMPL errors
    mutate(across(any_of(Urban_BMPs), ~map2(., urban_cost_coeffs_dat_mean_cap_se[grepl(cur_column(), names(urban_cost_coeffs_dat_mean_cap_se))], ~replace_na(.x, .y)))) %>%
    # unnest(cols = 4:ncol(.)) %>%
    mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    unique() %>%
    select(!contains("mean")) %>%
    select(comid_form, Land_Use, HSG, any_of(Urban_BMPs))
  
  #Fill in all BMPs if some are dropped due to no data
  bmp_cols <- urban_costs_capital_se %>%
    select(!comid_form)
  
  missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
  
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      if (nrow(urban_costs_capital_se) > 0) {
        urban_costs_capital_se[[col]] <- 0
      } else {
        urban_costs_capital_se[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
  urban_costs_operations_se <- urban_cost_coeffs_dat_se %>%
    pivot_wider(id_cols = c(comid_form, Land_Use, HSG), names_from = BMP, values_from = operations_se) %>%
    cbind(., urban_cost_coeffs_dat_mean_op_se) %>%
    # Replace NA values with the average cost within the model to avoid AMPL errors
    mutate(across(any_of(Urban_BMPs), ~map2(., urban_cost_coeffs_dat_mean_op_se[grepl(cur_column(), names(urban_cost_coeffs_dat_mean_op_se))], ~replace_na(.x, .y)))) %>%
    # unnest(cols = 4:ncol(.)) %>%
    mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%  # # Replace NA values with 0 to avoid adding costs to model
    unique() %>%
    select(!contains("mean")) %>%
    select(comid_form, Land_Use, HSG, any_of(Urban_BMPs))
  
  #Fill in all BMPs if some are dropped due to no data
  bmp_cols <- urban_costs_operations_se %>%
    select(!comid_form)
  
  missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
  
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      if (nrow(urban_costs_operations_se) > 0) {
        urban_costs_operations_se[[col]] <- 0
      } else {
        urban_costs_operations_se[[col]] <- vector("numeric", length = 0)
      }
    }
  }
}


if(length(road_BMPs) > 0) { 
  
  road_effic_bycomid_tn_se <- road.effic.comid %>%
    select(comid, season, year, HSG, BMP, effic_N, std_efficN, n_efficN) %>%
    filter(BMP %in% road_BMPs,
           year %in% year_filter) %>%
    mutate(category = "road",
           se_effic_N = std_efficN / (sqrt(n_efficN))) %>%
    select(
      category, comid, season, year, HSG, bmp = BMP, effic = effic_N, se = se_effic_N
    ) %>%
    unique() %>%
    pivot_wider(
      id_cols = c(comid, season, year, HSG), names_from = bmp, values_from = c(effic, se)
    ) %>%
    fill(any_of(contains(road_BMPs))) %>% unnest(cols = 1:ncol(.)) %>%
    unique() %>%
    mutate(across(-c(comid, season, year, HSG), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
    rename_with(~ gsub("^effic_", "", .), starts_with("effic_"))
  
  
  if (length(road_effic.p$comid > 0)) {
    road_effic_bycomid_tp_se <- road.effic.comid %>%
      select(comid, season, year, HSG, BMP, effic_P, std_efficP, n_efficP) %>%
      filter(BMP %in% road_BMPs,
             year %in% year_filter) %>%
      mutate(category = "road",
             se_effic_P = std_efficP / (sqrt(n_efficP))) %>%
      select(
        category, comid, season, year, HSG, bmp = BMP, effic = effic_P, se = se_effic_P
      ) %>%
      unique() %>%
      pivot_wider(
        id_cols = c(comid, season, year, HSG), names_from = bmp, values_from = c(effic, se)
      ) %>%
      fill(any_of(contains(road_BMPs))) %>%
      unnest(cols = 1:ncol(.)) %>%
      unique() %>%
      mutate(across(-c(comid, season, year, HSG), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
      rename_with(~ gsub("^effic_", "", .), starts_with("effic_"))
  } else {
    road_effic_bycomid_tp_se <- road_effic.p %>%
      select(comid, season, year, HSG)
  }
  
  ## Format road BMP efficiency data ####
  if(length(road_effic_bycomid_tn_se$comid) > 0) {
    temp_road_effic_dat_tn_se <- road_effic_bycomid_tn_se[
      road_effic_bycomid_tn_se$comid %in% 
        unlist(streamcat_subset_tn, use.names = FALSE),
    ] %>%
      merge(., year_xwalk, by = "year") %>%
      select(-year) %>%
      rename(year = model_year) %>%
      merge(., complete_road_N, by.x = c("comid", "season", "year", "HSG"), by.y = c("comid", "season", "year", "road_hsg"), all = T) %>%
      arrange(comid, season, year, HSG) %>%
      mutate(comid_form = paste0("'", comid, "'"),
             HSG = paste0("'", HSG, "'")) %>%
      select(comid_form, season, year, HSG, contains("se_")) %>%
      mutate(across(contains("se_"), ~ case_when(is.na(.) ~ 0, T ~ .))) %>%
      rename_at(vars(contains("se_")), list(~gsub("se_", "", .)))
    
    road_effic_dat_tn_se <- temp_road_effic_dat_tn_se %>% 
      select(comid_form, season, year, HSG, everything())
  } else {
    temp_road_effic_dat_tn_se <- data.frame(
      comid = unlist(streamcat_subset_tn, use.names = FALSE),
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
    
    road_effic_dat_tn_se <- temp_road_effic_dat_tn_se %>% 
      select(-comid) %>% 
      select(comid_form, season, year, HSG, everything())
    
  }
  
  if(length(road_effic_bycomid_tp_se$comid) > 0) {
    temp_road_effic_dat_tp_se <- road_effic_bycomid_tp_se[
      road_effic_bycomid_tp_se$comid %in% 
        unlist(streamcat_subset_tp, use.names = FALSE),
    ] %>%
      merge(., year_xwalk, by = "year") %>%
      select(-year) %>%
      rename(year = model_year) %>%
      merge(., complete_road_P, by.x = c("comid", "season", "year", "HSG"), by.y = c("comid", "season", "year", "road_hsg"), all = T) %>%
      arrange(comid, season, year, HSG) %>%
      mutate(comid_form = paste0("'", comid, "'"),
             HSG = paste0("'", HSG, "'")) %>%
      select(comid_form, season, year, HSG, contains("se_")) %>%
      mutate(across(contains("se_"), ~ case_when(is.na(.) ~ 0, T ~ .))) %>%
      rename_at(vars(contains("se_")), list(~gsub("se_", "", .)))
    
    road_effic_dat_tp_se <- temp_road_effic_dat_tp_se %>%
      select(comid_form, season, year, HSG, everything())
  } else {
    temp_road_effic_dat_tp_se <- data.frame(
      comid = unlist(streamcat_subset_tp, use.names = FALSE),
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
    
    road_effic_dat_tp_se <- temp_road_effic_dat_tp_se %>% 
      select(-comid) %>% 
      select(comid_form, season, year, HSG, everything())
  }
  
  # Add in missing BMP columns for TN
  bmp_cols <- road_effic_dat_tn_se %>%
    select(!comid_form)
  missing_columns <- setdiff(road_BMPs, names(bmp_cols))
  
  for (col in missing_columns) {
    if (nrow(road_effic_dat_tn_se) > 0) {
      road_effic_dat_tn_se[[col]] <- 0
    } else {
      road_effic_dat_tn_se[[col]] <- vector("numeric", length = 0)
    }
  }
  
  # Add in missing BMP columns for TP    
  bmp_cols <- road_effic_dat_tp_se %>%
    select(!comid_form)
  missing_columns <- setdiff(road_BMPs, names(bmp_cols))
  
  for (col in missing_columns) {
    if (nrow(road_effic_dat_tp_se) > 0) {
      road_effic_dat_tp_se[[col]] <- 0
    } else {
      road_effic_dat_tp_se[[col]] <- vector("numeric", length = 0)
    }
  }
  
  ##  Define road cost correction coefficients ####
  
  road_cost_coeffs_se_tmp <- road.dat %>%
    filter(comid %in% unlist(streamcat_subset_all)) %>%
    select(comid, BMP, HSG, capital_se = cap_rmse, operations_se = om_rmse)
  
  road_cost_coeffs_se_tmp <- road_cost_coeffs_se_tmp %>% 
    filter(BMP %in% road_BMPs) %>%
    # Annualize capital costs based on planning horizon and interest rate
    mutate(
      capital_se = (as.numeric(capital_se) * (
        interest_rate * ((1 + interest_rate) ^ horizon) / 
          (((1 + interest_rate) ^ horizon) - 1)
      )
      ),
      operations_se = as.numeric(operations_se)
    )
  
  road_cost_coeffs_dat_se <- road_cost_coeffs_se_tmp %>%
    merge(
      ., 
      streamcat_subset_all, 
      by = c("comid")
    ) %>%
    merge(., complete_road, by.x = c("comid", "HSG"), by.y = c("comid", "road_hsg"), all = T)
  
  
  road_cost_coeffs_dat_se$comid_form  <- paste0("'", road_cost_coeffs_dat_se$comid, "'")
  road_cost_coeffs_dat_se$HSG <- paste0("'", road_cost_coeffs_dat_se$HSG, "'")
  
  road_costs_capital_se <- road_cost_coeffs_dat_se %>% 
    pivot_wider(id_cols = c(comid_form, HSG), names_from = BMP, values_from = capital_se) %>%
    unnest(cols = 1:ncol(.)) %>%
    mutate(across(any_of(road_BMPs), ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    unique() %>%
    select(comid_form, HSG, any_of(road_BMPs))
  
  #Fill in all BMPs if some are dropped due to no data
  bmp_cols <- road_costs_capital_se %>%
    select(!comid_form)
  
  missing_columns <- setdiff(road_BMPs, names(bmp_cols))
  
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      if (nrow(road_costs_capital_se) > 0) {
        road_costs_capital_se[[col]] <- 0
      } else {
        road_costs_capital_se[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
  road_costs_operations_se <- road_cost_coeffs_dat_se %>%
    pivot_wider(id_cols = c(comid_form, HSG), names_from = BMP, values_from = operations_se) %>%
    unnest(cols = 1:ncol(.)) %>%
    mutate(across(any_of(road_BMPs), ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    unique() %>%
    select(comid_form, HSG, any_of(road_BMPs))
  
  #Fill in all BMPs if some are dropped due to no data
  bmp_cols <- road_costs_operations_se %>%
    select(!comid_form)
  
  missing_columns <- setdiff(road_BMPs, names(bmp_cols))
  
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      if (nrow(road_costs_operations_se) > 0) {
        road_costs_operations_se[[col]] <- 0
      } else {
        road_costs_operations_se[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
}

#### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####

## KM: Set Septic BMP efficiencies to 0
if(length(Septic_BMPs) > 0) {
  septic_effic_dat_tn_se <- septic_effic_dat_tn %>% 
    mutate(across(any_of(Septic_BMPs), ~ 0))
  
} else {
  septic_effic_dat_tn_se <- data.frame(
    comid = NA, comid_form = NA, effic = NA
  )
}

#### end edits ####

# Write Command Script ####

cat(
  " 
#WMOST Optimization Screening Tool AMPL command file with uncertainty

option display_transpose -10000;

param urban_frac_by_comid_bmp {c in comid_all, u in urban_bmp};
param road_frac_by_comid_bmp {c in comid_all, r in road_bmp};
",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "\n"
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("\ndisplay loads_lim_N", i, ";"),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0("\ndisplay loads_lim_P", i, ";"),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

cat(
  "
for {i in Scenarios} {

if i > 1
  then {
  ", 
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("\nfor {s in seasons, y in years} {
               \nlet loads_lim_N", i, "[s,y, 'limits']:= loads_lim_N", i, "[s,y,'limits'] * ", scenarioincrement, ";}"),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)


invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0("\nfor {s in seasons, y in years} {\nlet loads_lim_P", i, "[s,y,'limits']:= loads_lim_P", i, "[s,y,'limits'] * ", scenarioincrement, ";}"),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

cat(
  "
  };
solve;

for {c in comid_all, u in urban_bmp} {
  let urban_frac_by_comid_bmp[c,u] := 
    (sum {d in urban_land_uses, n in urban_soil_groups} 
     (urban_frac[c,d,n,u] * urban_area[c,d,n,'urban'])) / 
    (sum {d in urban_land_uses, n in urban_soil_groups} 
     urban_area[c,d,n,'urban'] + 1e-10);
}

for {c in comid_all, r in road_bmp} {
  let road_frac_by_comid_bmp[c,r] := 
    (sum {h in road_soil_groups} 
     (road_frac[c,h,r] * road_area[c,h,'road'])) / 
    (sum {h in road_soil_groups} 
     road_area[c,h,'road'] + 1e-10);
}

for {x in Bootstraps} {
",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)

cat(
  "
	let agcost_frac_rev[x] := Uniform(1,5/3);
	 ",    
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nfor {c in comid_N",
          i,
          "} {\nfor {s in seasons} {
            for {y in years} {
              for {l in loads_all} {\nrepeat {\nlet  baseloads_N",
          i,
          "_rev[c,s,y,l,x] := Normal(baseloads_N",
          i,
          "[c,s,y,l], baseloads_N",
          i,
          "_se[c,s,y,l]);\n} until baseloads_N",
          i,
          "_rev[c,s,y,l,x] >= 0;\n};\nrepeat {
          let transfer_coefficients_N",
          i,
          "_rev[c,s,y,'coeff',x] := Normal(transfer_coefficients_N",
          i,
          "[c,s,y,'coeff'], transfer_coefficients_N",
          i,
          "_se[c,s,y,'coeff']);\n} until transfer_coefficients_N",
          i,
          "_rev[c,s,y,'coeff',x] <= 1;\nfor {b in ripbuf_bmp} {\nlet riparianremoval_N",
          i,
          "_rev[c,s,y,b,x] := Normal(riparianremoval_N",
          i,
          "[c,s,y,b], riparianremoval_N",
          i,
          "_se[c,s,y,b]);\n  }; \n};  \n};  \n};"
        ),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)


if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
for {c in comid_all_N} {
  for {a in ag_bmp} {
    repeat {
      let ag_effic_N_rev[c,a,x] := Normal(ag_effic_N[c,a], ag_effic_N_se[c,a]);
    } until ag_effic_N_rev[c,a,x] <= 1;
  };
  for {g in graz_bmp} {
    repeat {
      let graz_effic_N_rev[c,g,x] := Normal(graz_effic_N[c,g], graz_effic_N_se[c,g]);
    } until graz_effic_N_rev[c,g,x] <=1;
  };
  for {p in point_bmp} {
    repeat {
      let point_effic_N_rev[c,p,x] := Normal(point_effic_N[c,p], point_effic_N_se[c,p]);
    } until point_effic_N_rev[c,p,x] <= 1;
  };
  for {u in urban_bmp} {
    for {s in seasons, y in years, d in urban_land_uses, n in urban_soil_groups} {
      repeat {
        let urban_effic_N_rev[c,s,y,d,n,u,x] := Normal(urban_effic_N[c,s,y,d,n,u], urban_effic_N_se[c,s,y,d,n,u]);
      } until urban_effic_N_rev[c,s,y,d,n,u,x] <= 1;
    };
  };
  for {r in road_bmp} {
    for {s in seasons, y in years, h in road_soil_groups} {
      repeat {
        let road_effic_N_rev[c,s,y,h,r,x] := Normal(road_effic_N[c,s,y,h,r], road_effic_N_se[c,s,y,h,r]);
      } until road_effic_N_rev[c,s,y,h,r,x] <= 1;
    };
  };
  for {e in septic_bmp} {
    repeat {
      let septic_effic_N_rev[c,e,x] := Normal(septic_effic_N[c,e], septic_effic_N_se[c,e]);
    } until septic_effic_N_rev[c,e,x] <= 1;
  };
};
",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
    sep = "",
    append = T
  )
}

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\nfor {c in comid_P",
          i,
          "} {\nfor {s in seasons} {
          for {y in years} { 
          for {l in loads_all} {\nrepeat {\nlet  baseloads_P",
          i,
          "_rev[c,s,y,l,x] := Normal(baseloads_P",
          i,
          "[c,s,y,l], baseloads_P",
          i,
          "_se[c,s,y,l]);\n} until baseloads_P",
          i,
          "_rev[c,s,y,l,x] >= 0;\n};\nrepeat {
          let transfer_coefficients_P",
          i,
          "_rev[c,s,y,'coeff',x] := Normal(transfer_coefficients_P",
          i,
          "[c,s,y,'coeff'], transfer_coefficients_P",
          i,
          "_se[c,s,y,'coeff']);\n} until transfer_coefficients_P",
          i,
          "_rev[c,s,y,'coeff',x] <= 1;\nfor {b in ripbuf_bmp} {\nlet riparianremoval_P",
          i,
          "_rev[c,s,y,b,x] := Normal(riparianremoval_P",
          i,
          "[c,s,y,b], riparianremoval_P",
          i,
          "_se[c,s,y,b]);\n  };\n};\n};\n};"
        ),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
for {c in comid_all_P} {
  for {a in ag_bmp} {
    repeat {
      let ag_effic_P_rev[c,a,x] := Normal(ag_effic_P[c,a], ag_effic_P_se[c,a]);
    } until ag_effic_P_rev[c,a,x] <=1;
  };
  for {g in graz_bmp} {
    repeat {
      let graz_effic_P_rev[c,g,x] := Normal(graz_effic_P[c,g], graz_effic_P_se[c,g]);
    } until graz_effic_P_rev[c,g,x] <=1;
  };
  for {p in point_bmp} {
    repeat {
      let point_effic_P_rev[c,p,x] := Normal(point_effic_P[c,p], point_effic_P_se[c,p]);
    } until point_effic_P_rev[c,p,x] <= 1;
  };
  for {u in urban_bmp} {
    for {s in seasons, y in years, d in urban_land_uses, n in urban_soil_groups} {
      repeat {
        let urban_effic_P_rev[c,s,y,d,n,u,x] := Normal(urban_effic_P[c,s,y,d,n,u], urban_effic_P_se[c,s,y,d,n,u]);
      } until urban_effic_P_rev[c,s,y,d,n,u,x] <= 1;
    };
  };
  for {r in road_bmp} {
    for {s in seasons, y in years, h in road_soil_groups} {
      repeat {
        let road_effic_P_rev[c,s,y,h,r,x] := Normal(road_effic_P[c,s,y,h,r], road_effic_P_se[c,s,y,h,r]);
      } until road_effic_P_rev[c,s,y,h,r,x] <= 1;
    };
  };
};",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
    sep = "\n",
    append = TRUE
  )
}


cat(
  "
for {c in comid_all} {
    
  for {p in point_bmp} {
    repeat {
      let point_costs_capital_rev[c,p,x] := Normal(point_costs_capital[c,p], point_costs_capital_se[c,p]);
    } until point_costs_capital_rev[c,p,x] >= 0;
    repeat {
      let point_costs_operations_rev[c,p,x] := Normal(point_costs_operations[c,p], point_costs_operations_se[c,p]);
    } until point_costs_operations_rev[c,p,x] >= 0;
  };
  
  for {b in ripbuf_bmp} {
    repeat {
      let ripbuf_costs_capital_rev[c,b,x] := Normal(ripbuf_costs_capital[c,b], ripbuf_costs_capital_se[c,b]);
    } until ripbuf_costs_capital_rev[c,b,x] >= 0;
    repeat { 
      let ripbuf_costs_operations_rev[c,b,x] := Normal(ripbuf_costs_operations[c,b], ripbuf_costs_operations_se[c,b]);
    } until ripbuf_costs_operations_rev[c,b,x] >= 0;
  };
  
  for {a in ag_bmp} {
    repeat {
      let ag_costs_capital_rev[c,a,x] := Normal(ag_costs_capital[c,a], ag_costs_capital_se[c,a]);
    } until ag_costs_capital_rev[c,a,x] >= 0;
    repeat {
      let ag_costs_operations_rev[c,a,x] :=  Normal(ag_costs_operations[c,a], ag_costs_operations_se[c,a]);
    } until ag_costs_operations_rev[c,a,x] >= 0;
  };
  
  for {g in graz_bmp} {
    repeat {
      let graz_costs_capital_rev[c,g,x] := Normal(graz_costs_capital[c,g], graz_costs_capital_se[c,g]);
    } until graz_costs_capital_rev[c,g,x] >= 0;
    repeat {
      let graz_costs_operations_rev[c,g,x] :=  Normal(graz_costs_operations[c,g], graz_costs_operations_se[c,g]);
    } until graz_costs_operations_rev[c,g,x] >= 0;
  };
  
  for {u in urban_bmp} {
    for {d in urban_land_uses, n in urban_soil_groups} {
      repeat {
	      let urban_costs_capital_rev[c,d,n,u,x] := Normal(urban_costs_capital[c,d,n,u], urban_costs_capital_se[c,d,n,u]);
	    } until urban_costs_capital_rev[c,d,n,u,x] >= 0;
  	  repeat {
  	      let urban_costs_operations_rev[c,d,n,u,x] := Normal(urban_costs_operations[c,d,n,u], urban_costs_operations_se[c,d,n,u]);
  	  } until urban_costs_operations_rev[c,d,n,u,x] >= 0;
    };
	};
	
	 for {r in road_bmp} {
	  for {h in road_soil_groups} {
	    repeat {
	      let road_costs_capital_rev[c,h,r,x] := Normal(road_costs_capital[c,h,r], road_costs_capital_se[c,h,r]);
  	  } until road_costs_capital_rev[c,h,r,x] >= 0;
  	  repeat {
  	      let road_costs_operations_rev[c,h,r,x] := Normal(road_costs_operations[c,h,r], road_costs_operations_se[c,h,r]);
  	  } until road_costs_operations_rev[c,h,r,x] >= 0;
	  };
	};
  
  
  for {ar in area_sub} {
    repeat {
      let area_rev[c,ar,x] := Normal(area[c,ar], area_se[c,ar]);
    } until area_rev[c,ar,x] >= 0;
  };
  
  for {ur in urban_area_sub} {
    for {d in urban_land_uses, n in urban_soil_groups} {
      repeat {
        let urban_area_rev[c,d,n,ur,x] := Normal(urban_area[c,d,n,ur], urban_area_se[c,d,n,ur]);
      } until urban_area_rev[c,d,n,ur,x] >= 0;
    };
  };
  
  for {rd in road_area_sub} {
    for {h in road_soil_groups} {
      repeat {
        let road_area_rev[c,h,rd,x] := Normal(road_area[c,h,rd], road_area_se[c,h,rd]);
      } until road_area_rev[c,h,rd,x] >= 0;
    };
  };
  
};
",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""), # CTC edited 2/6/2026
  sep = "\n",
  append = TRUE
)

#### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####

# Add septic costs loop only for TN watersheds
if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
for {c in comid_all_N} {
  for {e in septic_bmp} {
    repeat {
      let septic_costs_capital_rev[c,e,x] := Normal(septic_costs_capital[c,e], septic_costs_capital_se[c,e]);
    } until septic_costs_capital_rev[c,e,x] >= 0;
    repeat {
      let septic_costs_operations_rev[c,e,x] :=  Normal(septic_costs_operations[c,e], septic_costs_operations_se[c,e]);
    } until septic_costs_operations_rev[c,e,x] >= 0;
  };
};
",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
    sep = "\n",
    append = TRUE
  )
}

#### end edits ####

# CTC edited: 2/6/2026
cat( 
  "
};

option display_precision 10;
display solve_result_num, solve_result;
display cost.result;
display cost;
option display_1col 10000000000;
option omit_zero_rows 1;
option omit_zero_cols 1;
",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "\n",
  append = TRUE
)

cat(
  "display {x in Bootstraps} (sum {c in comid_all, p in point_bmp} (ps_coef_rev[c,p,x] * point_dec[c,p]) + 
  sum {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} (urban_coef_rev[c,d,n,u,x] * urban_frac[c,d,n,u]) + 
  sum {c in comid_all, h in road_soil_groups, r in road_bmp} (road_coef_rev[c,h,r,x] * road_frac[c,h,r]) + 
  sum {c in comid_all, a in ag_bmp} (ag_coef_rev[c,a,x] * ag_frac[c,a]) +
  sum {c in comid_all, g in graz_bmp} (graz_coef_rev[c,g,x] * graz_frac[c,g]) +",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
  sep = "\n", 
  append = T
)

#### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####

# Add septic cost component only if septic BMPs are enabled
if(length(Septic_BMPs) > 0) {
  cat(
    "sum {c in comid_all, b in ripbuf_bmp} (ripbuf_coef_rev[c,b,x] * ripbuf_length[c,b]) +
sum {c in comid_all_N, e in septic_bmp} (septic_coef_rev[c,e,x] * septic_frac[c,e]));",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
    sep = "\n", 
    append = T
  )
}
#### ND added 3/1/26
if(length(Septic_BMPs) == 0) {
  cat(
    "sum {c in comid_all, b in ripbuf_bmp} (ripbuf_coef_rev[c,b,x] * ripbuf_length[c,b]));",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
    sep = "\n", 
    append = T
  )
}


# Function to create revised septic terms conditionally
create_septic_terms_rev <- function() {
  if(length(Septic_BMPs) > 0) {
    return(paste0(
      "(baseloads_N", "PLACEHOLDER_I", "_rev [c,s,y,'point',x] + ((baseloads_N", "PLACEHOLDER_I", "_rev [c,s,y,'septic',x]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N_rev[c,'Sewer_convert',x])) * (1 - (point_effic_N_rev[c,'point',x] * point_dec[c])) + \n",
      "          (baseloads_N", "PLACEHOLDER_I", "_rev [c,s,y,'septic',x] - sum {e in septic_bmp} (baseloads_N", "PLACEHOLDER_I", "_rev [c,s,y,'septic',x]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N_rev[c,e,x])"
    ))
  } else {
    return("baseloads_NPLACEHOLDER_I_rev[c,s,y,'point',x] * (1 - (point_effic_N_rev[c,'point',x] * point_dec[c]))")
  }
}

#### end edits ####

if(actionable_load == TRUE){
  invisible(
    if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        
        #### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####
        
        # CTC Note: these lines are different from the LIS, edited based on actionable_load == FALSE code
        
        # Create conditional septic terms for complex equation
        if(length(Septic_BMPs) > 0) {
          septic_point_term_rev <- paste0("((baseloads_N", i, "_rev[c,s,y,'septic',x]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N_rev[c,'Sewer_convert',x])")
          septic_reduction_term_rev <- paste0("(baseloads_N", i, "_rev[c,s,y,'septic',x] - sum {e in septic_bmp} (baseloads_N", i, "_rev[c,s,y,'septic',x]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N_rev[c,e,x])")
        } else {
          septic_point_term_rev <- "0"
          septic_reduction_term_rev <- "0"
        }
        
        cat(
          paste0(
            "for {c in comid_N", i, ", s in seasons, y in years, x in Bootstraps} {\n
            let inc_storage_load_N",i,"_rev[c,s,y,x] :=
          if s=first(seasons) then (if y = first(years) then ((baseloads_N", i, "_rev[c,s,y,'storage',x] + \n
          baseloads_N", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_N_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_N_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_N", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_N_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',x] + ", septic_point_term_rev, ") * (1 - sum {p in point_bmp}(point_effic_N_rev[c,p,x] * point_dec[c,p])) - \n
          ", septic_reduction_term_rev, " - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_N", i, "_rev[c,s,y,b,x]))) else ((inc_storage_load_N", i, "_rev[c,4,y-1,x] * transfer_coefficients_N", i, "_rev[c,4,y-1,'coeff',x]) + \n
          baseloads_N", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_N_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_N_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_N", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_N_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',x] + ", septic_point_term_rev, ") * (1 - sum {p in point_bmp}(point_effic_N_rev[c,p,x] * point_dec[c,p])) - \n
          ", septic_reduction_term_rev, " - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_N", i, "_rev[c,s,y,b,x]))) else ((inc_storage_load_N", i, "_rev[c,s-1,y,x] * transfer_coefficients_N", i, "_rev[c,s-1,y,'coeff',x]) + \n
          baseloads_N", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_N_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_N_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_N", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_N_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',x] + ", septic_point_term_rev, ") * (1 - sum {p in point_bmp}(point_effic_N_rev[c,p,x] * point_dec[c,p])) - \n
          ", septic_reduction_term_rev, " - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_N", i, "_rev[c,s,y,b,x]));\n};"
          ),
          file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
          sep = "\n", 
          append = T
        )
        
        #### end edits ####
        
      }
    }
  ) #equation version without other load component
  print("Basing TN incremental equations only on actionable load components")
}else{
  print("Basing TN incremental equations on all load components")
}
if(actionable_load == FALSE){
  invisible(
    if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        
        #### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####
        
        # Create conditional septic terms for complex equation
        if(length(Septic_BMPs) > 0) {
          septic_point_term_rev <- paste0("((baseloads_N", i, "_rev[c,s,y,'septic',x]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N_rev[c,'Sewer_convert',x])")
          septic_reduction_term_rev <- paste0("(baseloads_N", i, "_rev[c,s,y,'septic',x] - sum {e in septic_bmp} (baseloads_N", i, "_rev[c,s,y,'septic',x]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N_rev[c,e,x])")
        } else {
          septic_point_term_rev <- "0"
          septic_reduction_term_rev <- "0"
        }
        
        cat(
          paste0(
            "for {c in comid_N", i, ", s in seasons, y in years, x in Bootstraps} {\n
            let inc_storage_load_N",i,"_rev[c,s,y,x] :=
          if s=first(seasons) then (if y = first(years) then ((baseloads_N", i, "_rev[c,s,y,'storage',x] + \n
          baseloads_N", i, "_rev[c,s,y,'other',x] + \n
          baseloads_N", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_N_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_N_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_N", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_N_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',x] + ", septic_point_term, ") * (1 - sum {p in point_bmp}(point_effic_N_rev[c,p,x] * point_dec[c,p])) - \n
          ", septic_reduction_term_rev, " - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_N", i, "_rev[c,s,y,b,x]))) else ((inc_storage_load_N", i, "_rev[c,4,y-1,x] * transfer_coefficients_N", i, "_rev[c,4,y-1,'coeff',x]) + \n
          baseloads_N", i, "_rev[c,s,y,'other',x] + \n
          baseloads_N", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_N_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_N_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_N", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_N_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',x] + ", septic_point_term, ") * (1 - sum {p in point_bmp}(point_effic_N_rev[c,p,x] * point_dec[c,p])) - \n
          ", septic_reduction_term_rev, " - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_N", i, "_rev[c,s,y,b,x]))) else ((inc_storage_load_N", i, "_rev[c,s-1,y,x] * transfer_coefficients_N", i, "_rev[c,s-1,y,'coeff',x]) + \n
          baseloads_N", i, "_rev[c,s,y,'other',x] + \n
          baseloads_N", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_N_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_N_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_N", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_N_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',x] + ", septic_point_term, ") * (1 - sum {p in point_bmp}(point_effic_N_rev[c,p,x] * point_dec[c,p])) - \n
          ", septic_reduction_term_rev, " - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_N", i, "_rev[c,s,y,b,x]));\n};"
          ),
          file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
          sep = "\n", 
          append = T
        )
        
        #### end edits ####
        
      }
    }
  )#equation version with other load component
  print("TN Incremental equations based on all load components")
}else{
  print("TN Incremental equations based only on actionable load components")
}

if(actionable_load == TRUE){
  invisible(
    if ("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      foreach(i = 1:length(param_loads_lim_tp)) %do% {
        cat(
          paste0(
            "for {c in comid_P", i, ", s in seasons, y in years, x in Bootstraps} {\n
            let inc_storage_load_P",i,"_rev[c,s,y,x] :=
          if s=first(seasons) then (if y = first(years) then ((baseloads_P", i, "_rev[c,s,y,'storage',x] + \n
          baseloads_P", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_P", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_P_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_P", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_P_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_P", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_P_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_P", i, "_rev[c,s,y,'point',x]) * (1 - sum {p in point_bmp}(point_effic_P_rev[c,p,x] * point_dec[c,p])) - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_P", i, "_rev[c,s,y,b,x]))) else ((inc_storage_load_P", i, "_rev[c,4,y-1,x] * transfer_coefficients_P", i, "_rev[c,4,y-1,'coeff',x]) + \n
          baseloads_P", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_P", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_P_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_P", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_P_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_P", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_P_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_P", i, "_rev[c,s,y,'point',x]) * (1 - sum {p in point_bmp}(point_effic_P_rev[c,p,x] * point_dec[c,p])) - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_P", i, "_rev[c,s,y,b,x]))) else ((inc_storage_load_P", i, "_rev[c,s-1,y,x] * transfer_coefficients_P", i, "_rev[c,s-1,y,'coeff',x]) + \n
          baseloads_P", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_P", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_P_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_P", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_P_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_P", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_P_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_P", i, "_rev[c,s,y,'point',x]) * (1 - sum {p in point_bmp}(point_effic_P_rev[c,p,x] * point_dec[c,p])) - \n
            sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_P", i, "_rev[c,s,y,b,x]));\n};"
          ),
          file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  ) #equation version without other load component
  print("Basing TP incremental equations only on actionable load components")
}else{
  print("Basing TP incremental equations on all load components")
}

if(actionable_load == FALSE){
  invisible(
    if ("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      foreach(i = 1:length(param_loads_lim_tp)) %do% {
        cat(
          paste0(
            "for {c in comid_P", i, ", s in seasons, y in years, x in Bootstraps} {\n
            let inc_storage_load_P",i,"_rev[c,s,y,x] :=
          if s=first(seasons) then (if y = first(years) then ((baseloads_P", i, "_rev[c,s,y,'storage',x] + \n
          baseloads_P", i, "_rev[c,s,y,'other',x] + \n
          baseloads_P", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_P", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_P_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_P", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_P_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_P", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_P_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_P", i, "_rev[c,s,y,'point',x]) * (1 - sum {p in point_bmp}(point_effic_P_rev[c,p,x] * point_dec[c,p])) - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_P", i, "_rev[c,s,y,b,x]))) else ((inc_storage_load_P", i, "_rev[c,4,y-1,x] * transfer_coefficients_P", i, "_rev[c,4,y-1,'coeff',x]) + \n
          baseloads_P", i, "_rev[c,s,y,'other',x] + \n
          baseloads_P", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_P", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_P_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_P", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_P_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_P", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_P_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_P", i, "_rev[c,s,y,'point',x]) * (1 - sum {p in point_bmp}(point_effic_P_rev[c,p,x] * point_dec[c,p])) - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_P", i, "_rev[c,s,y,b,x]))) else ((inc_storage_load_P", i, "_rev[c,s-1,y,x] * transfer_coefficients_P", i, "_rev[c,s-1,y,'coeff',x]) + \n
          baseloads_P", i, "_rev[c,s,y,'other',x] + \n
          baseloads_P", i, "_rev[c,s,y,'ag',x] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,x] * ag_frac[c,a])) + \n
          baseloads_P", i, "_rev[c,s,y,'graz',x] * (1 - sum {g in graz_bmp}(graz_effic_P_rev[c,g,x] * graz_frac[c,g])) + \n
          baseloads_P", i, "_rev[c,s,y,'urban',x] * (1 - sum{u in urban_bmp, d in urban_land_uses, n in urban_soil_groups}(urban_effic_P_rev[c,s,y,d,n,u,x] * urban_frac[c,d,n,u])) + \n
          baseloads_P", i, "_rev[c,s,y,'road',x] * (1 - sum{r in road_bmp, h in road_soil_groups}(road_effic_P_rev[c,s,y,h,r,x] * road_frac[c,h,r])) + \n
          (baseloads_P", i, "_rev[c,s,y,'point',x]) * (1 - sum {p in point_bmp}(point_effic_P_rev[c,p,x] * point_dec[c,p])) - \n
          sum {b in ripbuf_bmp} (ripbuf_length[c,b] * riparianremoval_P", i, "_rev[c,s,y,b,x]));\n};"
          ),
          file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  ) #equation version with other load component
  print("Incremental TP equations based on all load components")
}else{
  print("Incremental TP equations based only on actionable load components")
}

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\ndisplay {x in Bootstraps, s in seasons, y in years} (sum{c in comid_N",i,"} (inc_storage_load_N",i,"_rev[c,s,y,x]));"
        ),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "\n",
        append = TRUE
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\ndisplay {x in Bootstraps, s in seasons, y in years} (sum{c in comid_P",i,"} (inc_storage_load_P",i,"_rev[c,s,y,x]));"
        ),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "\n",
        append = TRUE
      )
    }
  }
)

# cat(
#   "
# display point_dec[*,*];
# 
# option display_1col 0;
# display urban_frac_by_comid_bmp;
# display road_frac_by_comid_bmp;
# display ripbuf_length[*,*];
# 
# option display_width 100000000000;
# display ag_frac[*,*];
# display graz_frac[*,*];
# 
# }
# 
# ",
# file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
# sep = "\n",
# append = TRUE
# )

#### CTC edited: 1/27/2026 ####

cat(
  paste0("\ndisplay seasons;\ndisplay years;
  \ndisplay point_dec;
         \noption display_1col 0; \ndisplay urban_frac_by_comid_bmp; \ndisplay road_frac_by_comid_bmp; \ndisplay ripbuf_length; \noption display_width 100000000000;
         \ndisplay ag_frac; \ndisplay graz_frac; \n}"),
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "\n",
  append = TRUE
)

#### end edits ####

#### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####

# If septic data is available, add septic-specific commands
if(length(Septic_BMPs) > 0 & septic_data_available) {
  cat(
    "display septic_frac;\n",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
    append = TRUE
  )
}

#### end edits ####

# Write Data Script ####

orig.dat <- read.table(
  file = paste(OutPath, "STdata_seasonal.dat", sep=""), 
  header = FALSE,
  sep = "\n",
  quote = ""
)

write.table(
  orig.dat, # selecting forces the order in case they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)

## Loads and transfer coefficients

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(inc_tn_dat)) %do% {
      write(
        paste0("\nparam baseloads_N", i, "_se : 'point' 'urban' 'road' 'ag' 'septic' 'graz' 'storage' 'other' :="), # CTC edited 2/6/2026
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
        append = T
      )
      write.table(
        inc_tn_dat_se[[i]] %>% 
          select(
            comid_form, season, year, point = sin_poin, urban = sin_urb, road = sin_road, ag = sin_ag, graz = sin_graz,
            septic = sin_septic, storage = sin_storage, other = sin_other # CTC edited: 2/6/2026
          ) %>%
          arrange(comid_form, season, year) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath,"STdata_dynamic_uncertainty.dat",sep = ""), append = T)
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(inc_tp_dat)) %do% {
      write(
        paste0("\nparam baseloads_P", i, "_se : 'point' 'urban' 'road' 'ag' 'graz' 'storage' 'other' :="), 
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
        append = T
      )
      write.table(
        inc_tp_dat_se[[i]] %>% 
          select(
            comid_form, season, year, point = sip_poin, urban = sip_urb, road = sip_road, ag = sip_ag, graz = sip_graz,
            storage = sip_storage, other = sip_other
          ) %>%
          arrange(comid_form, season, year) %>%
          unique(), # renaming also forces the order incase they are disordered in processing code above
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath,"STdata_dynamic_uncertainty.dat",sep = ""), append = T)
    }
  }
)

### TN Storage Coefficients ------
invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(sm_N_dat_se)) %do% {
      
      temp_sm_N_dat <- sm_N_dat_se[[i]]
      
      if (!is.null(temp_sm_N_dat) && nrow(temp_sm_N_dat) > 0) {
        
        # Write the header for the storage coefficients
        write(
          paste0("\nparam transfer_coefficients_N", i, "_se : 'coeff' :="),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
        
        # Write the data
        write.table(
          temp_sm_N_dat %>% 
            mutate(year = as.numeric(year)) %>%
            mutate(across(everything(), ~replace(., is.na(.), 0))),  # Replace NA with 0
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "",
          quote = FALSE
        )
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = TRUE)
      } else {
        # If the data is NULL or empty, write a message
        write(
          paste0("\nparam transfer_coefficients_N", i, "_se : 'coeff' :=\n;"),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
      }
    }
  }
)

### TP Storage Coefficients -----
invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(sm_P_dat_se)) %do% {
      
      temp_sm_P_dat <- sm_P_dat_se[[i]]
      
      if (!is.null(temp_sm_P_dat) && nrow(temp_sm_P_dat) > 0) {
        
        # Write the header for the storage coefficients
        write(
          paste0("\nparam transfer_coefficients_P", i, "_se : 'coeff' :="),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
        
        # Write the data
        write.table(
          temp_sm_P_dat %>% 
            mutate(year = as.numeric(year)) %>%
            mutate(across(everything(), ~replace(., is.na(.), 0))),  # Replace NA with 0
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "",
          quote = FALSE
        )
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = TRUE)
      } else {
        # If the data is NULL or empty, write a message
        write(
          paste0("\nparam transfer_coefficients_P", i, "_se : 'coeff' :=\n;"),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
      }
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(riparian_tn_dat)) %do% {
      if (!is.null(riparian_tn_dat[[i]]) && nrow(riparian_tn_dat[[i]]) > 0) {
        temp_ripar <- riparian_tn_dat[[i]]
        write(
          paste0("\nparam riparianload_N", i, "_se : 'riparian' :="), 
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = T
        )
        
        formatted_data <- temp_ripar %>%
          select(comid_form, season, year, riparian = sin_riparian) %>% # renaming also forces the order in case they are disordered in processing code above
          arrange(comid_form, season, year) %>%
          unique()  %>%
          # replace NA values with 0
          mutate(riparian = ifelse(is.na(riparian), 0, riparian)) 
        
        if(length(RiparianBuffer_BMPs) > 0) {
          write.table(
            formatted_data,
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
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
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        write( ";", file = paste(OutPath,"STdata_dynamic_uncertainty.dat",sep = ""), append = T) 
      }
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(riparian_tp_dat)) %do% {
      
      if (!is.null(riparian_tp_dat[[i]]) && nrow(riparian_tp_dat[[i]]) > 0) {
        
        temp_ripar <- riparian_tp_dat[[i]]
        
        # Write the header for the table
        write(
          paste0("\nparam riparianload_P", i, "_se : 'riparian' :="), #*#,
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
        
        # Prepare the data in the required format
        formatted_data <- temp_ripar %>%
          mutate(year = as.numeric(year)) %>%
          select(
            comid_form,
            season,
            year,
            riparian = sip_riparian
          ) %>%
          arrange(comid_form, season, year) %>%
          # replace NA values with 0
          mutate(riparian = ifelse(is.na(riparian), 0, riparian))
        
        if(length(RiparianBuffer_BMPs) > 0) {
          # Write the formatted data to the file
          write.table(
            formatted_data,
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
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
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        write( ";", file = paste(OutPath,"STdata_dynamic_uncertainty.dat",sep = ""), append = T)
      }
    }
  }
)

## Areas 

write( 
  "\n\nparam area_se : 'ag' 'graz' :=", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T
)
write.table(
  area_dat_se %>% select(comid_form, ag = ag_ac_se, graz = graz_ac_se), # renaming also forces the order in case they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)

write( 
  "\n\nparam urban_area_se : 'urban' :=", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T
)

# Get complete urban_area structure from base data and merge with SE values, filling missing with 0
urban_area_complete_se <- urban_area_dat %>%
  select(comid_form, Land_Use, HSG) %>%
  left_join(
    urban_area_dat_se %>% select(comid_form, Land_Use, HSG, urban_ac_se),
    by = c("comid_form", "Land_Use", "HSG")
  ) %>%
  mutate(urban_ac_se = replace_na(urban_ac_se, 0)) %>%
  arrange(comid_form, Land_Use, HSG) %>%
  unique()

write.table(
  urban_area_complete_se,
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)

write( 
  "\n\nparam road_area_se : 'road' :=", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T
)

# Get complete road_area structure from base data and merge with SE values, filling missing with 0
road_area_complete_se <- road_area_dat %>%
  select(comid_form, HSG) %>%
  left_join(
    road_area_se %>% select(comid_form, HSG, road_ac_se),
    by = c("comid_form", "HSG")
  ) %>%
  mutate(road_ac_se = replace_na(road_ac_se, 0)) %>%
  arrange(comid_form, HSG) %>%
  unique()

write.table(
  road_area_complete_se,
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)


## Efficiencies
invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      
      temp_ripar_rem <- riparian_tn_removal_se[[i]]
      
      if (!is.null(temp_ripar_rem) && nrow(temp_ripar_rem) > 0) {
        write(
          paste0(
            "\nparam riparianremoval_N", 
            i, 
            "_se : ", 
            paste(bmp_ripbuf_vec, collapse  = "  "), 
            " :="),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
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
          arrange(comid_form, season, year)
        
        # Write the formatted data to the file
        write.table(
          formatted_data,
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
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
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          ripbuf_bmp_dummy_tn[[i]] %>%
            select(comid, none),
          # renaming also forces the order incase they are disordered in preprocessing code above
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
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
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      
      temp_ripar_rem <- riparian_tp_removal_se[[i]]
      
      if (!is.null(temp_ripar_rem) && nrow(temp_ripar_rem) > 0) {
        write(
          paste0(
            "\nparam riparianremoval_P", 
            i, 
            "_se : ", 
            paste(bmp_ripbuf_vec, collapse  = "  "), 
            " :="),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
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
          arrange(comid_form, season, year)
        
        # Write the formatted data to the file
        write.table(
          formatted_data,
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
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
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = T
        )
      }
      write(
        ";",
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
        append = T
      )
    }
  }
)

if(length(Ag_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam ag_effic_N_se : ",bmp_ag_vec," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      ag_effic_dat_tn_se %>%
        select(comid_form, all_of(bmp_ag_vec_direct)) %>%
        arrange(comid_form) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam ag_effic_P_se : ",bmp_ag_vec," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      ag_effic_dat_tp_se %>%
        select(comid_form, all_of(bmp_ag_vec_direct)) %>%
        arrange(comid_form) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam ag_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tn)) %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam ag_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tp)) %>% select(comid, none),  # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
}

if(length(Graz_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam graz_effic_N_se : ",bmp_graz_vec," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      graz_effic_dat_tn_se %>% select(comid_form, any_of(Graz_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam graz_effic_P_se : ",bmp_graz_vec," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      graz_effic_dat_tp_se %>% select(comid_form, any_of(Graz_BMPs)),  # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam graz_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", graz_bmp_dummy_tn)) %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam graz_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", graz_bmp_dummy_tp)) %>% select(comid, none),  # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
}


if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_N_se : ",bmp_urban_vec," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      urban_effic_dat_tn_se %>%
               select(comid_form, season, year, Land_Use, HSG, all_of(bmp_urban_direct)) %>%
               arrange(comid_form, season, year, Land_Use, HSG)%>%
               unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tn) %>% 
               select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_P_se : ",bmp_urban_vec," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      urban_effic_dat_tp_se %>%
        select(comid_form, season, year, Land_Use, HSG, all_of(bmp_urban_direct)) %>%
        arrange(comid_form, season, year, Land_Use, HSG) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tp) %>% 
               select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(length(road_BMPs) > 0) {
    cat(
      "\nparam road_effic_N_se : ",bmp_road_vec," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      road_effic_dat_tn_se %>%
        select(comid_form, season, year, HSG, all_of(road_BMPs)) %>%
        arrange(comid_form, season, year, HSG)%>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam road_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", road_bmp_dummy_tn) %>% 
               select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(length(road_BMPs) > 0) {
    cat(
      "\nparam road_effic_P_se : ",bmp_road_vec," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      road_effic_dat_tp_se %>%
        select(comid_form, season, year, HSG, all_of(road_BMPs)) %>%
        arrange(comid_form, season, year, HSG)%>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam road_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", road_bmp_dummy_tp) %>% 
               select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}

#### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(length(Septic_BMPs) > 0) {
    cat(
      "\nparam septic_effic_N_se : ",paste(bmp_septic_vec, collapse  = "  ")," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(septic_effic_dat_tn_se %>% select(comid_form, all_of(bmp_septic_vec_direct))), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam septic_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", septic_bmp_dummy_tn) %>% 
               select(comid_form, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}

#### end edits ####

if (length(Point_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      cat(
        "\nparam point_effic_N_se : ",bmp_point_vec," :=	", 
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
      write.table(
        unique(point_effic_dat_tn_se %>% select(comid_form, all_of(Point_BMPs))), # selecting forces the order in case they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }

  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      cat(
        "\nparam point_effic_P_se : ",bmp_point_vec," :=	", 
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
      write.table(
        unique(point_effic_dat_tp_se %>% select(comid_form, all_of(Point_BMPs))), # selecting forces the order in case they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
      }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "\nparam point_effic_N_se : 'none' :=	", 
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    sep="", 
    append = T
  )
  cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
  write.table(
    unique(do.call("rbind", point_bmp_dummy_tn) %>% 
             select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
  
 if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "\nparam point_effic_P_se : 'none' :=	", 
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    sep="", 
    append = T
  )
  cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
  write.table(
    unique(do.call("rbind", point_bmp_dummy_tp) %>% 
             select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
 }
}

## Costs
if(length(RiparianBuffer_BMPs) > 0) {
  cat(
    "\nparam ripbuf_costs_capital_se : ", bmp_ripbuf_vec, " :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_cap_dat_se %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations_se : ", bmp_ripbuf_vec," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_op_dat_se %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam ripbuf_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
}

#### CTC edited: 2/6/2026 - transferred from dynamic LIS RBEROST ####

### Septic BMP Costs Standard Errors ------
if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(Septic_BMPs) > 0) {
    cat(
      paste0("\nparam septic_costs_capital_se : ",paste(bmp_septic_vec, collapse = " ")," :=	"),
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      septic_cap_costs_se %>% select(comid_form, all_of(bmp_septic_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
    
    cat(
      paste0("\nparam septic_costs_operations_se : ", paste(bmp_septic_vec, collapse = " ")," :=	"),
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      septic_op_costs_se %>% select(comid_form, all_of(bmp_septic_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam septic_costs_capital_se : 'none' :=	",
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      septic_bmp_dummy %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
    
    cat(
      "\nparam septic_costs_operations_se : 'none' :=	",
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      septic_bmp_dummy %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
}

#### end edits ####

if(length(Ag_BMPs) > 0) {
  cat(
    "\nparam ag_costs_capital_se : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_cap_dat_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ag_costs_operations_se : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_op_dat_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam ag_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ag_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
}

if(length(Graz_BMPs) > 0) {
  cat(
    "\nparam graz_costs_capital_se : ",bmp_graz_vec," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    graz_costs_cap_dat_se %>% select(comid_form, any_of(Graz_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam graz_costs_operations_se : ",bmp_graz_vec," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table( 
    graz_costs_op_dat_se %>% select(comid_form, any_of(Graz_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam graz_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    graz_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam graz_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    graz_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
}

if(length(Point_BMPs) > 0) {
  cat(
    "\nparam point_costs_capital_se : ",bmp_point_vec," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    point_costs_capital_dat_se %>% select(comid_form, any_of(Point_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam point_costs_operations_se : ",bmp_point_vec," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table( 
    point_costs_operations_dat_se %>% select(comid_form, any_of(Point_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam point_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    point_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam point_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    point_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
}

cat( 
  "\nparam urban_costs_capital_se :", bmp_urban_vec," :=	", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
  append = T
)
cat(
  "\n",
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
  sep = "", 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_costs_capital_se %>% 
      select(comid_form, Land_Use, HSG, all_of(bmp_urban_direct)) %>%
      arrange(comid_form, Land_Use, HSG) %>%
      mutate(across(where(is.list), unlist)) %>%
      unique(),
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    urban_bmp_dummy,
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)

cat( 
  "\nparam urban_costs_operations_se :", bmp_urban_vec," :=	", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
  append = T
)
cat(
  "\n",
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
  sep = "", 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_costs_operations_se %>% 
      select(comid_form, Land_Use, HSG, all_of(bmp_urban_direct)) %>%
      arrange(comid_form, Land_Use, HSG) %>%
      mutate(across(where(is.list), unlist)) %>%
      unique(),
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    urban_bmp_dummy,
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)

cat( 
  "\nparam road_costs_capital_se :", bmp_road_vec," :=	", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
  append = T
)
cat(
  "\n",
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
  sep = "", 
  append = T
)
if(length(road_BMPs) > 0) {
  write.table(
    road_costs_capital_se %>% 
      select(comid_form, HSG, all_of(bmp_road_direct)) %>%
      arrange(comid_form, HSG) %>%
      unique(),
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    road_bmp_dummy,
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)

cat( 
  "\nparam road_costs_operations_se :", bmp_road_vec," :=	", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
  append = T
)
cat(
  "\n",
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
  sep = "", 
  append = T
)
if(length(road_BMPs) > 0) {
  write.table(
    road_costs_operations_se %>%
      select(comid_form, HSG, all_of(bmp_road_direct)) %>%
      arrange(comid_form, HSG) %>%
      unique(),
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    road_bmp_dummy,
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)

# Write Model Script ####

orig.mod <- readLines(paste(OutPath, "STmodel_seasonal.mod", sep=""))

write.table(
  orig.mod, # selecting forces the order in case they are disordered in preprocessing code above
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = "") , 
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nparam baseloads_N", 
          i, 
          "_se {comid_N", 
          i, 
          ", seasons, years, loads_all} >= 0;"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      if (!is.null(param_loads_lim_tp[[i]]) && nrow(param_loads_lim_tp[[i]]) > 0) {
        cat(
          paste0(
            "\nparam baseloads_P", 
            i, 
            "_se {comid_P", 
            i, 
            ", seasons, years, loads_all} >= 0;"
          ),
          file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
          sep = "", 
          append = T
        )
      }
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0("\nparam riparianload_N", i, "_se {comid_N", i, ", seasons, years, loads_riparian} >= 0;"),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0("\nparam riparianload_P", i, "_se {comid_P", i, ", seasons, years, loads_riparian} >= 0;"),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  foreach(i = 1:length(sm_N_dat_se)) %do% {
    cat(
      paste0("param transfer_coefficients_N", i, "_se {comid_N", i, ", seasons, years, coeff};"),
      file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
  }
)

cat("\n", file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""), append = T)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(sm_P_dat_se)) %do% {
      cat(
        paste0("param transfer_coefficients_P", i, "_se {comid_P", i, ", seasons, years, coeff};"),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
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
        paste0(
          "param riparianremoval_N", 
          i, 
          "_se {c in comid_N", 
          i, 
          ", s in seasons, y in years, b in ripbuf_bmp};"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0(
          "param riparianremoval_P", 
          i, 
          "_se {c in comid_P", 
          i, 
          ", s in seasons, y in years, b in ripbuf_bmp};"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

#### CTC edited: 2/6/2026 ####

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
param ag_effic_N_se {comid_all_N ,ag_bmp} >= 0;
param urban_effic_N_se {comid_all_N, seasons, years, urban_land_uses, urban_soil_groups, urban_bmp} >= 0;
param road_effic_N_se {comid_all_N, seasons, years, road_soil_groups, road_bmp} >= 0;
param point_effic_N_se {comid_all_N, point_bmp} >= 0;
param septic_effic_N_se {comid_all_N, septic_bmp} >= 0;
param graz_effic_N_se {comid_all_N, graz_bmp} >= 0;",
    file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
  
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
param ag_effic_P_se {comid_all_P ,ag_bmp} >= 0;
param urban_effic_P_se {comid_all_P, seasons, years, urban_land_uses, urban_soil_groups, urban_bmp} >= 0;
param road_effic_P_se {comid_all_P, seasons, years, road_soil_groups, road_bmp} >= 0;
param point_effic_P_se {comid_all_P, point_bmp} >= 0;
param graz_effic_P_se {comid_all_P, graz_bmp} >= 0;",
    file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}
cat(
  "
param ripbuf_costs_capital_se {comid_all, ripbuf_bmp};
param ripbuf_costs_operations_se {comid_all, ripbuf_bmp};

param area_se {comid_all,area_sub} >=0;
param urban_area_se {comid_all, urban_land_uses, urban_soil_groups, urban_area_sub} >=0;
param road_area_se {comid_all, road_soil_groups, road_area_sub} >=0;

param ag_costs_capital_se {comid_all,ag_bmp};
param ag_costs_operations_se {comid_all,ag_bmp};

param urban_costs_capital_se {comid_all, urban_land_uses, urban_soil_groups, urban_bmp};
param urban_costs_operations_se {comid_all, urban_land_uses, urban_soil_groups, urban_bmp};

param road_costs_capital_se {comid_all, road_soil_groups, road_bmp};
param road_costs_operations_se {comid_all, road_soil_groups, road_bmp};

param graz_costs_capital_se {comid_all,graz_bmp};
param graz_costs_operations_se {comid_all,graz_bmp};

param point_costs_capital_se {comid_all,point_bmp};
param point_costs_operations_se {comid_all,point_bmp};

param septic_costs_capital_se {comid_all_N, septic_bmp} >= 0;
param septic_costs_operations_se {comid_all_N, septic_bmp} >= 0;

",
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

#### end edits ####

cat(
  paste0("set Scenarios := 1 .. ", n.scenarios, " by 1;"),
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

cat(
  "
set Bootstraps := 1 .. 10 by 1;
",
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nparam baseloads_N", 
          i, 
          "_rev {comid_N", 
          i, 
          ", seasons, years, loads_all, Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\nparam baseloads_P", 
          i, 
          "_rev {comid_P", 
          i, 
          ", seasons, years, loads_all, Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nparam riparianload_N", 
          i, 
          "_rev {comid_N",i,", seasons, years, loads_riparian, Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\nparam riparianload_P", 
          i, 
          "_rev {comid_P",i,",seasons, years, loads_riparian, Bootstraps};"
        ),   
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(sm_N_dat)) %do% {
      cat(
        paste0("param transfer_coefficients_N", i, "_rev {comid_N", i, ", seasons, years, coeff, Bootstraps};"),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

cat("\n", file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""), append = T)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(sm_P_dat)) %do% {
      cat(
        paste0("param transfer_coefficients_P", i, "_rev  {comid_P", i, ", seasons, years, coeff, Bootstraps};"),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

#### CTC edited: 2/6/2026 ####

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
param ag_effic_N_rev {comid_all_N ,ag_bmp, Bootstraps};
param point_effic_N_rev {comid_all_N, point_bmp, Bootstraps};
param urban_effic_N_rev {comid_all_N, seasons, years, urban_land_uses, urban_soil_groups, urban_bmp, Bootstraps};
param road_effic_N_rev {comid_all_N, seasons, years, road_soil_groups, road_bmp, Bootstraps};
param septic_effic_N_rev {comid_all_N, septic_bmp, Bootstraps};
param graz_effic_N_rev {comid_all_N, graz_bmp, Bootstraps};",
    file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
param ag_effic_P_rev {comid_all_P ,ag_bmp, Bootstraps};
param point_effic_P_rev {comid_all_P, point_bmp, Bootstraps};
param urban_effic_P_rev {comid_all_P, seasons, years, urban_land_uses, urban_soil_groups, urban_bmp, Bootstraps};
param road_effic_P_rev {comid_all_P, seasons, years, road_soil_groups, road_bmp, Bootstraps};
param graz_effic_P_rev {comid_all_P, graz_bmp, Bootstraps};",
    file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}
cat(
  "
param area_rev {comid_all,area_sub, Bootstraps};
param urban_area_rev {comid_all,urban_land_uses, urban_soil_groups,urban_area_sub, Bootstraps};
param road_area_rev {comid_all, road_soil_groups,road_area_sub, Bootstraps};

param ag_costs_capital_rev {comid_all,ag_bmp, Bootstraps};
param ag_costs_operations_rev {comid_all,ag_bmp, Bootstraps};

param septic_costs_capital_rev {comid_all_N, septic_bmp, Bootstraps};
param septic_costs_operations_rev {comid_all_N, septic_bmp, Bootstraps};

param point_costs_capital_rev {comid_all,point_bmp, Bootstraps};
param point_costs_operations_rev {comid_all,point_bmp , Bootstraps};

param urban_costs_capital_rev {comid_all, urban_land_uses, urban_soil_groups, urban_bmp, Bootstraps};
param urban_costs_operations_rev {comid_all, urban_land_uses, urban_soil_groups, urban_bmp, Bootstraps};

param road_costs_capital_rev {comid_all, road_soil_groups, road_bmp, Bootstraps};
param road_costs_operations_rev {comid_all, road_soil_groups, road_bmp, Bootstraps};

param ripbuf_costs_capital_rev {comid_all, ripbuf_bmp, Bootstraps};
param ripbuf_costs_operations_rev {comid_all, ripbuf_bmp, Bootstraps};

param graz_costs_capital_rev {comid_all, graz_bmp, Bootstraps};
param graz_costs_operations_rev {comid_all, graz_bmp, Bootstraps};
",
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_N", 
          i, 
          "_rev {c in comid_N", 
          i, 
          ", s in seasons, y in years, b in ripbuf_bmp, Bootstraps};"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_P", 
          i, 
          "_rev {c in comid_P", 
          i, 
          ", s in seasons, y in years, b in ripbuf_bmp, Bootstraps};"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

cat(
  "
param agcost_frac_rev {Bootstraps} >=0;

param ps_coef_rev {c in comid_all, p in point_bmp, x in Bootstraps} := point_costs_capital_rev[c,p,x] + point_costs_operations_rev[c,p,x];
param septic_coef_rev {c in comid_all_N, e in septic_bmp, x in Bootstraps} := septic_costs_capital_rev[c,e,x] + septic_costs_operations_rev[c,e,x];
param urban_coef_rev {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp, x in Bootstraps} := urban_area_rev[c,d,n,'urban',x] * (urban_costs_capital_rev[c,d,n,u,x] + urban_costs_operations_rev[c,d,n,u,x]);
param road_coef_rev {c in comid_all, h in road_soil_groups, r in road_bmp, x in Bootstraps} := road_area_rev[c,h,'road',x] * (road_costs_capital_rev[c,h,r,x] + road_costs_operations_rev[c,h,r,x]);
param ag_coef_rev {c in comid_all, a in ag_bmp, x in Bootstraps} := agcost_frac_rev[x] * area_rev[c,'ag',x] *  (ag_costs_capital_rev[c,a,x] + ag_costs_operations_rev[c,a,x]) ; 
param ripbuf_coef_rev {c in comid_all, b in ripbuf_bmp, x in Bootstraps} := agcost_frac_rev[x] * (ripbuf_costs_capital_rev[c,b,x] +  ripbuf_costs_operations_rev[c,b,x]);
param graz_coef_rev {c in comid_all, g in graz_bmp, x in Bootstraps} := agcost_frac_rev[x] * area_rev[c,'graz',x] * (graz_costs_capital_rev[c,g,x] + graz_costs_operations_rev[c,g,x]) ;


",
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

#### end edits ####

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      write(
        paste0(
          "\nvar inc_storage_load_N", 
          i, 
          "_rev {c in comid_N", 
          i, 
          ", s in seasons, y in years, x in Bootstraps} >= 0;"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      write(
        paste0(
          "\nvar inc_storage_load_P", 
          i, 
          "_rev {c in comid_P", 
          i, 
          ", s in seasons, y in years, x in Bootstraps} >= 0;"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)


print(
  paste(
    "RBEROST has finished writing AMPL scripts with uncertainty at",
    Sys.time()
  )
)

