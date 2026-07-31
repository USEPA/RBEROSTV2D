# ND 6/23/26 Modified to use future package in R since doParallel is now blocked by Windows firewall
# ND 6/23/26 Modified to use dynamic SPARROW model output to estimate season x year urban riparian loadings
# 8. Setup -----

packages <- c('tidyverse', 'foreach', 'doParallel', 'sf', 'data.table', 'purrr', 'tidyr', 'units',"future", "future.apply", "dplyr", "doFuture","futurize")
lapply(packages, install.packages) #*# # Run this code if packages are not installed
lapply(packages, library, character.only = T)

# Create a cluster with 12 cores - blocked by Windows firewall
#cl <- makeCluster(12)

# Register the cluster for parallel processing - blocked by windows firewall
#registerDoParallel()

working_dir <- "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/Tier_1_Optimization-SSWR.5.3.2/"

InPath<-paste0(working_dir, "RBEROST-Pacific/Preprocessing/Inputs/")

# source helper functions
source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R") 

# 9. Load Data -----
datafolder <- "./Data/PS_RiparianData/" #*#
NHD_data_folder <- "./Data/nhdplus_17/" #*# NHD plus data provided by Sam Ennett, ICF

### Import Data ----- 
PS_flowlines <- read_sf(
  paste0(NHD_data_folder, "NHDFlowline.shp"))

PS_catchments <- read_sf(
  paste0(NHD_data_folder, "Catchment.shp")) 

# ND 6/22/26 Calculate urban area per catchment
# Select urban area and incremental area from SPARROW input data
# NEW USGS SPARROW DATA (DYNAMIC) https://www.sciencebase.gov/catalog/item/65860a06d34eff134d437200
sparrow_in_DYN <- fread(paste(InPath,"data1_puget.csv",sep="")) #*#
temp_sparrow_area <- sparrow_in_DYN %>% 
  select(comid, year, quarter, IncAreaKm2, urb) %>%
#  filter(year == 2019, quarter == 1) %>%
  mutate(urb = ifelse(is.na(urb), 0, urb),
         urban_km2 = IncAreaKm2 * (urb / 100)) %>%
  mutate(IncAreaKm2 = set_units(IncAreaKm2, "km^2"),
         urban_km2 = set_units(urban_km2, "km^2"))

## SPARROW data
# ND 6/22/26 Add urban data from SPARROW in place of urban loading coefficients from R1
sparrow_tn_out_tmp <- fread(
  paste("./RBEROST-Pacific/Preprocessing/Inputs/predict_puget_tn.csv",sep=""), data.table = FALSE
) #*#

sparrow_tp_out_tmp <- fread(
  paste("./RBEROST-Pacific/Preprocessing/Inputs/predict_puget_tp.csv",sep=""), data.table = FALSE
) #*#

sparrow_tn_out <- sparrow_tn_out_tmp %>%
  mutate(
    il_tn_fer = PLOAD_INC_AG_KG + PLOAD_INC_CAFOS,
    ilse_tn_fer = my_propogateerror(
      vals = list(
        c(PLOAD_INC_AG_KG, SE_PLOAD_INC_AG_KG), 
        c(PLOAD_INC_CAFOS, SE_PLOAD_INC_CAFOS)
      ),
      method = "addsub"
    )
  )
# ND 6/22/26
sparrow_tn_urb <- sparrow_tn_out_tmp %>%
  select(comid,year,quarter,PLOAD_INC_URB_KM2,SE_PLOAD_INC_URB_KM2) %>%
  left_join(temp_sparrow_area,by = c("comid","year","quarter")) %>% 
  mutate(TNKG_URB_PER_KM2 = PLOAD_INC_URB_KM2/urban_km2) %>%
  select(comid,year,quarter,PLOAD_INC_URB_KM2,SE_PLOAD_INC_URB_KM2,TNKG_URB_PER_KM2, urban_km2) %>%
  mutate(cv_numpixels = 0.17) %>%
  mutate(urban_km2_se = urban_km2 * cv_numpixels) %>%
  mutate(TNKG_URB_PER_KM2_SE = my_propogateerror(
    vals = list(
      c(PLOAD_INC_URB_KM2,SE_PLOAD_INC_URB_KM2),
      c(urban_km2,urban_km2_se)),
    method = "div"
  )
  )
  

sparrow_tp_out <- sparrow_tp_out_tmp %>%
  mutate(
    il_tp_fer = PLOAD_INC_AG_KG + PLOAD_INC_CAFOS,
    ilse_tp_fer = my_propogateerror(
      vals = list(
        c(PLOAD_INC_AG_KG, SE_PLOAD_INC_AG_KG), 
        c(PLOAD_INC_CAFOS, SE_PLOAD_INC_CAFOS)
      ),
      method = "addsub"
    )
  ) 

# ND 6/22/26
sparrow_tp_urb <- sparrow_tp_out_tmp %>%
  select(comid,year,quarter,PLOAD_INC_URB_KM2,SE_PLOAD_INC_URB_KM2) %>%
  left_join(temp_sparrow_area,by = c("comid","year","quarter")) %>% 
  mutate(TPKG_URB_PER_KM2 = PLOAD_INC_URB_KM2/urban_km2) %>%
  select(comid,year,quarter,PLOAD_INC_URB_KM2,SE_PLOAD_INC_URB_KM2,TPKG_URB_PER_KM2, urban_km2) %>%
  mutate(cv_numpixels = 0.17) %>%
  mutate(urban_km2_se = urban_km2 * cv_numpixels) %>%
  mutate(TPKG_URB_PER_KM2_SE = my_propogateerror(
         vals = list(
           c(PLOAD_INC_URB_KM2,SE_PLOAD_INC_URB_KM2),
           c(urban_km2,urban_km2_se)),
         method = "div"
       )
)

land_covers <- fread(paste0(datafolder,"landcovers_bycomid_tmp.csv")) #*#


ftperkm <- 3280.84

PS_flowlines <- read_sf(
  paste0(NHD_data_folder, "NHDFlowline.shp")) %>%
  mutate(lengthkm_se = sqrt(
    (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + # error in each direction for each point consists of a 90% tolerance of 0.02 in (to map scale). The digitized maps have a tolerance of 0.003 in (to map scale). Not knowing exactly how these tolerances interact, I will assume that the uncertainty is similar to if they were added together. The model is using the medium resolution NHD+ data, which has a resolution of 1:100,000. Inches are then converted to kilometers.
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + 
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + 
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 )
  ) # error in stream length depends on error in the location of the start and end point, in the x and y directions, so 4 total components.


# Slope and NLCD data are read in below in parallel processing. Users can edit file paths tagged with #*#


# 10. Summarize landcover by buffer widths ####

buffer.options <- c(20, 40, 60, 80, 100, 300, 400)

catchment_crosswalk <-PS_catchments %>%
  select(id = GRIDCODE, comid = FEATUREID)
st_geometry(catchment_crosswalk) <- NULL
# Due to memory constraints the earlier Model Builder approach which
# used zonal histograms to characterize land-use in riparian zones has
# been updated to use Zonal Statistics as table instead. 

# Process output files from Tabulate Area in SAS and export as csv then import here


landcovers_bycomid_tmp <- land_covers %>%
  merge(., catchment_crosswalk, by.x = c("COMID"), by.y = "id") %>%
  select(-COMID) %>%
  rename(COMID=comid) %>%
  filter(!is.na(COMID)) %>%
  mutate(landcover = case_when(
    landcover == "Developed, Open Space" ~ "Lowdev", 
    landcover == "Developed, Low Intensity" ~ "Meddev",
    landcover == "Developed, Medium Intensity/Developed, High Intensity" ~ 
      "Highdev",
    landcover == "Deciduous Forest/Evergreen Forest/Mixed Forest" ~
      "Forest",
    landcover == "Herbaceuous" ~ "Grass",
    landcover == "Hay/Pasture" ~ "Grazing",
    landcover == "Cultivated Crops" ~ "Cropland")
  )    

# Pivot the data to wide form for easier human readibility 

landcovers_bycomid <- landcovers_bycomid_tmp %>%
  tidyr::complete(COMID, buffersize, landcover) %>% 
  group_by(COMID, landcover) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "COMID", 
    names_from = c("landcover", "buffersize"), 
    values_from = "value"
  ) %>%
  rename_with(
    .fn = ~paste0(
      lapply(str_split(., "_"), "[[", 1), 
      "_", 
      buffer.options[as.numeric(lapply(str_split(., "_"), "[[", 2))], 
      "ft"
    ), 
    .cols = !COMID
  ) %>%
  mutate(cv_numpixels = 0.17) # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)

# 11. Choose slope category for each comid ####
# NOTE: ALL SLOPES (% X 100) ARE < 500 (SO IN 0-5% CATEGORY) IN THIS
# DATASET SO THIS SECTION CAN BE GREATLY SIMPLIFIED --- AK: Simplification could be a problem if other values are ever introduced. 

# This parallel process will load each slope data file from the data folder, format it as necessary, then combine all the slope data

# ND 6/23/26 Function definition adapted from EPA AI tool
plan(multisession, workers = 3) 

slopes_bycomid_future <- function(datafolder, buffer.options, idx = 1:5) {
  # idx: which indices to process (defaults to 1:5 as in your foreach)
  # buffer.options: a vector with at least length max(idx)
  # datafolder: path prefix for the files
  
  results <- future.apply::future_lapply(idx, function(i) {
    j <- i
    dat <- read.csv(
      paste0(datafolder, "slope_", buffer.options[j], "ft_bycomid-Pacific.txt")
    )
    
    dplyr::mutate(
      dat,
      value = dplyr::case_when(
        dplyr::between(MEAN, 0, 500) ~ "0-5%",
        dplyr::between(MEAN, 500, 1000) ~ "5-10%",
        dplyr::between(MEAN, 1000, 1500) ~ "10-15%",
        MEAN/100 > 1500 ~ ">15%"
      ),
      buffersize = i
    ) |>
      dplyr::select(comid = featurd, value, buffersize)
  })
  
  # Combine into a single data frame
  dplyr::bind_rows(results)
}

slopes_bycomid <- slopes_bycomid_future(
  datafolder = datafolder,
  buffer.options = c(20, 40, 60, 80, 100, 300, 400)
)
head(slopes_bycomid)

# NOTE: 300 and 400 ft slope files missing

# buffer.options <- c(20, 40, 60, 80, 100, 300, 400)
# 
# slopes_bycomid <- foreach(
#   i = 1:5, .combine = "rbind"
# ) %dopar% {
#   
#   j <- i
#   
#   dat <- read.csv(
#     paste0(datafolder, "slope_", buffer.options[j], "ft_bycomid-Pacific.txt") #*#
#   )
#   
#   dat.munged <- dat %>%
#     mutate(
#       value = case_when(
#         between(MEAN, 0, 500) ~ "0-5%",
#         between(MEAN, 500, 1000) ~ "5-10%",
#         between(MEAN, 1000, 1500) ~ "10-15%",
#         MEAN/100 > 1500 ~ ">15%"
#       ),
#       buffersize = i
#     ) %>%
#     select(comid = featurd, value, buffersize)
#   
# }  

# slopes_bycomid_dist_pre <- foreach(
#   i = 1:5, .combine = "rbind"
# ) %dopar% {
#   
#   j <- i
#   
#   dat <- read.csv(
#     paste0(datafolder, "slope_", buffer.options[j], "ft_bycomid-Pacific.txt") #*#
#   )
#   # NOTE: Slopes in original grid were in percent slope x 100 so need to divide by 100;
#   dat.munged <- dat %>%
#     
#     rowwise() %>%
#     mutate(resampledslopes = list(rnorm(n = 10, mean = MEAN, sd = STD))) %>%
#     mutate(
#       value = list(
#         case_when(
#           resampledslopes < 500 ~ "0-5%",
#           between(resampledslopes, 500, 1000) ~ "5-10%",
#           between(resampledslopes, 1000, 1500) ~ "10-15%",
#           resampledslopes > 1500 ~ ">15%"
#         )
#       ),
#       buffersize = paste0("buff_",i)
#     ) %>%
#     select(comid = featurd, value, buffersize)  
# 
# } 

slopes_bycomid_dist_pre_future <- function(
    datafolder,
    buffer.options,
    idx = 1:5,
    n_resamples = 10,
    scale_by100 = FALSE,         # If TRUE, divide resampled slopes by 100 and use thresholds 5/10/15
    comid_col = "featurd"        # Column name in the file that should be renamed to 'comid'
) {
  results <- future.apply::future_lapply(idx, function(i) {
    # Read data for this buffer option
    dat <- read.csv(
      paste0(datafolder, "slope_", buffer.options[i], "ft_bycomid-Pacific.txt")
    )
    
    # Choose thresholds based on scaling preference
    thr <- if (scale_by100) c(5, 10, 15) else c(500, 1000, 1500)
    
    # Build row-wise list columns of resampled values and their class labels
    out <- dat |>
      dplyr::rowwise() |>
      dplyr::mutate(
        resampledslopes = list({
          rs <- stats::rnorm(n = n_resamples, mean = MEAN, sd = STD)
          if (scale_by100) rs <- rs / 100
          rs
        }),
        value = list(dplyr::case_when(
          resampledslopes < thr[1] ~ "0-5%",
          dplyr::between(resampledslopes, thr[1], thr[2]) ~ "5-10%",
          dplyr::between(resampledslopes, thr[2], thr[3]) ~ "10-15%",
          resampledslopes > thr[3] ~ ">15%"
        )),
        buffersize = paste0("buff_", i)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(comid = dplyr::all_of(comid_col), value, buffersize)
    
    out
  })
  
  # Combine all buffers into a single data frame
  dplyr::bind_rows(results)
}

slopes_bycomid_dist_pre <- slopes_bycomid_dist_pre_future(
  datafolder = datafolder,
  buffer.options = c(20, 40, 60, 80, 100, 300, 400),
  idx = 1:5,
  n_resamples = 10,
  scale_by100 = FALSE,         # set TRUE if you want to classify on 5/10/15 percent instead of 500/1000/1500
  comid_col = "featurd"        # change if column name differs
)

head(slopes_bycomid_dist_pre)

# slopes_bycomid_dist <- slopes_bycomid_dist_pre %>%
#   as.data.frame() %>%
#   tidyr::complete(comid, buffersize) %>%
#   group_by(comid) %>%
#   arrange(buffersize) %>%
#   fill(value, .direction = "downup") %>%
#   pivot_wider(
#     id_cols = "comid", names_from = "buffersize", values_from = "value"
#   ) %>%
#   rename_with(
#     .fn = ~paste0("meanslope_", c(1:5) * 20, "ft"),
#     .cols = all_of(c("buff_1", "buff_2", "buff_3", "buff_4", "buff_5"))
#   )

# Sequential (simple, recommended):
slopes_bycomid_dist_finalize <- function(
    slopes_bycomid_dist_pre,
    buffers_levels = paste0("buff_", 1:5),
    distances_ft = seq_along(buffers_levels) * 20
) {
  # Ensure buffersize is ordered as desired
  dat <- slopes_bycomid_dist_pre %>%
    as.data.frame() %>%
    mutate(buffersize = factor(buffersize, levels = buffers_levels)) %>%
    tidyr::complete(comid, buffersize = buffers_levels) %>%
    group_by(comid) %>%
    arrange(buffersize, .by_group = TRUE) %>%
    tidyr::fill(value, .direction = "downup") %>%
    tidyr::pivot_wider(
      id_cols = "comid",
      names_from = "buffersize",
      values_from = "value"
    )
  
  # Rename buff_* columns to meanslope_*ft using the supplied distances
  dat %>%
    rename_with(
      .fn = function(nm) {
        idx <- match(nm, buffers_levels)
        ifelse(is.na(idx), nm, paste0("meanslope_", distances_ft[idx], "ft"))
      },
      .cols = dplyr::all_of(buffers_levels)
    ) %>%
    ungroup()
}

# Optional: "futurized" variant that processes each comid in parallel
# (use when you have very large data and many comids)
# slopes_bycomid_dist_finalize_future <- function(
#     slopes_bycomid_dist_pre,
#     buffers_levels = paste0("buff_", 1:5),
#     distances_ft = seq_along(buffers_levels) * 20
# ) {
#   # Requires future + future.apply packages
#   stopifnot(requireNamespace("future.apply", quietly = TRUE))
#   
#   dat <- slopes_bycomid_dist_pre %>%
#     as.data.frame() %>%
#     mutate(buffersize = factor(buffersize, levels = buffers_levels))
#   
#   # Split/combine per comid using futures
#   comids <- unique(dat$comid)
#   results <- future.apply::future_lapply(comids, function(cid) {
#     grp <- dat %>% filter(comid == cid)
#     grp %>%
#       tidyr::complete(comid = cid, buffersize = buffers_levels) %>%
#       arrange(buffersize) %>%
#       tidyr::fill(value, .direction = "downup") %>%
#       tidyr::pivot_wider(
#         id_cols = "comid",
#         names_from = "buffersize",
#         values_from = "value"
#       )
#   })
#   
#   out <- dplyr::bind_rows(results)
#   
#   out %>%
#     rename_with(
#       .fn = function(nm) {
#         idx <- match(nm, buffers_levels)
#         ifelse(is.na(idx), nm, paste0("meanslope_", distances_ft[idx], "ft"))
#       },
#       .cols = dplyr::all_of(buffers_levels)
#     )
# }

# simple - fast enough without parallelization
final_df <- slopes_bycomid_dist_finalize(slopes_bycomid_dist_pre)

plan(sequential) # resets number of cores to default

# 12. Summarize Hydrologic Soil Group by buffer widths ####

# This parallel process will load each NATSGO data file from the data folder, format it as necessary, then combine all the NATSGO data

# This parallel process will load each NATSGO data file from the data folder, format it as necessary, then combine all the NATSGO data

# hsg_bycomid_tmp <- foreach(
#   i = 1:5, .combine = "rbind", .packages = "data.table"
# ) %do% {
#   
#   j <- i
#   
#   dat <- fread(
#     paste0(datafolder, "STATSGO2_", buffer.options[j], "ft_bycomid-Pacific.csv") #*#
#   )
#   
#   dat.munged <- dat %>% 
#     rename(comid = featureid) %>%
#     select(-V1) %>%
#     pivot_longer(cols = -comid, names_to = "HSG") %>%
#     mutate(buffersize = i)
#   
#   dat.munged
#   
# } 

hsg_bycomid_tmp_future <- function(datafolder, buffer.options, idx = 1:5) {
  results <- future.apply::future_lapply(idx, function(i) {
    # Read the per-buffer file
    dat <- data.table::fread(
      paste0(datafolder, "STATSGO2_", buffer.options[i], "ft_bycomid-Pacific.csv")
    )
    
    # Munge: rename, drop V1 if present, long format, add buffer label
    dat.munged <- dat %>%
      dplyr::rename(comid = featureid) %>%
      dplyr::select(-dplyr::any_of("V1")) %>%
      tidyr::pivot_longer(cols = -comid, names_to = "HSG") %>%
      dplyr::mutate(buffersize = i)
    
    dat.munged
  })
  
  dplyr::bind_rows(results)
}

plan(multisession, workers = 3)  

hsg_bycomid_tmp <- hsg_bycomid_tmp_future(
  datafolder = datafolder,
  buffer.options = c(20, 40, 60, 80, 100, 300, 400),
  idx = 1:5
)

head(hsg_bycomid_tmp)

plan(sequential)

# Pivot the data wider to make it more human - readable
buffer.options = c(20, 40, 60, 80, 100, 300, 400)
hsg_bycomid <- hsg_bycomid_tmp %>%
  tidyr::complete(comid, buffersize, HSG) %>%
  group_by(comid, HSG) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "comid", 
    names_from = c("HSG", "buffersize"), 
    values_from = "value"
  ) %>%
  rename_with(
    .fn = ~paste0(
      lapply(str_split(., "_"), "[[", 1), 
      "_", 
      buffer.options[as.numeric(lapply(str_split(., "_"), "[[", 2))], 
      "ft"
    ), 
    .cols = !comid
  )

hsg_bycomid_toresample <- hsg_bycomid_tmp  %>%
  tidyr::complete(comid, buffersize, HSG) %>%
  group_by(comid, HSG) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  ungroup() %>%
  group_by(comid, buffersize) %>%
  summarize(resampledist = list(c(rep(x = HSG, times = floor(value / 100))))) # spatial resolution here is 1 m^2, which creates unnecessarily long lists. Down sampled to 10m x 10m "pixels

# 13. Compute bank lengths for each comid ####

river.lengths <- PS_flowlines %>%
  rename(comid=COMID,
         lengthkm=LENGTHKM,
         reachcode=REACHCODE) %>%
  select(comid, lengthkm, lengthkm_se, reachcode) %>%
  mutate(
    totalbanklength_km = lengthkm * 2, 
    totalbanklength_ft = totalbanklength_km * ftperkm,
    totalbanklength_km_se = lengthkm_se * 2,
    totalbanklength_ft_se = totalbanklength_km_se * ftperkm
  ) 

st_geometry(river.lengths) <- NULL

# 14. Calculate Uncertainty in PLER ####

## Table 2 in Green_Credit_Report_Final.pdf gives Pollutant Load Export Rates (PLER) by land use. 
## The values are based on Table 3-1 in Attachment 3 of Appendix F of the 2017 NH MS4 permit
## https://www.epa.gov/npdes-permits/new-hampshire-small-ms4-general-permit

# NEED TO UPDATE FOR PUGET SOUND BASIN
P.export.rates <- data.frame(
  P_SourceCategory_LandUse = c(
    "COM+IND", 
    "MFR+HDR", 
    "MDR", 
    "LDR",
    "HWY", 
    "FOR", 
    "OPEN", 
    "AG", 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C", 
    "DevPERV_CD",
    "DevPERV_D"
  ),
  Ploadexport_lb_acreyear = c(
    1.78, 2.32, 1.96, 1.52, 1.34, 1.52, 1.52, 1.52, 0.03, 0.12, 0.21, 0.29, 0.37
  ),
  LandUse = c(
    "Commercial+Transportation", 
    "Residential", 
    "Residential", 
    "LowResidential",
    "Commercial+Transportation", 
    NA_character_, 
    NA_character_, 
    NA_character_, 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C",
    "DevPERV_CD",
    "DevPERV_D"
  )
) %>%
  pivot_wider(
    id_cols = LandUse, 
    names_from = P_SourceCategory_LandUse, 
    values_from = Ploadexport_lb_acreyear
  ) %>%
  fill(contains("DevPERV"), .direction = "updown") %>%
  select(-c(FOR, OPEN, AG)) %>%
  pivot_longer(
    cols = -c("LandUse", contains("DevPERV")), 
    names_to = "P_SourceCategory_LandUse", 
    values_to = "Ploadexport_lb_acreyear"
  ) %>%
  drop_na() %>%
  pivot_longer(
    cols = contains("DevPERV"), 
    names_to = "DevPERV_P_SourceCategory_LandUse", 
    values_to = "DevPERV_Ploadexport_lb_acreyear"
  ) 

## The values are based on Table 3-2 in Attachment 3 of Appendix F of the 2017 NH MS4 permit
## https://www.epa.gov/npdes-permits/new-hampshire-small-ms4-general-permit

N.export.rates <- data.frame(
  N_SourceCategory_LandUse = c(
    "COM+IND", 
    "AllRes", 
    "AllRes", 
    "HWY", 
    "FOR", 
    "OPEN", 
    "AG", 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C", 
    "DevPERV_CD",
    "DevPERV_D"
  ),
  Nloadexport_lb_acreyear = c(
    15, 14.1, 14.1, 10.5, 11.3, 11.3, 11.3, 0.3, 1.2, 2.4, 3.1, 3.6
  ),
  LandUse = c(
    "Commercial+Transportation", 
    "Residential", 
    "LowResidential",
    "Commercial+Transportation", 
    NA_character_, 
    NA_character_, 
    NA_character_, 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C",
    "DevPERV_CD",
    "DevPERV_D"
  )
) %>%
  pivot_wider(
    id_cols = LandUse, 
    names_from = N_SourceCategory_LandUse, 
    values_from = Nloadexport_lb_acreyear
  ) %>%
  fill(contains("DevPERV"), .direction = "updown") %>%
  select(-c(FOR, OPEN, AG)) %>%
  pivot_longer(
    cols = -c("LandUse", contains("DevPERV")), 
    names_to = "N_SourceCategory_LandUse", 
    values_to = "Nloadexport_lb_acreyear"
  ) %>%
  drop_na() %>%
  pivot_longer(
    cols = contains("DevPERV"), 
    names_to = "DevPERV_N_SourceCategory_LandUse", 
    values_to = "DevPERV_Nloadexport_lb_acreyear"
  ) 

GC_landuses <- data.frame(
  LandUse = c(
    rep("LowResidential", 36), 
    rep("Residential", 25), 
    rep("Commercial+Transportation", 40)
  ),
  PercDensityofIC = 0:100 / 100
) %>%
  mutate(PercDevPERV = (1 - PercDensityofIC)) 

P.exports <- GC_landuses %>%
  full_join(., P.export.rates, by = "LandUse") %>%
  mutate(
    PLER = 
      PercDensityofIC * Ploadexport_lb_acreyear + 
      PercDevPERV * DevPERV_Ploadexport_lb_acreyear
  )

N.exports <- GC_landuses%>%
  full_join(., N.export.rates, by = "LandUse") %>%
  mutate(
    PLER = 
      PercDensityofIC * Nloadexport_lb_acreyear + 
      PercDevPERV * DevPERV_Nloadexport_lb_acreyear
  )

GC_PLER_estimates = data.frame(
  LandUse = c("LowResidential", "Residential", "Commercial+Transportation"),
  PLER_TP = c(0.55, 1.07, 1.16),
  PLER_TN = c(3.8, 6.2, 9.3)
) 

P_export_ses <- P.exports %>%
  group_by(LandUse) %>%
  summarize(standarddev = sd(PLER)/ sqrt(n()))

N_export_ses <- N.exports %>%
  group_by(LandUse) %>%
  summarize(standarddev = sd(PLER) / sqrt(n()))


# 15.Calculate Riparian Loading for each comid ####
keep_list <- c(
  "landcovers_bycomid",
  "sparrow_tn_urb",
  "sparrow_tp_urb",
  "river.lengths",
  "sparrow_tn_out",
  "sparrow_tp_out",
  "sparrow_tn_out",
  "PS_catchments",
  "InPath",
  "working_dir")

# Remove all objects except those in keep_list
rm(list = setdiff(ls(), keep_list))

m2peracre <- 4046.86
km2peracre <- 0.00404686
lbperkg <- 2.20462

# Calculate loading based on Green_Credits_Report_Final.pdf
# ND 6/23/26 Re-calculate loading based on dynamic SPARROW
riparianloadings_tmp <- landcovers_bycomid %>%
  select(comid = COMID, Lowdev_400ft, Meddev_300ft, Highdev_100ft, cv_numpixels) %>%
  mutate(
    contrib_lowres_m2 = as.numeric(Lowdev_400ft), 
#    contrib_lowres_acres = contrib_lowres_m2 / m2peracre, 
    contrib_res_m2 = as.numeric(Meddev_300ft), 
#    contrib_res_acres = contrib_res_m2 / m2peracre, 
    contrib_commerc_m2 = as.numeric(Highdev_100ft), 
#    contrib_commerc_acres = contrib_commerc_m2 / m2peracre,
#    contrib_lowres_acres_se = 
#      as.numeric(Lowdev_400ft) * cv_numpixels/ m2peracre,
#    contrib_res_acres_se = 
#      as.numeric(Meddev_300ft) * cv_numpixels/ m2peracre,
#    contrib_commerc_acres_se = 
#      as.numeric(Highdev_100ft) * cv_numpixels/ m2peracre,
contrib_lowres_m2_se = 
  as.numeric(Lowdev_400ft) * cv_numpixels,
contrib_res_m2_se = 
  as.numeric(Meddev_300ft) * cv_numpixels,
contrib_commerc_m2_se = 
  as.numeric(Highdev_100ft) * cv_numpixels,
contrib_dev_km2 =
      (contrib_lowres_m2 + contrib_res_m2 + contrib_commerc_m2)/1000000,
contrib_dev_km2_se =
  (contrib_lowres_m2_se + contrib_res_m2_se + contrib_commerc_m2_se)/1000000
  ) %>% #ND replace with loading rates from dynamic SPARROW
  left_join(sparrow_tn_urb, by = "comid") %>%  
  left_join(sparrow_tp_urb, by = c("comid","year","quarter")) %>%  
  mutate(
    P_dev_PLER_kg_km2yr = TPKG_URB_PER_KM2 * 4, 
    N_dev_PLER_kg_km2yr = TNKG_URB_PER_KM2 * 4, 
    P_dev_PLER_kg_km2yr_se = TPKG_URB_PER_KM2_SE * 4,
    N_dev_PLER_kg_km2yr_se = TNKG_URB_PER_KM2_SE * 4) %>%
      
    # P_lowres_PLER_lb_acyr = 0.55,
    # P_res_PLER_lb_acyr = 1.07,
    # P_commerc_PLER_lb_acyr = 1.16,
    # N_lowres_PLER_lb_acyr = 3.8,
    # N_res_PLER_lb_acyr = 6.2,
    # N_commerc_PLER_lb_acyr = 9.3,
    # P_lowres_PLER_lb_acyr_se = P_export_ses[[2,2]],
    # P_res_PLER_lb_acyr_se = P_export_ses[[3,2]],
    # P_commerc_PLER_lb_acyr_se = P_export_ses[[1,2]],
    # N_lowres_PLER_lb_acyr_se = N_export_ses[[2,2]],
    # N_res_PLER_lb_acyr_se = N_export_ses[[3,2]],
    # N_commerc_PLER_lb_acyr_se = N_export_ses[[1,2]]
  #) %>%
  mutate(
    N_PLER_kg_yr = contrib_dev_km2 * N_dev_PLER_kg_km2yr,    
    P_PLER_kg_yr = contrib_dev_km2 * P_dev_PLER_kg_km2yr
    # P_PLER_lb_yr = contrib_lowres_acres * P_lowres_PLER_lb_acyr + 
    #   contrib_res_acres * P_res_PLER_lb_acyr + 
    #   contrib_commerc_acres * P_commerc_PLER_lb_acyr,
    # N_PLER_lb_yr = contrib_lowres_acres * N_lowres_PLER_lb_acyr + 
    #   contrib_res_acres * N_res_PLER_lb_acyr + 
    #   contrib_commerc_acres * N_commerc_PLER_lb_acyr,
    # P_PLER_kg_yr = P_PLER_lb_yr / lbperkg,
    # N_PLER_kg_yr = N_PLER_lb_yr / lbperkg
  ) %>%
  rowwise() %>%
  mutate(
    N_PLER_kg_yr_se = my_propogateerror(
      vals = list(
      c(contrib_dev_km2, contrib_dev_km2_se), 
      c(N_dev_PLER_kg_km2yr, N_dev_PLER_kg_km2yr_se)
      ),
      method = "mult"
            )
          ) %>%
  mutate(
    P_PLER_kg_yr_se = my_propogateerror(
      vals = list(
        c(contrib_dev_km2, contrib_dev_km2_se), 
        c(P_dev_PLER_kg_km2yr, P_dev_PLER_kg_km2yr_se)
      ),
      method = "mult"
    )
  ) %>% 
  # mutate(
  #   P_PLER_lb_yr_se = my_propogateerror(
  #     vals = list(
  #       if(contrib_lowres_acres > 0) {
  #         c(
  #           contrib_lowres_acres * P_lowres_PLER_lb_acyr, 
  #           my_propogateerror(
  #             vals = list(
  #               c(contrib_lowres_acres, contrib_lowres_acres_se), 
  #               c(P_lowres_PLER_lb_acyr, P_lowres_PLER_lb_acyr_se)
  #             ),
  #             method = "mult"
  #           )
  #         )
  #       } else {c(1,0)},
  #       if(contrib_res_acres > 0) {
  #         c(
  #           contrib_res_acres * P_res_PLER_lb_acyr,           
  #           my_propogateerror(
  #             vals = list(
  #               c(contrib_res_acres, contrib_res_acres_se), 
  #               c(P_res_PLER_lb_acyr, P_res_PLER_lb_acyr_se)
  #             ),
  #             method = "mult"
  #           )
  #         )
  #       } else {c(1,0)},
  #       if(contrib_commerc_acres > 0) {
  #         c(
  #           contrib_commerc_acres * P_commerc_PLER_lb_acyr,          
  #           my_propogateerror(
  #             vals = list(
  #               c(contrib_commerc_acres, contrib_commerc_acres_se), 
  #               c(P_commerc_PLER_lb_acyr, P_commerc_PLER_lb_acyr_se)
  #             ),
  #             method = "mult"
  #           )
  #         )
  #       } else{c(1,0)}
  #     ), 
  #     method = "addsub"
  #   ),
  #   N_PLER_lb_yr_se = my_propogateerror(
  #     vals = list(
  #       if(contrib_lowres_acres > 0) {
  #         c(
  #           contrib_lowres_acres * N_lowres_PLER_lb_acyr, 
  #           my_propogateerror(
  #             vals = list(
  #               c(contrib_lowres_acres, contrib_lowres_acres_se), 
  #               c(N_lowres_PLER_lb_acyr, N_lowres_PLER_lb_acyr_se)
  #             ),
  #             method = "mult"
  #           )
  #         )
  #       } else {c(1,0)},
  #       if(contrib_res_acres > 0) {
  #         c(
  #           contrib_res_acres * N_res_PLER_lb_acyr,           
  #           my_propogateerror(
  #             vals = list(
  #               c(contrib_res_acres, contrib_res_acres_se), 
  #               c(N_res_PLER_lb_acyr, N_res_PLER_lb_acyr_se)
  #             ),
  #             method = "mult"
  #           )
  #         )
  #       } else {c(1,0)},
  #       if(contrib_commerc_acres > 0) {
  #         c(
  #           contrib_commerc_acres * N_commerc_PLER_lb_acyr,          
  #           my_propogateerror(
  #             vals = list(
  #               c(contrib_commerc_acres, contrib_commerc_acres_se), 
  #               c(N_commerc_PLER_lb_acyr, N_commerc_PLER_lb_acyr_se)
  #             ),
  #             method = "mult"
  #           )
  #         )
  #       } else{c(1,0)}
  #     ), 
  #     method = "addsub"
  #   )
  # ) %>%
  # mutate(
  #   P_PLER_kg_yr_se = P_PLER_lb_yr_se / lbperkg,
  #   N_PLER_kg_yr_se = N_PLER_lb_yr_se / lbperkg,)
  # ) %>%
  mutate(N_PLER_kg_yr = as.numeric(N_PLER_kg_yr),
         P_PLER_kg_yr = as.numeric(P_PLER_kg_yr),
         P_PLER_kg_yr_se = as.numeric(P_PLER_kg_yr_se),
         N_PLER_kg_yr_se = as.numeric(N_PLER_kg_yr_se)) %>%
  left_join(., river.lengths, by = "comid") %>% 
  replace_na(
    list(
      N_PLER_kg_yr = 0,
      P_PLER_kg_yr = 0, 
      P_PLER_kg_yr_se = 0,
      N_PLER_kg_yr_se = 0
    )
  ) %>%
  select(
    comid, year, quarter, 
    N_riparian_developed_kgyr = N_PLER_kg_yr, 
    P_riparian_developed_kgyr = P_PLER_kg_yr,
    N_riparian_developed_kgyr_se = N_PLER_kg_yr_se, 
    P_riparian_developed_kgyr_se = P_PLER_kg_yr_se
  ) %>% 
  left_join(
    ., sparrow_tn_out %>% select(comid, year, quarter, il_tn_fer, ilse_tn_fer), by = c("comid", "year", "quarter")
  ) %>% 
  left_join(
    ., 
    sparrow_tp_out %>% 
      select(comid, year, quarter, il_tp_fer, ilse_tp_fer), 
    by = c("comid", "year", "quarter")
  ) 

riparian_loading_comid_miss <- riparianloadings_tmp %>%
  filter(is.na(year))
## 24 COMIDs in the PS NLCD data that aren't in SPARROW

nlcd_missing_comid <- sparrow_tn_out %>%
  filter(!comid %in% unique(landcovers_bycomid$COMID)) %>%
  select(comid) %>%
  unique() # n = 939 (zeros?)

sparrow_miss <- PS_catchments %>%
  filter(FEATUREID %in% unique(riparian_loading_comid_miss$comid))

nlcd_miss <- PS_catchments %>%
  filter(FEATUREID %in% unique(nlcd_missing_comid$comid))

missing_map <- ggplot() +
  geom_sf(data = PS_catchments, fill = "grey90", color = "white", size = 0.2) +
  geom_sf(data = sparrow_miss, aes(color = "Missing COMIDs"), size = 2) +
  scale_color_manual(values = c("Missing COMIDs" = "red")) +
  labs(title = "Missing COMIDs in Puget Sound",
       color = "Legend") +
  theme_minimal()

#missing_map

nlcd_missing_map <- ggplot() +
  geom_sf(data = PS_catchments, fill = "grey90", color = "white", size = 0.2) +
  geom_sf(data = nlcd_miss, aes(color = "Missing COMIDs"), size = 2) +
  scale_color_manual(values = c("Missing COMIDs" = "red")) +
  labs(title = "Missing COMIDs in Puget Sound",
       color = "Legend") +
  theme_minimal()

#nlcd_missing_map

riparianloadings <- riparianloadings_tmp %>%
  filter(!is.na(year)) %>%
  mutate(
    N_riparian_kgyr = N_riparian_developed_kgyr + il_tn_fer,
    N_riparian_kgyr_se = my_propogateerror(
      vals = list(
        c(N_riparian_developed_kgyr, N_riparian_developed_kgyr_se), 
        c(il_tn_fer, ilse_tn_fer)
      ),
      method = "addsub"
    )
  ) %>%
  mutate(
    P_riparian_kgyr = P_riparian_developed_kgyr + il_tp_fer,
    P_riparian_kgyr_se = my_propogateerror(
      vals = list(
        c(P_riparian_developed_kgyr, P_riparian_developed_kgyr_se), 
        c(il_tp_fer, ilse_tp_fer)
      ),
      method = "addsub"
    )
  ) %>%
  select(
    comid,
    year,
    quarter,
    N_riparian_kgyr, 
    P_riparian_kgyr, 
    N_riparian_kgyr_se, 
    P_riparian_kgyr_se
  )


# 16. Calculate current river length in buffer already ####


sqftpersqm <- 10.7639

bufferedlengths_tmp <- foreach(
  i = 1:5, .combine = "rbind", .packages = "tidyverse"
) %dopar% {
  
  bufwid <- buffer.options[i]
  
  tmp<-landcovers_bycomid %>%
    select(comid = COMID, contains(paste(bufwid)), cv_numpixels) %>%
    mutate(across(contains(paste(bufwid)), ~ as.numeric(.))) %>%
    rename_with(
      .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
      .cols = contains(paste(bufwid))
    ) %>%
    left_join(., river.lengths, by = "comid") %>%
    select(
      comid, 
      Forest, 
      Grass, 
      totalbanklength_ft, 
      cv_numpixels, 
      totalbanklength_ft_se
    ) %>%
    mutate(
      bufferwidth = bufwid,
      Forest = case_when(is.na(Forest) ~ 0, TRUE ~ Forest),
      Grass = case_when(is.na(Grass) ~ 0, TRUE ~ Grass)
    ) 
  
  tmp2 <- tmp %>%
    mutate(
      Forest_area_m2 = Forest,
      Grass_area_m2 = Grass,
      Forest_area_ft2 = Forest_area_m2 * sqftpersqm,
      Grass_area_ft2 = Grass_area_m2 * sqftpersqm,
      Forest_area_ft2_se = Forest_area_ft2 * cv_numpixels,
      Grass_area_ft2_se = Grass_area_ft2 * cv_numpixels
    ) %>%
    mutate(
      Forest_bufferlength_ft = Forest_area_ft2 / bufferwidth,
      Grass_bufferlength_ft = Grass_area_ft2 / bufferwidth,
      Forest_bufferlength_ft_se = my_propogateerror(
        vals = list(
          if(Forest_area_ft2 > 0) {
            c(Forest_area_ft2, Forest_area_ft2_se)
          } else {c(1,0)},
          c(bufferwidth, 0)
        ),
        method = "div"
      ),
      Grass_bufferlength_ft_se = my_propogateerror(
        vals = list(
          if(Grass_area_ft2 >0) {
            c(Grass_area_ft2, Grass_area_ft2_se)
          } else {c(1,0)},
          c(bufferwidth, 0)
        ),
        method = "div"
      )
    ) %>%
    mutate(
      Forest_bufferlength_ft_rev = case_when(
        Forest_bufferlength_ft > totalbanklength_ft ~ totalbanklength_ft,
        Forest_bufferlength_ft <= totalbanklength_ft ~ Forest_bufferlength_ft
      ),
      Forest_bufferlength_ft_rev_se = case_when(
        Forest_bufferlength_ft > totalbanklength_ft ~ totalbanklength_ft_se,
        Forest_bufferlength_ft <= totalbanklength_ft ~ Forest_bufferlength_ft_se
      ),
      Grass_bufferlength_ft_rev = case_when(
        Grass_bufferlength_ft >
          totalbanklength_ft - Forest_bufferlength_ft_rev ~
          totalbanklength_ft - Forest_bufferlength_ft_rev,
        Grass_bufferlength_ft <=
          totalbanklength_ft - Forest_bufferlength_ft_rev ~
          Grass_bufferlength_ft
      ),
      Grass_bufferlength_ft_rev_se = case_when(
        Grass_bufferlength_ft >
          totalbanklength_ft - Forest_bufferlength_ft_rev ~
          my_propogateerror(
            vals = list(
              c(totalbanklength_ft, totalbanklength_ft_se),
              c(Forest_bufferlength_ft_rev, Forest_bufferlength_ft_rev_se)
            ),
            method = "addsub"
          ),
        Grass_bufferlength_ft <=
          totalbanklength_ft - Forest_bufferlength_ft_rev ~
          Grass_bufferlength_ft_se
      )
    )
  
  tmp2
  
} 

# Pivot to a wider format that is more human-readible 
bufferedlengths <- bufferedlengths_tmp %>%
  select(
    comid, 
    bufferwidth_ft = bufferwidth, 
    Grass_buffer_ft = Grass_bufferlength_ft_rev, 
    Forest_buffer_ft = Forest_bufferlength_ft_rev,
    Grass_buffer_ft_se = Grass_bufferlength_ft_rev_se,
    Forest_buffer_ft_se = Forest_bufferlength_ft_rev_se
  ) %>%
  pivot_wider(
    id_cols = c(comid), 
    names_from = bufferwidth_ft, 
    values_from = c(
      Grass_buffer_ft, Forest_buffer_ft, Grass_buffer_ft_se, Forest_buffer_ft_se
    )
  ) %>%
  left_join(
    ., 
    river.lengths %>% 
      select(comid, totalbanklength_ft, totalbanklength_ft_se), 
    by = "comid"
  ) %>% 
  rename_with(
    .fn = ~paste0(gsub("_ft", "", .), "ft_ft"), .cols = contains("buffer")
  )


# 17. Enter Efficiency Data ####

# The data entered here was read off of Green_Credit_Report_Final.pdf

# Create the empty dataframe
base_curve_data_empty <- data.frame(
  expand.grid(
    HSG = c("A", "B", "C", "D"),
    Width = c(20, 35, 100),
    Nutrient = c("N", "P"),
    VegType = c("Grass", "Forest")
  ),
  Efficiency = NA_real_
) 

# Enter data
base_curve_data <-   mutate(
  base_curve_data_empty,
  Efficiency = case_when(
    Width == 100 ~ case_when(
      VegType == "Forest" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 18/100,
          HSG == "B" ~ 28/100,
          HSG == "C" ~ 48/100,
          HSG == "D" ~ 65/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 45/100,
          HSG == "B" ~ 34/100,
          HSG == "C" ~ 20/100,
          HSG == "D" ~ 11/100
        )
      ),
      VegType == "Grass" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 13/100,
          HSG == "B" ~ 23/100,
          HSG == "C" ~ 39/100,
          HSG == "D" ~ 52/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 36/100,
          HSG == "B" ~ 27/100,
          HSG == "C" ~ 16/100,
          HSG == "D" ~ 9/100
        )
      )
    ),
    Width == 35 ~ case_when(
      VegType == "Forest" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 5/100,
          HSG == "B" ~ 8/100,
          HSG == "C" ~ 14/100,
          HSG == "D" ~ 20/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 30/100,
          HSG == "B" ~ 22/100,
          HSG == "C" ~ 13/100,
          HSG == "D" ~ 8/100
        )
      ),
      VegType == "Grass" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 4/100,
          HSG == "B" ~ 6/100,
          HSG == "C" ~ 12/100,
          HSG == "D" ~ 15/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 24/100,
          HSG == "B" ~ 18/100,
          HSG == "C" ~ 10/100,
          HSG == "D" ~ 6/100
        )
      )
    ),
    Width == 20 ~ case_when(
      VegType == "Forest" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 2/100,
          HSG == "B" ~ 4/100,
          HSG == "C" ~ 8/100,
          HSG == "D" ~ 10/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 20/100,
          HSG == "B" ~ 13/100,
          HSG == "C" ~ 9/100,
          HSG == "D" ~ 5/100
        )
      ),
      VegType == "Grass" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 2/100,
          HSG == "B" ~ 4/100,
          HSG == "C" ~ 6/100,
          HSG == "D" ~ 8/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 16/100,
          HSG == "B" ~ 12/100,
          HSG == "C" ~ 7/100,
          HSG == "D" ~ 4/100
        )
      )
    )
  )
)  


# 18. Compute efficiency curves ####

# Phosphorus curves are best fit with an exponential function and Nitrogen curves with a linear function

basecurves <- tibble(
  foreach(
    i = unique(base_curve_data$HSG), 
    .combine = "rbind",
    .packages = c("foreach", "tidyverse"),
    .export = "base_curve_data"
  ) %dopar% {
    foreach(j = unique(base_curve_data$VegType), .combine = "rbind") %dopar% {
      foreach(k = unique(base_curve_data$Nutrient), .combine = "rbind") %dopar% {
        mod1 <- with(
          
          base_curve_data[
            with(
              base_curve_data, which(HSG == i & VegType == j & Nutrient == k)
            ), 
          ],
          if(k == "N") {
            lm(Efficiency ~ (Width))
          } else if(k == "P") {
            lm(Efficiency ~ log(Width))
          }
        )
        
        adj.rsquared <- summary(mod1)$adj.r.squared
        
        curve.form <- if(k == "N") {
          "coef.3 * (coef.1 + coef.2 * x)"
        } else if(k == "P") {
          " coef.3 * (coef.1 + coef.2 * log(x))"
        }
        
        tibble(
          HSG = i, 
          VegType = j, 
          Nutrient = k, 
          Slope = c("0-5%", "5-10%", "10-15%", ">15%"),
          coef.1 = coef(mod1)[1], 
          coef.2 = coef(mod1)[2],
          coef.3 = c(1, 0.75, 0.5, 0),
          curve.form = curve.form,
          adj.r.squared = adj.rsquared
        ) %>%
          rowwise() %>%
          mutate(
            Efficiency = gsub(
              "coef.3", coef.3, gsub(
                "coef.2", coef.2, gsub("coef.1", coef.1, curve.form)
              )
            )
          ) 
      }
    }
  }
)

# 19. Write efficiency curves and limits for each comid ####

riparianefficiencies_bycomid_tmp <- foreach(
  i = 1:5, .combine = "rbind", .packages = "tidyverse"
) %dopar% {
  
  bufwid <- buffer.options[i]
  
  river.lengths %>%
    select(comid) %>%
    left_join(
      ., 
      hsg_bycomid %>% 
        select(comid, contains(paste(bufwid))) %>%
        mutate(across(contains(paste(bufwid)), ~ as.numeric(.))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
          .cols = contains(paste(bufwid))
        ), 
      by= "comid"
    ) %>%
    mutate(
      HSG_N = case_when(
        A > 0 ~ "A", B > 0 ~ "B", C > 0 ~ "C", D > 0 ~ "D", TRUE ~ "A"
      ),
      HSG_P = case_when(
        D > 0 ~ "D", C > 0 ~ "C", B > 0 ~ "B", A > 0 ~ "A", TRUE ~ "D"
      ),
      bufferwidth_ft = bufwid
    ) %>%
    left_join(
      ., 
      slopes_bycomid %>%  
        select(comid, Slope = value, contains(paste(bufwid))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
          .cols = contains(paste(bufwid))
        ), 
      by = "comid"
    ) %>% filter (!is.na(A))
} 

# format the curves for writing to csv

riparianefficiencies_bycomid_vals <- riparianefficiencies_bycomid_tmp %>%
  select(
    comid, 
    bufferwidth_ft,
    HSG_N,
    HSG_P,
    Slope
  ) %>%
  pivot_longer(
    cols = c(HSG_N, HSG_P), names_to = "Nutrient", values_to = "HSG"
  ) %>%
  mutate(
    Nutrient = gsub("HSG_", "", Nutrient),
    VegType = rep(c("Forest", "Grass"), length.out = nrow(.))
  ) %>%
  tidyr::complete(comid, Nutrient, VegType, bufferwidth_ft) %>%
  group_by(comid, Nutrient, bufferwidth_ft) %>%
  fill(Slope, HSG, .direction = "downup") %>%
  mutate(Slope = case_when(is.na(Slope) ~ "0-5%", T ~ Slope)) %>% ##KM: All are 0-5%, fill just didn't work properly
  left_join(., basecurves, by = c("HSG", "Slope", "VegType", "Nutrient")) %>% ##Included SLOPE
  rowwise() %>%
  mutate(
    Efficiency = gsub(
      "coef.3", coef.3, gsub(
        "coef.2", coef.2, gsub("coef.1", coef.1, curve.form)
      )
    )
  ) %>%
  select(comid, Nutrient, VegType, bufferwidth_ft, Efficiency) %>%
  distinct()  %>% 
  pivot_wider(
    names_from = c(Nutrient, VegType, bufferwidth_ft),
    values_from = Efficiency
  ) %>%
  rename_with(.fn = ~paste0(., "ft"), .cols = -comid)

# Combine all the necessary data to choose an efficiency curve

riparianefficiencies_bycomid_tmp_dist <- foreach(
  i = 1:5, .combine = "rbind", .packages = "tidyverse"
) %dopar% {
  
  bufwid <- buffer.options[i]
  
  river.lengths %>%
    select(comid) %>%
    left_join(
      ., 
      hsg_bycomid_toresample %>% 
        filter(buffersize == i),
      by= "comid"
    ) %>%
    rowwise() %>%
    mutate(
      HSG_N = list(
        tryCatch(
          sample(x = unlist(resampledist), size = 10, replace = TRUE), 
          error = function(e) {list()}
        )
      ),
      HSG_P = list(
        tryCatch(
          sample(x = unlist(resampledist), size = 10, replace = TRUE), 
          error = function(e) {list()})
      ),
      bufferwidth_ft = bufwid
    )  %>%
    left_join(
      .,
      slopes_bycomid_dist %>%
        select(comid, contains(paste(bufwid))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .),
          .cols = contains(paste(bufwid))
        ),
      by = "comid"
    )
  
} 
# format the curves for writing to csv

riparianefficiencies_bycomid_dist <- riparianefficiencies_bycomid_tmp_dist %>%
  select(
    comid,
    bufferwidth_ft,
    HSG_N,
    HSG_P,
    Slope = meanslope
  ) %>%
  pivot_longer(
    cols = c(HSG_N, HSG_P), names_to = "Nutrient", values_to = "HSG"
  ) %>%
  mutate(
    Nutrient = gsub("HSG_", "", Nutrient),
    VegType = rep(c("Forest", "Grass"), length.out = nrow(.))
  ) %>%
  tidyr::complete(comid, Nutrient, VegType, bufferwidth_ft) %>%
  group_by(comid, Nutrient, bufferwidth_ft) %>%
  fill(HSG, Slope, .direction = "downup") %>%
  mutate(Slope = replace_na(Slope, list(rep(">15%", 10))))  # for missing slope values, assume >15%, which is the least effective slope class for riparian buffer nutrient removal

riparianefficiencies_bycomid_dist_end <- riparianefficiencies_bycomid_dist %>%
  filter(HSG != "list()") %>%
  mutate(
    Efficiency_uncertainty = pmap_chr(
      .l = list(x = HSG, y = Slope, Nutrient, VegType),
      .f = ~ {
        nut = ..3
        a = ..4
        x = .x
        y = .y
        paste0(
          "c('",
          paste(
            c(
              basecurves$Efficiency[
                pmap_int(
                  .l = list(x = x, y = y),
                  .f = ~which(
                    basecurves$Nutrient == nut &
                      basecurves$VegType == a &
                      basecurves$HSG == .x &
                      basecurves$Slope == .y
                  ),
                  nut = nut,
                  a = a
                )
              ]
            ), collapse = "', '"
          ),
          "')"
        )
      }
    )
  ) %>%
  select(comid, Nutrient, VegType, bufferwidth_ft, Efficiency_uncertainty) %>%
  pivot_wider(
    id_cols = c(comid), 
    names_from = c(Nutrient, VegType, bufferwidth_ft),
    values_from = Efficiency_uncertainty
  ) %>%
  rename_with(.fn = ~paste0(., "ft_uncertainty"), .cols = -comid)

riparianefficiencies_bycomid <- merge(
  riparianefficiencies_bycomid_vals, 
  riparianefficiencies_bycomid_dist_end,
  by = 'comid',
  all = T
)

# 20. Write Files ####
write.csv(
  riparianloadings,
  file = "./RBEROST-Pacific/Preprocessing/Inputs/RiparianLoadings_update_dynSPRW.csv" #*#
)

write.csv(
  bufferedlengths, 
  file = "./RBEROST-Pacific/Preprocessing/Inputs/LengthinBuffer_2016_update.csv" #*#
)

write.csv(
  riparianefficiencies_bycomid, 
  file = 
    "./RBEROST-Pacific/Preprocessing/Inputs/RiparianEfficiencies_update.csv"
)

# 21. QA/QC ####
riparianloadings_QA <- riparianloadings %>%
  filter(is.na(N_riparian_kgyr) | is.na(P_riparian_kgyr))

# Get unique COMIDs from each data frame
comids_list <- list(
  riparianloadings = unique(riparianloadings$comid),
  bufferedlengths = unique(bufferedlengths$comid),
  riparianefficiencies_bycomid = unique(riparianefficiencies_bycomid$comid)
)

# Find all unique COMIDs across all data frames
all_comids <- unique(unlist(comids_list))

# Create a data frame showing presence/absence in each
comid_compare <- tibble(comid = all_comids) %>%
  mutate(
    in_riparianloadings = comid %in% comids_list$riparianloadings,
    in_bufferedlengths = comid %in% comids_list$bufferedlengths,
    in_riparianefficiencies_bycomid = comid %in% comids_list$riparianefficiencies_bycomid
  ) %>%
  filter(
    in_riparianloadings == FALSE | in_bufferedlengths == FALSE | 
    in_riparianefficiencies_bycomid == FALSE
  )
