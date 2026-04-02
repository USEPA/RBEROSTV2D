###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Assign cost and efficiency data for road BMPs ####
# BY: Kelly-Anne Moffa
# DATE:  03/25/2024 ####
###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~


###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 1. Setup -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

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

InPath <- paste0(working_dir, "RBEROST-Pacific/Preprocessing/Inputs/")

user_specs_BMPs <- fread(paste(InPath,"01_UserSpecs_BMPs.csv",sep=""), data.table = FALSE) 

road_bmps <- (user_specs_BMPs %>% filter(BMP_Category == "road"))$BMP

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 2. Load relevant data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

urban_data <- fread(paste0(InPath, "UrbanBMPData.csv"))
urban_data_effic <- fread(paste0(InPath, "UrbanBMP_Effic_Data.csv"))

# Extract road-specific data (Land_Use == "UIDU")
# Note: NA HSG values are handled in 00_CostandEffic_Calc.R with high costs/zero efficiency
road_data <- urban_data[BMP %in% road_bmps & Land_Use == "UIDU", ] 

road_effic_data <- urban_data_effic[BMP %in% road_bmps & Land_Use == "UIDU", ]

road_area_tmp <- as.data.frame(road_data[, .(comid, HSG, Area)]) %>%
  distinct() 

road_area <- road_area_tmp %>%
  group_by(comid) %>% 
  mutate(area_sum = sum(Area)) %>%
  mutate(area_port = Area/area_sum) %>%
  ungroup() 

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 3. Print data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

write.csv(road_data, paste0(InPath, "road_bmps.csv"), row.names = F)
write.csv(road_effic_data, paste0(InPath, "road_bmps_effic.csv"), row.names = F)
write.csv(road_area, paste0(InPath, "road_areas.csv"), row.names = F)

