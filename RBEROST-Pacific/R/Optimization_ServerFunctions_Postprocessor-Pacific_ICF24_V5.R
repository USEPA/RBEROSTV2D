###########################################################################################
# TITLE: Postprocessing Shiny App Server Functions
# PURPOSE: To allow users to input a text file and see the results of their NEOS optimization
# BY: Cathy Chamberlin and Sam Ennett, earlier versions of the code by Kate Munson and Alyssa Le (ICF)
# DATE: 9/23/2021
###########################################################################################

# This file is NOT intended to be run by itself. It is sourced through Optimization_ServerFile.R. This file contains most of the code necessary for the postprocessor to run as functions.
# Debugging is best done with browser() inserted in reactive contexts that may be causing problems.
# Lines marked #*# may be changed by the user.

# The components of the Server are roughly organized as such:

##################################################
# Table of Contents:
# 1. Upload user-supplied files
# 2. Parse NEOS data file
# 3. Combine NEOS results with other dataframes
# 4. Render Preview UI
# 5. Write Report Outputs
# 6. Render Report UI
##################################################

##################################################
# 1.Upload user-supplied files ####
##################################################

read_csvs_using_baseR <- function(path) {
  tryCatch(
    # when reading semicolon separated files, having a comma separator causes `read.csv` to error. Using read.csv2 causes no error.
    expr = {
      read.csv2(
        file = path, 
        colClasses = "character",
        row.names = NULL,
        header = FALSE,
        blank.lines.skip = FALSE,
        sep = "\n"
      )
    },
    error = function(e) {stop(safeError(error = e))} # return a safeError if a parsing error occurs
  )
}

read_csvs_using_fread <- function(path) {
  tmp <- tryCatch(
    expr = {fread(file = path, fill = TRUE)},
    error = function(e) {stop(safeError(error = e)) }
  )
  
  return(tmp)
}

read_all_streamcat_files <- function(scfiles) {
  tmp <- base::lapply(scfiles$datapath, read_csvs_using_fread)
  names(tmp) <- scfiles$name
  
  return(tmp)
}


##################################################
# 2.Parse NEOS data file ####
##################################################

get_solvestatus <- function(results_astable) {
  
  solved_txt <- "solve_result = "
  solved_rows <-
    results_astable[which(str_detect(results_astable[, 1], solved_txt)), 1]
  
  # Check for presolve issues first (these indicate infeasibility)
  presolve_issues <- any(grepl("presolve:.*cannot hold", results_astable[, 1], ignore.case = TRUE))
  
  if(length(solved_rows) > 0) {
    solved_tmp <- strsplit(solved_rows, "\\s+")
    
    solved_tmp2 <- unlist(
      mapply(
        "[[", 
        solved_tmp, 
        index = mapply(
          which, 
          x = lapply(
            FUN = function(x, pattern) {x == pattern}, 
            X = solved_tmp, 
            pattern = "solve_result"
          )
        ) + 2
      ), use.names = FALSE
    )
    
    if ("solved" %in% solved_tmp2 | "infeasible" %in% solved_tmp2) {
      solved <- foreach(i = 1:length(solved_tmp2)) %do% {
        if(
          solved_tmp2[i] == "solved"
        ) {1} else if(
          solved_tmp2[i] == "infeasible" | presolve_issues
        ) {2} else {-98}  # 2 = infeasible (including presolve)
      }
    } else if(any(grepl("Error", results_astable[, 1]))){
      solved <- list(0)  # 0 = error/failed optimization
    } else {solved <- list(-98)}
  } else if(presolve_issues) {
    solved <- list(2)  # 2 = infeasible due to presolve
  } else if(any(grepl("Error", results_astable[, 1]))){
    solved <- list(0)  # 0 = error/failed optimization
  } else if (!any(grepl("solve_result", results_astable[, 1]))) {
    solved <- list(-99)
  } else {solved <- list(-98)}
  
  return(solved)
}


get_user_notes <- function(results_astable) {
  notetextstart <- "%% COMMENTS %%"
  notetextend <- "%%%%%%%%%%"
  
  notetextlinestart <- which(str_detect(results_astable[, 1], notetextstart))
  notetextlineend <-  which(substr(results_astable[, 1], 1, 10) == notetextend)
  
  if(!is_empty(notetextlinestart)) {
    UserNotes <- paste(
      results_astable[(notetextlinestart):(notetextlineend),1], 
      collapse = "\n"
    )
    UserNotes2 <- gsub("%%", "", gsub("%% COMMENTS %%", "", UserNotes))
  } else {UserNotes2 <- ""}
  
  return(UserNotes2)
}


get_scen.vis <- function(input, x) {
  if(as.numeric(as.character(input)) <= x){
    tmp <- as.numeric(as.character(input))
  } else {tmp <- 1}
  return(tmp)
}

get_presolve_display <- function(results_astable) {
  
  presolve_lines <- which(grepl("presolve:", results_astable[, 1], ignore.case = TRUE))
  
  if(length(presolve_lines) > 0) {
    # Find the range of presolve messages
    start_line <- min(presolve_lines)
    
    # Look for where presolve messages end
    # They typically end before actual results or at the end of consecutive presolve lines
    end_candidates <- which(grepl("solve_result|cost|point_dec|urban_frac|road_frac|ag_frac|sum\\{", results_astable[, 1]))
    
    if(length(end_candidates) > 0) {
      end_line <- min(end_candidates[end_candidates > start_line]) - 1
    } else {
      # Find last consecutive presolve-related line
      consecutive_end <- start_line
      for(i in (start_line + 1):nrow(results_astable)) {
        if(grepl("presolve:|cannot hold:|body <=|difference =|messages suppressed", 
                 results_astable[i, 1], ignore.case = TRUE)) {
          consecutive_end <- i
        } else if(nchar(trimws(results_astable[i, 1])) > 0) {
          break
        }
      }
      end_line <- consecutive_end
    }
    
    # Extract and format the presolve messages
    presolve_msgs <- results_astable[start_line:end_line, 1]
    
    # Add a summary message at the beginning
    constraint_lines <- grep("constraint.*cannot hold:", presolve_msgs)
    num_constraints <- length(constraint_lines)
    
    summary <- paste0(
      "PRESOLVE INFEASIBILITY DETECTED",
      "The optimization problem is infeasible due to presolve constraints.",
      "Number of constraint violations: ", num_constraints,
      "This typically means the loading reduction targets cannot be met with the",
      "available BMPs and their effectiveness levels. Consider:",
      "  1. Relaxing the loading reduction targets",
      "  2. Adding more BMP options",
      "  3. Reviewing the baseline loading estimates"
    )
    
    display <- c(summary, presolve_msgs)
  } else {
    display <- NULL
  }
  
  return(display)
}

get_error_display <- function(results_astable) {
  
  error_lines <- which(grepl(c("Error|error"), results_astable[, 1]))
  
  if(length(error_lines) > 0) {
    start_line <- min(error_lines)
    
    # Find where the actual results begin (after the errors)
    results_start <- which(grepl("solve_result|cost|point_dec|urban_frac|road_frac|ag_frac", results_astable[, 1]))
    
    if(length(results_start) > 0) {
      end_line <- min(results_start) - 1
    } else {
      end_line <- nrow(results_astable)
    }
    
    display <- results_astable[start_line:end_line, 1]
  } else {
    display <- "No errors found in the results file."
  }
  
  return(display)
}

get_total_cost <- function(results_astable, scenarionumber) {
  
  cost_txt <- "cost = "
  cost_row_indices <- which(str_detect(results_astable[, 1], cost_txt))
  
  # Check if cost information exists and if scenarionumber is valid
  if(length(cost_row_indices) == 0 || scenarionumber > length(cost_row_indices)) {
    return(NULL)
  }
  
  cost_rows <- results_astable[cost_row_indices, 1][scenarionumber]
  
  # Check if cost_rows is valid (not NA or empty)
  if(is.na(cost_rows) || length(cost_rows) == 0) {
    return(NULL)
  }
  
  cost_tmp <- strsplit(cost_rows, " ")
  cost_tmp2 <- unlist(
    suppressWarnings(as.numeric(as.character(lapply(cost_tmp, "[[", 3)))), 
    use.names = FALSE
  )
  total_cost <- cost_tmp2[which(!is.na(cost_tmp2))]
  
  # Return NULL if no valid cost found
  if(length(total_cost) == 0) {
    return(NULL)
  }
  
  return(total_cost)
}


get_cost_bootstraps <- function(
  results_astable, scenarionumber, endrows, startrows
) {
  
  display_txt <- "sum[{]c in comid_all"
  coef_txt <- "coef"
  
  cost_bootstraps_begin <- which(
    grepl(results_astable[, 1], pattern = display_txt) &
      str_detect(results_astable[, 1], coef_txt)
  )[scenarionumber]
  
  if(length(cost_bootstraps_begin) > 0 & !is.na(cost_bootstraps_begin)) {
    cost_bootstraps_end <- min(endrows[which(endrows > cost_bootstraps_begin)])
    
    if(scenarionumber == 1) {
      cost_bootstraps_tablebegin <- min(
        startrows[which(startrows > cost_bootstraps_begin)]
      )
    } else {
      cost_bootstraps_tablebegin <- cost_bootstraps_begin
    }
    
    if((cost_bootstraps_tablebegin + 1) < (cost_bootstraps_end - 1)) {
      
      cost_bootstraps <- data.frame(
        str_split_fixed(
          parse_character(
            results_astable[
              (cost_bootstraps_tablebegin + 1):(cost_bootstraps_end - 1), 1
            ]
          )
          , " ", n = 2)
      ) %>%
        rename(Bootstrap = 1, Cost = 2) %>%
        mutate(across(.fns = ~as.numeric(as.character(.))))
    } else {
      # Bootstrap section exists but is empty (e.g., when cost = 0)
      cost_bootstraps <- NULL
    }
  } else { cost_bootstraps <- NULL}
  
  
  return(cost_bootstraps)
}

get_cost_uncertainty_message <- function(costs) {
  
  # Handle case where costs is NULL or has no data (e.g., when optimization cost = 0)
  if(is.null(costs) || nrow(costs) == 0 || all(is.na(costs$Cost))) {
    return("Uncertainty analysis not available: optimization resulted in zero cost.")
  }
  
  CI95.low <- quantile(costs$Cost, 0.025, na.rm = TRUE)
  CI95.high <- quantile(costs$Cost, 0.975, na.rm = TRUE)
  
  cost.message <- paste0(
    "Estimates of Total Annualized Cost ranged from ",
    sprintf(
      "$%s",
      formatC(CI95.low, format = "f", big.mark = ",", digits = 2)
    ),
    " to ",
    sprintf(
      "$%s",
      formatC(CI95.high, format = "f", big.mark = ",", digits = 2)
    ),
    ". RBEROST costs are usually most influenced by the amount of retrofitting necessary to install urban BMPs. More retrofitting leads to higher costs."
  )
  
  return(cost.message)
}

# get_loading_targets <- function(results_astable) {
#   
#   loads_lims_lines <- results_astable[
#     which(grepl(results_astable[, 1], pattern = "loads_lim_")),1
#   ]
#   
#   targets <- str_extract(
#     loads_lims_lines,
#     pattern = "(?<=loads_lim_)(.*?)(?= = )"
#   )
#   
#   loads_lims <- data.frame(
#     Nutrient = substr(targets, 1,1),
#     Number = as.numeric(as.character(substr(targets, 2, nchar(targets)))),
#     Limit =  as.numeric(
#       as.character(str_extract(loads_lims_lines, pattern = "(?<= = )(.*)"))
#     )
#   )
#   
#   return(loads_lims)
# }

#### CTC edited: 1/26/2026 ####

get_loading_targets <- function(results_astable) {
  
  loads_lims_lines <- results_astable[
    which(grepl(results_astable[, 1], pattern = "loads_lim_")),1
  ]
  
  targets <- str_extract( 
    loads_lims_lines, 
    pattern = "(?<=loads_lim_)(.*?)(?= :=)"
  )
  
  season_lims_lines <-
    results_astable[
      which(grepl(results_astable[, 1], pattern = "limits")),1
    ]
  
  loads_lims <- data.frame(
    Nutrient = substr(targets, 1,1),
    Number = as.numeric(as.character(substr(targets, 2, nchar(targets)))),
    Limit =  as.numeric(
      as.character(str_extract(season_lims_lines, pattern = "\\d+(?:\\.\\d+)?(?=\\s*$)"))
    )
  ) %>%
    group_by(Nutrient, Number) %>% # Note, included to create annual estimates
    summarize(Limit = sum(Limit), .groups = "keep") %>%
    ungroup() %>%
    as.data.frame()
  
  return(loads_lims)
}

#### end edits ####

get_loading_bootstraps <- function(
  results_astable, scenarionumber, x, endrows, startrows
) {
  
  display_txt <- "sum[{]c in comid_"
  # load_txt <- "other_loads"
  load_txt <- "inc_storage_load" # CTC edited: 1/26/2026
  
  loading_bootstraps_begin_all <- which(
    grepl(results_astable[, 1], pattern = display_txt) &
      str_detect(results_astable[, 1], load_txt)
  )
  loading_bootstraps_begin <- loading_bootstraps_begin_all[
    (length(loading_bootstraps_begin_all) / x * (scenarionumber - 1) + 1):
      (length(loading_bootstraps_begin_all) / x * (scenarionumber))
  ]
  
  if(
    (length(loading_bootstraps_begin) > 0) &
    all(!unlist(lapply(loading_bootstraps_begin, is.na)))
  ) {
    loading_bootstraps <- data.frame(
      foreach(
        i = 1:length(loading_bootstraps_begin), .combine = "rbind"
      ) %do% {
        loading_bootstraps_end <- min(
          endrows[
            which(endrows > loading_bootstraps_begin[i])
          ]
        )
        
        # if(scenarionumber == 1) {
        #   loading_bootstraps_tablebegin <- min(
        #     startrows[
        #       which(startrows > loading_bootstraps_begin[i])
        #     ]
        #   )
        # } else {
        #   loading_bootstraps_tablebegin <- loading_bootstraps_begin[i]
        # }
        
        loading_bootstraps_tablebegin <- loading_bootstraps_begin[i] # CTC edited: 1/26/2026
        
        load.designation <- str_extract(
          results_astable[loading_bootstraps_begin[i],1],
          # pattern = "(?<=other_loads_)(.*?)(?=_rev)"
          pattern = "(?<=inc_storage_load_)(.*?)(?=_rev)" # CTC edited 1/26/2026
        )
        
        load.nutrient <- substr(load.designation, 1, 1)
        load.number <- as.numeric(
          as.character(substr(load.designation, 2, nchar(load.designation)))
        )
        
        if(
          (loading_bootstraps_tablebegin + 1) < (loading_bootstraps_end - 1)
        ) {
          
          loading_bootstraps.tmp <- data.frame(
            str_split_fixed(
              str_squish( # CTC edited: 1/26/2026
              parse_character(
                results_astable[
                  (loading_bootstraps_tablebegin + 1):
                    (loading_bootstraps_end - 1),
                  1
                ]
              # ), " ", n = 2
              )), " ", n = 4 # CTC edited: 1/26/2026
            )
          ) %>%
            # rename(Bootstrap = 1, Load = 2) %>%
            rename(Bootstrap = 1, Season = 2, Year = 3, Load = 4) %>% # CTC edited: 1/26/2026
            mutate(
              across(.fns = ~as.numeric(as.character(.))),
              Nutrient = load.nutrient,
              Number = load.number
            ) %>%
            group_by(Bootstrap, Year, Nutrient, Number) %>% # CTC edited: 1/26/2026 Note, included to create annual estimates
            summarize(Load = sum(Load), .groups = "keep") %>% 
            ungroup()
          
        } else {loading_bootstraps.tmp <- NULL}
        
        loading_bootstraps.tmp
      }
    )
    
  } else { loading_bootstraps <- NULL}
  
  
  return(loading_bootstraps)
}

#@ SE: updated 6-5-24
mung_UserSpecs_loadingtargets <- function(loadingtargets) {
  UserSpecs_loadingtargets_munged <- loadingtargets %>%
    mutate(Nutrient = gsub("T", "", TN_or_TP), ticker = 1) %>%
    #group_by(Nutrient) %>% # CTC updated 11/4/2024
    #mutate(Number = cumsum(ticker)) %>% ## CTC updated 11/4/2024
    select(watershed_name, adjusted_daily_target_kgperday, adjusted_annual_target_kgperyr, Nutrient, Number)
  
  return(UserSpecs_loadingtargets_munged)
}

#@ SE: updated 6-5-24
get_loading_uncertainty_message <- function(
    loadings, loadingtargets, loadingtargetsmunged
) {
  
  CI.summary <- loadings %>%
    left_join(., loadingtargets, by = c("Nutrient", "Number")) %>%
    left_join(
      ., loadingtargetsmunged, by = c("Nutrient", "Number")
    ) %>%
    group_by(Nutrient, Number, Limit, watershed_name, adjusted_annual_target_kgperyr) %>%
    summarize(
      CI95.low = quantile(Load, 0.025),
      CI95.high = quantile(Load, 0.975),
      prob.met = sum(Load < Limit) / n(),
      .groups = "keep"
    ) %>%
    mutate(
      Target.name = watershed_name,
      LoadingUncertaintyMessage = paste0(
        formatC(CI95.low, format = "f", big.mark = ",", digits = 1),
        " - ",
        formatC(CI95.high, format = "f", big.mark = ",", digits = 1),
        " kg ",
        paste(Nutrient),
        "/yr"
      ),
      Limit.display = paste0(
        signif(adjusted_annual_target_kgperyr, 2),
        "kg reduction in T",
        Nutrient,
        "; or ",
        formatC(Limit, format = "f", big.mark = ",", digits = 1),
        " kg ",
        paste(Nutrient),
        "/yr"
      )
    ) %>%
    ungroup() %>%
    select(Target.name, Limit.display, LoadingUncertaintyMessage, prob.met)
  
  return(CI.summary)
}


read_postprocessor_point_bmp_vec <-function(results_astable, scenarionumber) {
  
  point_txt <- "point_dec [*,*]"
  
  # Check if point section exists
  point_line <- which(results_astable[, 1] == point_txt)
  if(length(point_line) == 0) {
    return(c())
  }
  
  point_bmp_tmp <- results_astable[
    (point_line + 1), 1
  ][scenarionumber]
  
  if(
    is.na(point_bmp_tmp) ||
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac|NEOS|Presolve|^\\s*$", 
      point_bmp_tmp
    )
  ) {
    return(c())
  }
  
  point_bmp <- strsplit(point_bmp_tmp, "\\s+")
  point_bmp_vec_tmp <- as.vector(unlist(point_bmp))
  point_bmp_vec_tmp1 <- point_bmp_vec_tmp[
    !sapply(point_bmp_vec_tmp, function(x) all(x == ""))
  ]
  
  if(length(point_bmp_vec_tmp1) <= 2) {
    return(c())
  }
  
  point_bmp_vec <- point_bmp_vec_tmp1[-c(1, length(point_bmp_vec_tmp1))]
  
  return(point_bmp_vec)
}

parse_point_results <- function(
  results_astable, scenarionumber, endrows, pointbmps
) {
  
  point_txt <- "point_dec [*,*]"
  
  point_rows_begin <- which(results_astable[, 1] == point_txt)[scenarionumber]
  
  if(length(point_rows_begin) > 0 && !is.na(point_rows_begin)) {
    
    # Check if there's actual data after the header
    potential_data_line <- point_rows_begin + 2
    if(potential_data_line <= nrow(results_astable)) {
      data_line <- results_astable[potential_data_line, 1]
      
      # If the data line is empty or contains other section headers, return NULL
      if(is.na(data_line) || 
         grepl("^\\s*$|ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac", data_line)) {
        return(NULL)
      }
    } else {
      return(NULL)
    }
    
    point_rows_end <- min(endrows[which(endrows > point_rows_begin)])
    
    point_rows <- results_astable[
      (point_rows_begin + 2):(point_rows_end - 1), 1
    ]
    
    # Filter out empty rows
    point_rows <- point_rows[!grepl("^\\s*$", point_rows)]
    
    if(length(point_rows) == 0) {
      return(NULL)
    }
    
    point_tmp <- strsplit(point_rows, "\\s+")
    
    # Check if we have valid data after splitting
    if(length(point_tmp) == 0 || all(sapply(point_tmp, length) == 0)) {
      return(NULL)
    }
    
    point_results <- do.call(rbind.data.frame, point_tmp)
    
    # Check if we have the expected number of columns
    expected_cols <- length(pointbmps) + 1  # +1 for COMID
    if(ncol(point_results) != expected_cols) {
      return(NULL)
    }
    
    names(point_results) <- c("COMID", pointbmps)
    
    # Adjust the formatting of the NEOS results
    point_results$COMID_adj <- gsub("'", "", point_results$COMID)
    point_results$COMID_int <- strtoi(point_results$COMID_adj)
    point_results <- point_results %>%
      select(COMID = COMID_int, all_of(pointbmps))
  } else {point_results <- NULL}
  
  return(point_results)
}

read_postprocessor_urban_bmp_vec <- function(results_astable, scenarionumber) {
  
  # Search term within NEOS results - check for all possible formats
  urban_txt_formats <- c(
    "urban_frac [*,*]", 
    "urban_frac_by_comid_bmp [*,*]",
    "sum{d in urban_land_uses, n in urban_soil_groups} urban_frac[c,d,n,u] [*,*]"
  )
  
  # Find which format exists in the results
  urban_txt_line <- NULL
  for(format in urban_txt_formats) {
    line_match <- which(results_astable[, 1] == format)
    if(length(line_match) > 0) {
      urban_txt_line <- line_match[scenarionumber]
      break
    }
  }
  
  # If no urban section found, return empty vector
  if(is.null(urban_txt_line) || length(urban_txt_line) == 0) {
    return(c())
  }
  
  # Find the line where the urban results begin
  urban_bmp_tmp <- results_astable[(urban_txt_line + 1), 1]
  
  # Error check - if the next line contains other sections or is empty, return empty
  if(
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac|NEOS|Presolve|^\\s*$", 
      urban_bmp_tmp
    ) || is.na(urban_bmp_tmp)
  ) {
    return(c())
  }
  
  # Use str_match_all to match all occurrences of text within apostrophes
  urban_bmp <- stringr::str_match_all(urban_bmp_tmp, "\\S+")
  
  # Convert the result to a vector
  urban_bmp_vec <- as.vector(unlist(urban_bmp))
  
  # Remove spaces and special characters from the vector
  keep <- !sapply(urban_bmp_vec, function(x) x %in% c(":", ":="))
  urban_bmp_vec <- urban_bmp_vec[keep]
  urban_bmp_vec <- stringr::str_replace_all(urban_bmp_vec, " ", "")
  
  return(urban_bmp_vec)
  
}

parse_urban_results <- function(
    results_astable, scenarionumber, endrows, urbanbmps
) {
  
  # Check for all possible formats
  urban_txt_formats <- c(
    "urban_frac [*,*]", 
    "urban_frac_by_comid_bmp [*,*]",
    "sum{d in urban_land_uses, n in urban_soil_groups} urban_frac[c,d,n,u] [*,*]"
  )
  
  urban_rows_begin <- NULL
  for(format in urban_txt_formats) {
    line_match <- which(results_astable[, 1] == format)
    if(length(line_match) > 0) {
      urban_rows_begin <- line_match[scenarionumber]
      break
    }
  }
  
  if(length(urban_rows_begin) > 0 && !is.na(urban_rows_begin)) {
    
    # Check if there's actual data after the header
    potential_data_line <- urban_rows_begin + 2
    if(potential_data_line <= nrow(results_astable)) {
      data_line <- results_astable[potential_data_line, 1]
      
      # If the data line is empty or contains other section headers, return NULL
      if(is.na(data_line) || 
         grepl("^\\s*$|ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac", data_line)) {
        return(NULL)
      }
    } else {
      return(NULL)
    }
    
    urban_rows_end <- min(endrows[which(endrows > urban_rows_begin)])
    
    urban_rows <- results_astable[
      (urban_rows_begin + 2):(urban_rows_end - 1), 1
    ]
    
    # Filter out empty rows
    urban_rows <- urban_rows[!grepl("^\\s*$", urban_rows)]
    
    if(length(urban_rows) == 0) {
      return(NULL)
    }
    
    urban_tmp <- strsplit(urban_rows, "\\s+")
    
    # Check if we have valid data after splitting
    if(length(urban_tmp) == 0 || all(sapply(urban_tmp, length) == 0)) {
      return(NULL)
    }
    
    # rename are urban_results_temp after fix
    urban_results_temp <- do.call(rbind.data.frame, urban_tmp)
    
    # Check if we have the expected number of columns
    expected_cols <- length(urbanbmps) + 1  # +1 for COMID
    if(ncol(urban_results_temp) != expected_cols) {
      return(NULL)
    }
    
    names(urban_results_temp) <- c("COMID", urbanbmps)
    
    # Adjust the formatting of the NEOS results
    urban_results_temp$COMID_adj <- gsub("'", "", urban_results_temp$COMID)
    split_comid <- strsplit(urban_results_temp$COMID_adj, "_")
    urban_results_temp$COMID <- as.integer(sapply(split_comid, `[`, 1))
    
    # Check if Land_Use and HSG information is available in the COMID format
    land_use_values <- sapply(split_comid, `[`, 2)
    hsg_values <- sapply(split_comid, `[`, 3)
    
    if(all(is.na(land_use_values))) {
      # No Land_Use information in COMID, set default values
      urban_results_temp$Land_Use <- "URHD"  # Default land use
    } else {
      urban_results_temp$Land_Use <- land_use_values
    }
    
    if(all(is.na(hsg_values))) {
      # No HSG information in COMID, set default value
      urban_results_temp$HSG <- "C"  # Default HSG value
    } else {
      urban_results_temp$HSG <- hsg_values
    }
    
    urban_results <- urban_results_temp[c("COMID", "Land_Use", "HSG", urbanbmps)]
    
  } else {urban_results <- NULL}
  
  return(urban_results)
}

read_postprocessor_road_bmp_vec <-function(results_astable, scenarionumber) {
  
  # Check for all possible formats
  road_txt_formats <- c(
    "road_frac [*,*]", 
    "road_frac_by_comid_bmp [*,*]",
    "sum{h in road_soil_groups} road_frac[c,h,r] [*,*]"
  )
  
  # Find which format exists in the results
  road_txt_line <- NULL
  for(format in road_txt_formats) {
    line_match <- which(results_astable[, 1] == format)
    if(length(line_match) > 0) {
      road_txt_line <- line_match[scenarionumber]
      break
    }
  }
  
  # If no road section found, return empty vector
  if(is.null(road_txt_line) || length(road_txt_line) == 0) {
    return(c())
  }
  
  # Find the line where the road results begin
  road_bmp_tmp <- results_astable[(road_txt_line + 1), 1]
  
  # Error check - if the next line contains other sections or is empty, return empty
  if(
    grepl(
      "ag_frac|urban_frac|road_frac|point_dec|ripbuf_length|graz_frac|NEOS|Presolve|^\\s*$", 
      road_bmp_tmp
    ) || is.na(road_bmp_tmp)
  ) {
    return(c())
  }

  # Use str_match_all to match all occurrences of text within apostrophes
  road_bmp <- stringr::str_match_all(road_bmp_tmp, "\\S+")
  
  # Convert the result to a vector
  road_bmp_vec <- as.vector(unlist(road_bmp))
  
  # Remove spaces from each element of the vector
  keep <- !sapply(road_bmp_vec, function(x) x %in% c(":", ":="))
  road_bmp_vec <- road_bmp_vec[keep]
  road_bmp_vec <- stringr::str_replace_all(road_bmp_vec, " ", "")
  
  return(road_bmp_vec)
}

parse_road_results <- function(
    results_astable, scenarionumber, endrows, roadbmps
) {
  
  # Check for all possible formats
  road_txt_formats <- c(
    "road_frac [*,*]", 
    "road_frac_by_comid_bmp [*,*]",
    "sum{h in road_soil_groups} road_frac[c,h,r] [*,*]"
  )
  
  road_rows_begin <- NULL
  for(format in road_txt_formats) {
    line_match <- which(results_astable[, 1] == format)
    if(length(line_match) > 0) {
      road_rows_begin <- line_match[scenarionumber]
      break
    }
  }
  
  if(length(road_rows_begin) > 0 && !is.na(road_rows_begin)) {
    
    # Check if there's actual data after the header
    potential_data_line <- road_rows_begin + 2
    if(potential_data_line <= nrow(results_astable)) {
      data_line <- results_astable[potential_data_line, 1]
      
      # If the data line is empty or contains other section headers, return NULL
      if(is.na(data_line) || 
         grepl("^\\s*$|ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac", data_line)) {
        return(NULL)
      }
    } else {
      return(NULL)
    }
    
    road_rows_end <- min(endrows[which(endrows > road_rows_begin)])
    
    road_rows <- results_astable[
      (road_rows_begin + 2):(road_rows_end - 1), 1
    ]
    
    # Filter out empty rows
    road_rows <- road_rows[!grepl("^\\s*$", road_rows)]
    
    if(length(road_rows) == 0) {
      return(NULL)
    }
    
    road_tmp <- strsplit(road_rows, "\\s+")
    
    # Check if we have valid data after splitting
    if(length(road_tmp) == 0 || all(sapply(road_tmp, length) == 0)) {
      return(NULL)
    }
    
    road_results <- do.call(rbind.data.frame, road_tmp)
    
    # Check if we have the expected number of columns
    expected_cols <- length(roadbmps) + 1  # +1 for COMID
    if(ncol(road_results) != expected_cols) {
      return(NULL)
    }
    
    names(road_results) <- c("COMID", roadbmps)
    
    head(road_results)
    
    # Adjust the formatting of the NEOS results
    road_results$COMID_adj <- gsub("'", "", road_results$COMID)
    split_comid <- strsplit(road_results$COMID_adj, "_")
    road_results$COMID <- as.integer(sapply(split_comid, `[`, 1))
    
    # Check if HSG information is available in the COMID format
    hsg_values <- sapply(split_comid, `[`, 2)
    if(all(is.na(hsg_values))) {
      # No HSG information in COMID, set as character to avoid issues
      road_results$HSG <- "C"  # Default HSG value
    } else {
      road_results$HSG <- hsg_values
    }
    
    road_results <- road_results[c("COMID", "HSG", roadbmps)]
  } else {road_results <- NULL}
  
  return(road_results)
}

read_postprocessor_ag_bmp_vec <- function(results_astable, scenarionumber) {
  
  ag_txt <- "ag_frac [*,*]"
  
  # Check if ag section exists
  ag_line <- which(results_astable[, 1] == ag_txt)
  if(length(ag_line) == 0) {
    return(c())
  }
  
  ag_bmp_tmp <- results_astable[
    (ag_line + 1), 1
  ][scenarionumber]
  
  if(
    is.na(ag_bmp_tmp) ||
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac|NEOS|Presolve|^\\s*$", ag_bmp_tmp
    )
  ) {
    return(c())
  }
  
  ag_bmp <- strsplit(ag_bmp_tmp, "\\s+")
  ag_bmp_vec_tmp <- as.vector(unlist(ag_bmp))
  ag_bmp_vec_tmp1 <- ag_bmp_vec_tmp[
    !sapply(ag_bmp_vec_tmp, function(x) all(x == ""))
  ]
  
  if(length(ag_bmp_vec_tmp1) <= 2) {
    return(c())
  }
  
  ag_bmp_vec <- ag_bmp_vec_tmp1[-c(1, length(ag_bmp_vec_tmp1))]
  
  return(ag_bmp_vec)
}

parse_ag_results <- function(results_astable, scenarionumber, endrows, agbmps) {
  
  ag_txt <- "ag_frac [*,*]"
  
  ag_rows_begin <- which(results_astable[, 1] == ag_txt)[scenarionumber]
  
  if(length(ag_rows_begin) > 0 && !is.na(ag_rows_begin)) {
    
    # Check if there's actual data after the header
    potential_data_line <- ag_rows_begin + 2
    if(potential_data_line <= nrow(results_astable)) {
      data_line <- results_astable[potential_data_line, 1]
      
      # If the data line is empty or contains other section headers, return NULL
      if(is.na(data_line) || 
         grepl("^\\s*$|ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac", data_line)) {
        return(NULL)
      }
    } else {
      return(NULL)
    }
    
    ag_rows_end <- min(endrows[which(endrows > ag_rows_begin)])
    
    ag_rows <- results_astable[(ag_rows_begin + 2):(ag_rows_end - 1), 1]
    
    # Filter out empty rows
    ag_rows <- ag_rows[!grepl("^\\s*$", ag_rows)]
    
    if(length(ag_rows) == 0) {
      return(NULL)
    }
    
    ag_tmp <- strsplit(ag_rows, "\\s+")
    
    # Check if we have valid data after splitting
    if(length(ag_tmp) == 0 || all(sapply(ag_tmp, length) == 0)) {
      return(NULL)
    }
    
    ag_results <- do.call(rbind.data.frame, ag_tmp)
    
    # Check if we have the expected number of columns
    expected_cols <- length(agbmps) + 1  # +1 for COMID
    if(ncol(ag_results) != expected_cols) {
      return(NULL)
    }
    
    names(ag_results) <- c("COMID", agbmps)
    
    ag_results$COMID_adj <- gsub("'", "", ag_results$COMID)
    ag_results$COMID_int <- strtoi(ag_results$COMID_adj)
    ag_results <- ag_results %>%
      select(COMID = COMID_int, all_of(agbmps))
    
  } else {ag_results <- NULL}
  
  return(ag_results)
}


read_postprocessor_ripbuf_bmp_vec <- function(results_astable, scenarionumber) {
  
  ripbuf_txt <- "ripbuf_length [*,*]"
  
  # Check if ripbuf section exists
  ripbuf_line <- which(results_astable[, 1] == ripbuf_txt)
  if(length(ripbuf_line) == 0) {
    return(c())
  }
  
  ripbuf_bmp_tmp <- results_astable[
    (ripbuf_line + 1), 1
  ][scenarionumber]
  
  if(
    is.na(ripbuf_bmp_tmp) ||
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac|NEOS|Presolve|^\\s*$", 
      ripbuf_bmp_tmp
    )
  ) {
    return(c())
  }
  
  ripbuf_bmp <- strsplit(ripbuf_bmp_tmp, "\\s+")
  ripbuf_bmp_vec_tmp <- as.vector(unlist(ripbuf_bmp))
  ripbuf_bmp_vec_tmp1 <- ripbuf_bmp_vec_tmp[
    !sapply(ripbuf_bmp_vec_tmp, function(x) all(x == ""))
  ]
  
  if(length(ripbuf_bmp_vec_tmp1) <= 2) {
    return(c())
  }
  
  ripbuf_bmp_vec <- ripbuf_bmp_vec_tmp1[-c(1, length(ripbuf_bmp_vec_tmp1))]
  
  return(ripbuf_bmp_vec)
}


parse_ripbuf_results <- function(
  results_astable, scenarionumber, endrows, riparianbmps
) {
  ripbuf_txt <- "ripbuf_length [*,*]"
  
  ripbuf_rows_begin <- which(results_astable[, 1] == ripbuf_txt)[scenarionumber]
  
  if(length(ripbuf_rows_begin) > 0 & !is.na(ripbuf_rows_begin)) {
    
    # Check if there's actual data after the header
    potential_data_line <- ripbuf_rows_begin + 2
    if(potential_data_line <= nrow(results_astable)) {
      data_line <- results_astable[potential_data_line, 1]
      
      # If the data line is empty or contains other section headers, return NULL
      if(is.na(data_line) || 
         grepl("^\\s*$|ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac", data_line)) {
        return(NULL)
      }
    } else {
      return(NULL)
    }
    
    if(length(which(endrows > ripbuf_rows_begin)) > 0) {
      ripbuf_rows_end <- min(
        endrows[which(endrows > ripbuf_rows_begin)]
      )
    } else {ripbuf_rows_end <- ripbuf_rows_begin}
    
    ripbuf_rows <- results_astable[
      (ripbuf_rows_begin + 2):(ripbuf_rows_end - 1), 1
    ]
    
    # Filter out empty rows
    ripbuf_rows <- ripbuf_rows[!grepl("^\\s*$", ripbuf_rows)]
    
    if(length(ripbuf_rows) == 0) {
      return(NULL)
    }
    
    ripbuf_tmp <- strsplit(ripbuf_rows, "\\s+")
    
    ripbuf_results <- do.call(rbind.data.frame, ripbuf_tmp)
    
    names(ripbuf_results) <- c("COMID", riparianbmps)
    
    ripbuf_results$COMID_adj <- gsub("'", "", ripbuf_results$COMID)
    ripbuf_results$COMID_int <- strtoi(ripbuf_results$COMID_adj)
    ripbuf_results <- ripbuf_results %>%
      select(COMID = COMID_int, all_of(riparianbmps))
  } else {
    ripbuf_results <- NULL
  }
  
  return(ripbuf_results)
}

#### CTC edited: 2/9/2026 - transferred from dynamic LIS RBEROST ####

# Adding in functions to parse septic results from NEOS file
read_postprocessor_septic_bmp_vec <- function(results_astable, scenarionumber) {
  
  # septic_txt <- "septic_frac :="
  septic_txt <- "septic_frac [*,*]" # CTC: edited 12-30-2025
  septic_bmp_tmp <- results_astable[
    (which(results_astable[, 1] == septic_txt) + 1), 1
  ][scenarionumber]
  
  if(
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|NEOS|Presolve", septic_bmp_tmp
    )
  ) {septic_bmp_tmp <- ""}
  
  septic_bmp <- strsplit(septic_bmp_tmp, "\\s+")
  septic_bmp_vec_tmp <- as.vector(unlist(septic_bmp))
  septic_bmp_vec_tmp1 <- septic_bmp_vec_tmp[
    !sapply(septic_bmp_vec_tmp, function(x) all(x == ""))
  ]
  septic_bmp_vec <- septic_bmp_vec_tmp1[-c(1, length(septic_bmp_vec_tmp1))]
  
  return(septic_bmp_vec)
}

parse_septic_results <- function(results_astable, scenarionumber, endrows, septicbmps) {
  
  # septic_txt <- "septic_frac :="
  septic_txt <- "septic_frac [*,*]" # CTC: edited 12-30-2025
  
  septic_rows_begin <- which(results_astable[, 1] == septic_txt)[scenarionumber]
  
  if(length(septic_rows_begin) > 0) {
    
    septic_rows_end <- min(endrows[which(endrows > septic_rows_begin)])
    
    septic_rows <- results_astable[
      (septic_rows_begin + 2):(septic_rows_end - 1), 1
    ]
    
    septic_tmp <- strsplit(septic_rows, "\\s+")
    
    septic_results <- do.call(rbind.data.frame, septic_tmp)
    
    names(septic_results) <- c("COMID", septicbmps)
    
    septic_results$COMID_adj <- gsub("'", "", septic_results$COMID)
    septic_results$COMID_int <- strtoi(septic_results$COMID_adj)
    septic_results <- septic_results %>%
      select(COMID = COMID_int, all_of(septicbmps))
    
  } else {septic_results <- NULL}
  
  return(septic_results)
}

merge_septic_results <- function(septicresults, areadf, septicbmps, km2_to_ac) {
  
  # Merge the COMID to HUC12 crosswalk to the septic BMP results
  septic_results_HUCmerge <- merge(
    septicresults, areadf, by = "COMID", all.x = TRUE
  )
  
  # Calculate BMP implementation (parcels) by HUC12
  for (i in 1:length(septicbmps)) {
    septic_results_HUCmerge[septicbmps[i]] <- sapply(
      sapply(septic_results_HUCmerge[septicbmps[i]], as.character), 
      as.numeric
    )
    colname <- paste(septicbmps[i], "parcels", sep = "_")
    septic_results_HUCmerge[colname] <- septic_results_HUCmerge[septicbmps[i]]
  }
  
  foreach(i = 1:length(septicbmps)) %do% {
    attr(septic_results_HUCmerge[, septicbmps[i]], 'dimnames') <- NULL
  }
  
  return(septic_results_HUCmerge)
}

present_septic_results_HUC <- function(septicresults, septicbmps) {
  septic_results_HUC <- data.frame(septicresults) %>%
    rename_at(
      vars(one_of(septicbmps)), list( ~ paste0(., "_ParcelsImplement"))
    ) %>%
    select(
      COMID,
      HUC_12,
      IncAreaKm2,
      StateAbbrev,
      contains(septicbmps)
    )
  
  return(septic_results_HUC)
}

calculate_septic_total <- function(septicresults, septicbmps) {
  
  septic_total <- data.frame(
    lapply(
      septicresults[paste0(septicbmps, "_parcels")], sum
    )
  )
  
  septic_total[paste0(septicbmps, "_parcels")] <- format(
    septic_total[paste0(septicbmps, "_parcels")],
    big.mark = ',',
    scientific = FALSE,
    # digits = 0, 
    digits = NULL, # CTC: edited 12-30-2025
    nsmall = 0
  )
  
  names(septic_total) <- gsub("_", " ", paste0(septicbmps, "_parcels"))
  names(septic_total) <- gsub("parcels", "(parcels)", names(septic_total))
  
  return(septic_total)
}

#### end edits ####

read_postprocessor_graz_bmp_vec <-function(results_astable, scenarionumber) {
  
  graz_txt <- "graz_frac [*,*]"
  
  # Check if graz section exists
  graz_line <- which(results_astable[, 1] == graz_txt)
  if(length(graz_line) == 0) {
    return(c())
  }
  
  graz_bmp_tmp <- results_astable[
    (graz_line + 1), 1
  ][scenarionumber]
  
  if(
    is.na(graz_bmp_tmp) ||
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac|NEOS|Presolve|^\\s*$", 
      graz_bmp_tmp
    )
  ) {
    return(c())
  }
  
  graz_bmp <- strsplit(graz_bmp_tmp, "\\s+")
  graz_bmp_vec_tmp <- as.vector(unlist(graz_bmp))
  graz_bmp_vec_tmp1 <- graz_bmp_vec_tmp[
    !sapply(graz_bmp_vec_tmp, function(x) all(x == ""))
  ]
  
  if(length(graz_bmp_vec_tmp1) <= 2) {
    return(c())
  }
  
  graz_bmp_vec <- graz_bmp_vec_tmp1[-c(1, length(graz_bmp_vec_tmp1))]
  
  return(graz_bmp_vec)
}

parse_graz_results <- function(
  results_astable, scenarionumber, endrows, grazbmps
) {
  
  graz_txt <- "graz_frac [*,*]"
  
  graz_rows_begin <- which(results_astable[, 1] == graz_txt)[scenarionumber]
  
  if(length(graz_rows_begin) > 0 && !is.na(graz_rows_begin)) {
    
    # Check if there's actual data after the header
    potential_data_line <- graz_rows_begin + 2
    if(potential_data_line <= nrow(results_astable)) {
      data_line <- results_astable[potential_data_line, 1]
      
      # If the data line is empty or contains other section headers, return NULL
      if(is.na(data_line) || 
         grepl("^\\s*$|ag_frac|urban_frac|point_dec|ripbuf_length|graz_frac|road_frac", data_line)) {
        return(NULL)
      }
    } else {
      return(NULL)
    }
    
    graz_rows_end <- min(endrows[which(endrows > graz_rows_begin)])
    
    graz_rows <- results_astable[
      (graz_rows_begin + 2):(graz_rows_end - 1), 1
    ]
    
    # Filter out empty rows
    graz_rows <- graz_rows[!grepl("^\\s*$", graz_rows)]
    
    if(length(graz_rows) == 0) {
      return(NULL)
    }
    
    graz_tmp <- strsplit(graz_rows, "\\s+")
    
    # Check if we have valid data after splitting
    if(length(graz_tmp) == 0 || all(sapply(graz_tmp, length) == 0)) {
      return(NULL)
    }
    
    graz_results <- do.call(rbind.data.frame, graz_tmp)
    
    # Check if we have the expected number of columns
    expected_cols <- length(grazbmps) + 1  # +1 for COMID
    if(ncol(graz_results) != expected_cols) {
      return(NULL)
    }
    
    names(graz_results) <- c("COMID", grazbmps)
    
    # Adjust the formatting of the NEOS results
    graz_results$COMID_adj <- gsub("'", "", graz_results$COMID)
    graz_results$COMID_int <- strtoi(graz_results$COMID_adj)
    graz_results <- graz_results %>%
      select(COMID = COMID_int, all_of(grazbmps))
  } else {graz_results <- NULL}
  
  return(graz_results)
}
##################################################
# 3. Combine NEOS results with other dataframes ####
##################################################

make_areadf <- function(sparrowinputs, km2_to_ac, streamcatinputs) {
  comid_huc12 <- sparrowinputs %>%
    rename(comid = 1) %>%
    select(comid, HUC_12 = huc12, IncAreaKm2, urban_km2 = developed11_km2) %>%
    mutate(HUC12_Char = str_pad(HUC_12, width = 12, pad = "0")) %>%
    select(COMID = comid,
           HUC_12 = HUC12_Char,
           IncAreaKm2,
           urban_km2)
  
  cropland <- imap_dfr(
    streamcatinputs, ~{return(.x %>% mutate(StateAbbrev = str_sub(.y, 1, 2)))}
  )
  
  streamcat_crop <- cropland %>%
    select(
      c("COMID", "PctCrop2019Cat", "PctHay2019Cat", "WsAreaSqKm", "StateAbbrev")
    ) %>%
    mutate(WsAreaAc = WsAreaSqKm / km2_to_ac)
  
  # Calculate available agricultural area
  area <- merge(comid_huc12, streamcat_crop, by = "COMID")
  area <- area[!duplicated(area$COMID),] %>%
    mutate(
      ag_km2 = (PctCrop2019Cat / 100) * IncAreaKm2,
      graz_km2 = (PctHay2019Cat / 100) * IncAreaKm2,
      COMID = as.character(COMID)
    )
  
  return(area)
}

make_urban_areadf <- function(sparrowinputs, km2_to_ac, urban_input) {
  comid_huc12 <- sparrowinputs %>%
    rename(comid = 1) %>%
    select(comid, HUC_12 = huc12, IncAreaKm2, urban_km2 = developed11_km2) %>%
    mutate(HUC12_Char = str_pad(HUC_12, width = 12, pad = "0")) %>%
    select(COMID = comid,
           HUC_12 = HUC12_Char,
           IncAreaKm2,
           urban_km2)
  
  urban_area_tmp <- urban_input %>%
    tibble() %>%
    mutate(area_m2 = Area, 
           # Convert area in meters to acres for output
           acre_area = area_m2 * 0.000247105) %>%
    select(Land_Use, HSG, 'COMID' = comid, acre_area) %>%
    distinct() %>%
    group_by(COMID) %>% 
    mutate(tot_urban_area = sum(acre_area)) %>% 
    ungroup() %>%
    units::drop_units()
  
  # Calculate available agricultural area
  urban_area <- merge(comid_huc12, urban_area_tmp, by = "COMID")
  
  return(urban_area)
}

make_road_areadf <- function(sparrowinputs, km2_to_ac, road_input) {
  comid_huc12 <- sparrowinputs %>%
    rename(comid = 1) %>%
    select(comid, HUC_12 = huc12) %>%
    mutate(HUC12_Char = str_pad(HUC_12, width = 12, pad = "0")) %>%
    select(COMID = comid,
           HUC_12 = HUC12_Char)
  
  road_area_tmp <- road_input %>% 
    mutate(area_m2 = Area,
           area_sum_m2 = area_sum,
           area_sum_ac = area_sum_m2 * 0.000247105,
           road_area_ac = area_m2 * 0.000247105) %>%
    select(HSG, 'COMID' = comid, road_area_ac, area_sum_ac) %>%
    distinct() %>%
    group_by(COMID) %>% 
    mutate(tot_area_ac = sum(area_sum_ac)) %>% 
    ungroup() %>%
    units::drop_units() %>%
    select(-area_sum_ac)
  
  # Check if HSG column has valid data, if not set a default
  if(all(is.na(road_area_tmp$HSG))) {
    road_area_tmp$HSG <- "C"  # Default HSG value
  }
  
  # Calculate available road area
  road_area <- merge(comid_huc12, road_area_tmp, by = "COMID", all.x = TRUE)
  
  return(road_area)
}

merge_urban_results <- function(urbanresults, areadf, urbanbmps, km2_to_ac) {
  # Merge the SPARROW and StreamCat data to the urban BMP results
  # First try merging by COMID, Land_Use, and HSG
  urban_results_HUCmerge <- merge(urbanresults,
                                  areadf,
                                  by = c("COMID", "Land_Use", "HSG"),
                                  all.x = TRUE)
  
  # If merge failed (all HUC_12 values are NA), try merging by COMID only
  if(all(is.na(urban_results_HUCmerge$HUC_12))) {
    # Try merging just by COMID
    areadf_simple <- areadf %>%
      group_by(COMID) %>%
      summarise(
        HUC_12 = first(HUC_12),
        IncAreaKm2 = first(IncAreaKm2),
        urban_km2 = first(urban_km2),
        acre_area = sum(acre_area, na.rm = TRUE),
        tot_urban_area = first(tot_urban_area),
        .groups = 'drop'
      )
    
    urban_results_HUCmerge <- merge(urbanresults,
                                    areadf_simple,
                                    by = "COMID",
                                    all.x = TRUE)
  }

  # Calculate BMP implementation area (acres) (note: this used to sum by HUC12 - stakeholders indicated that the ComID resolution would be more helpful)
  for (i in 1:length(urbanbmps)) {
    urban_results_HUCmerge[urbanbmps[i]] <- sapply(
      sapply(urban_results_HUCmerge[urbanbmps[i]], as.character), 
      as.numeric
    )
    colname <- paste(urbanbmps[i], "areaac", sep = "_")
    
    # Handle cases where acre_area might be NA
    acre_area_values <- ifelse(is.na(urban_results_HUCmerge$acre_area), 0, urban_results_HUCmerge$acre_area)
    bmp_values <- ifelse(is.na(urban_results_HUCmerge[urbanbmps[i]]), 0, urban_results_HUCmerge[urbanbmps[i]])
    
    urban_results_HUCmerge[colname] <- acre_area_values * as.numeric(bmp_values[[1]])
  }

  foreach(i = 1:length(urbanbmps)) %do% {
    attr(urban_results_HUCmerge[, urbanbmps[i]], 'dimnames') <- NULL
  } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes

  return(urban_results_HUCmerge)
}
calculate_urban_total <- function(urbanresults, urbanbmps) {
  
  urban_total <- urbanresults %>%
    dplyr::summarise(across(.cols = paste0(urbanbmps, "_areaac"), .fns = ~ sum(.x, na.rm = TRUE), .names = "{.col}"))
  
  urban_total[paste0(urbanbmps, "_areaac")] <- format(
    urban_total[paste0(urbanbmps, "_areaac")],
    big.mark = ',',
    scientific = FALSE,
    digits = 2,
    nsmall = 1
  )
  
  names(urban_total) <- gsub("_", " ", paste0(urbanbmps, "_areaac"))
  names(urban_total) <- gsub("areaac", "(ac)", names(urban_total))
  
  return(urban_total)
}


present_urban_results_HUC <- function(urbanresults, urbanbmps) {
  
  urban_results_HUC <- urbanresults %>%
    rename_with(
      ~ paste0(., "_FracImplement"), .cols = contains(urbanbmps) & !contains('areaac')
    ) %>%
    select(
      COMID,
      HUC_12,
      Land_Use,
      HSG,
      contains(urbanbmps)
    )
  
  return(urban_results_HUC)
}

merge_road_results <- function(roadresults, road_area, roadbmps, km2_to_ac) {
  # Merge the SPARROW and StreamCat data to the road BMP results
  # First try merging by both COMID and HSG
  road_results_HUCmerge <- merge(roadresults,
                                 road_area,
                                 by = c("COMID", "HSG"),
                                 all.x = TRUE)
  
  # If merge failed (all HUC_12 values are NA), try merging by COMID only
  if(all(is.na(road_results_HUCmerge$HUC_12))) {
    # Try merging just by COMID
    road_area_simple <- road_area %>%
      group_by(COMID) %>%
      summarise(
        HUC_12 = first(HUC_12),
        road_area_ac = sum(road_area_ac, na.rm = TRUE),
        tot_area_ac = first(tot_area_ac),
        .groups = 'drop'
      )
    
    road_results_HUCmerge <- merge(roadresults,
                                   road_area_simple,
                                   by = "COMID",
                                   all.x = TRUE)
  }
  
  # Calculate BMP implementation area (acres) (note: this used to sum by HUC12 - stakeholders indicated that the ComID resolution would be more helpful)
  for (i in 1:length(roadbmps)) {
    road_results_HUCmerge[roadbmps[i]] <- sapply(
      sapply(road_results_HUCmerge[roadbmps[i]], as.character), 
      as.numeric
    )
    colname <- paste(roadbmps[i], "areaac", sep = "_")
    
    # Handle cases where road_area_ac might be NA
    road_area_values <- ifelse(is.na(road_results_HUCmerge$road_area_ac), 0, road_results_HUCmerge$road_area_ac)
    bmp_values <- ifelse(is.na(road_results_HUCmerge[roadbmps[i]]), 0, road_results_HUCmerge[roadbmps[i]])
    
    road_results_HUCmerge[colname] <- road_area_values * as.numeric(bmp_values[[1]])
  }
  
  foreach(i = 1:length(roadbmps)) %do% {
    attr(road_results_HUCmerge[, roadbmps[i]], 'dimnames') <- NULL
  } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
  
  return(road_results_HUCmerge)
}

calculate_road_total <- function(roadresults, roadbmps) {
  
  road_total <- roadresults %>%
    dplyr::summarise(across(.cols = paste0(roadbmps, "_areaac"), .fns = ~ sum(.x, na.rm = TRUE), .names = "{.col}"))
  
  road_total[paste0(roadbmps, "_areaac")] <- format(
    road_total[paste0(roadbmps, "_areaac")],
    big.mark = ',',
    scientific = FALSE,
    digits = 2,
    nsmall = 1
  )
  
  names(road_total) <- gsub("_", " ", paste0(roadbmps, "_areaac"))
  names(road_total) <- gsub("areaac", "(ac)", names(road_total))
  
  return(road_total)
}

present_road_results_HUC <- function(roadresults, roadbmps) {
  
  road_results_HUC <- roadresults %>%
    rename_with(
      ~ paste0(., "_FracImplement"), .cols = contains(roadbmps) & !contains('areaac')
    ) %>%
    select(
      COMID,
      HUC_12,
      HSG,
      contains(roadbmps)
    )
  
  return(road_results_HUC)
}

merge_ag_results <- function(agresults, areadf, agbmps, km2_to_ac) {
  
  # Handle case where no agriculture BMPs are present
  if(length(agbmps) == 0 || nrow(agresults) == 0) {
    # Return merged area data only
    ag_results_HUCmerge <- merge(
      agresults, areadf, by = "COMID", all.x = TRUE
    )
    return(ag_results_HUCmerge)
  }
  
  # Merge the COMID to HUC12 crosswalk to the ag BMP results
  ag_results_HUCmerge <- merge(
    agresults, areadf, by = "COMID", all.x = TRUE
  )
  # Calculate BMP implementation area (acres) by HUC12
  for (i in 1:length(agbmps)) {
    ag_results_HUCmerge[agbmps[i]] <- sapply(
      sapply(
        ag_results_HUCmerge[agbmps[i]], as.character
      ),
      as.numeric
    )
    colname <- paste(agbmps[i], "areaac", sep = "_")
    ag_results_HUCmerge[colname] <- ag_results_HUCmerge$ag_km2 *
      ag_results_HUCmerge[agbmps[i]] /
      km2_to_ac
    
  }
  
  foreach(i = 1:length(agbmps)) %do% {
    attr(ag_results_HUCmerge[, agbmps[i]], 'dimnames') <- NULL
  } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
  
  return(ag_results_HUCmerge)
}


present_ag_results_HUC <- function(agresults, agbmps) {
  
  # Handle case where no agriculture BMPs are present
  if(length(agbmps) == 0 || nrow(agresults) == 0) {
    # Return empty dataframe with expected columns
    ag_results_HUC <- data.frame(
      COMID = character(0),
      HUC_12 = character(0),
      IncAreaKm2 = numeric(0),
      ag_km2 = numeric(0),
      PctCrop2019Cat = numeric(0),
      StateAbbrev = character(0),
      stringsAsFactors = FALSE
    )
    return(ag_results_HUC)
  }
  
  ag_results_HUC <- data.frame(agresults) %>%
    rename_with(
      ~ paste0(., "_FracImplement"), any_of(agbmps)
    ) %>%
    select(
      COMID,
      HUC_12,
      IncAreaKm2,
      ag_km2,
      PctCrop2019Cat,
      StateAbbrev,
      contains(agbmps)
    )
  
  return(ag_results_HUC)
}

calculate_ag_total <- function(agresults, agbmps) {
  
  # Handle case where no agriculture BMPs are present
  if(length(agbmps) == 0 || nrow(agresults) == 0) {
    # Return empty dataframe
    ag_total <- data.frame()
    return(ag_total)
  }
  
  ag_total <- data.frame(
    lapply(agresults[paste0(agbmps, "_areaac")], function(x) sum(x, na.rm = TRUE))
  )
  ag_total[paste0(agbmps, "_areaac")] <- format(
    ag_total[paste0(agbmps, "_areaac")],
    big.mark = ',',
    scientific = FALSE,
    digits = 2,
    nsmall = 1
  )
  names(ag_total) <-gsub("_", " ", paste0(agbmps, "_areaac"))
  names(ag_total) <- gsub("areaac", "(ac)", names(ag_total))
  
  return(ag_total)
}


merge_ripbuf_results <- function(
  ripbufbmps, ripbufresults, areadf, sparrowinputs
) {
  if(length(ripbufbmps) > 0) {

    req(
      "comid" %in% names(sparrowinputs),
      "COMID" %in% names(ripbufresults)
    )
    
    ripbuf_results_HUCmerge <- merge(
      merge(
        ripbufresults,
        sparrowinputs %>% rename(COMID = comid, HUC_12 = huc12),
        by = "COMID", all.x = TRUE
      ),
      areadf,
      by = c("COMID", "HUC_12", "IncAreaKm2"),
      all.x = TRUE
    )
    
    # Calculate BMP implementation area (acres) by HUC12
    for (i in 1:length(ripbufbmps)) {
      ripbuf_results_HUCmerge[ripbufbmps[i]] <- sapply(
        sapply(
          ripbuf_results_HUCmerge[ripbufbmps[i]], as.character
        ),
        as.numeric
      )
      colname <- paste(ripbufbmps[i], "banklengthft", sep = "_")
      ripbuf_results_HUCmerge[colname] <- ripbuf_results_HUCmerge[
        ripbufbmps[i]
      ]
      
    }
    
    foreach(i = 1:length(ripbufbmps)) %do% {
      attr(ripbuf_results_HUCmerge[, ripbufbmps[i]], 'dimnames') <- NULL
    } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
    
    ftperkm <- 3280.84
    
    ripbuf_results_HUCmerge <- ripbuf_results_HUCmerge %>%
      mutate(totalbanklength_ft = length * 2 * ftperkm) %>%
      select(
        COMID,
        HUC_12,
        IncAreaKm2,
        StateAbbrev,
        totalbanklength_ft,
        all_of(ripbufbmps),
        contains("_banklengthft")
      ) %>%
      arrange(COMID)
  } else {ripbuf_results_HUCmerge <- NULL}
  
  return(ripbuf_results_HUCmerge)
}


present_ripbuf_results_HUC <- function(ripbufresults, ripbufbmps) {
  
  ripbuf_results_HUC <- data.frame(ripbufresults) %>%
    mutate(riverreachlength_ft = totalbanklength_ft / 2) %>%
    rename_with(
      ~ paste0(., "_LengthImplemented_ft"), all_of(ripbufbmps)
    )
  
  return(ripbuf_results_HUC)
}


calculate_ripbuf_total <- function(ripbufresults, ripbufbmps) {
  ripbuf_total <- data.frame(
    lapply(ripbufresults[paste0(ripbufbmps, "_banklengthft")], sum)
  )
  ripbuf_total[paste0(ripbufbmps, "_banklengthft")] <- format(
    ripbuf_total[paste0(ripbufbmps, "_banklengthft")],
    big.mark = ',',
    scientific = FALSE,
    digits = 2,
    nsmall = 1
  )
  names(ripbuf_total) <-gsub("_", " ", paste0(ripbufbmps, "_banklengthft"))
  names(ripbuf_total) <- gsub(
    "banklengthft", "(ft of bank)", names(ripbuf_total)
  )
  
  return(ripbuf_total)
}


parse_point_results_wwtp <- function(pointresults, pointcomid) {
  
  point_results_wwtp <- merge(
    pointresults, pointcomid, by = "COMID"
  )
  
  return(point_results_wwtp)
}


get_point_plants <- function(pointresults, pointbmps) {
  
  pointresults_slim <- pointresults %>%
    select(-COMID, -State, -NPDES_ID) %>%
    mutate(across(any_of(pointbmps), ~as.numeric(.))) 
  
  nbmpmax <- max(str_count(pointbmps, "\\_")) + 1
  
  point_plants <- foreach(i = 1:nrow(pointresults_slim), .combine = "rbind") %do% {
    pointresults_slim %>% 
      slice(i) %>%
      pivot_longer(
        cols = any_of(pointbmps), names_to = "BMP", values_to = "Implemented"
      ) %>%
      separate(
        BMP, sep = "_", into = paste0("BMP", seq_len(nbmpmax)), fill = "right"
      ) %>%
      pivot_longer(
        cols = contains("BMP"), names_to = "BMPnumber", values_to = "BMP"
      ) 
  } %>%
    filter(!is.na(BMP)) %>%
    select(-BMPnumber) %>%
    unique() %>%
    mutate(
      BMP = case_when(
        BMP == "FOURBDP" ~ "Fourstage_Bardenpho",
        BMP == "C" ~ "Chem_add",
        BMP == "DNF" ~ "Denitrification_filters",
        BMP == "F" ~ "Tertiary_filters",
        BMP == "M" ~ "MeOH_addition",
        BMP == "MBR" ~ "Membrane_bioreactors",
        BMP == "MLE" ~ "Modified_Ludzack_Ettinger",
        BMP == "SBR" ~ "Sequencing_batch_reactor"
      )
    )%>%
    group_by(Plant_Name, BMP) %>%
    summarize(Implemented = max(Implemented), .groups = "drop") %>%
    mutate(
      Implemented = case_when(
        Implemented == 1 ~ Plant_Name, TRUE ~ NA_character_)
    ) %>%
    pivot_wider(
      names_from = "BMP", values_from = "Implemented"
    ) %>%
    select(-Plant_Name)
  
  return(point_plants)
}

get_point_HUC <- function(pointresults, pointbmps) {

  pointresults_slim <- pointresults %>%
    mutate(across(any_of(pointbmps), ~as.numeric(.))) 
  
  nbmpmax <- max(str_count(pointbmps, "\\_")) + 1
  
  point_plants <- foreach(i = 1:nrow(pointresults_slim), .combine = "rbind") %do% {
    pointresults_slim %>% 
      slice(i) %>%
      pivot_longer(
        cols = any_of(pointbmps), names_to = "BMP", values_to = "Implemented"
      ) %>%
      separate(
        BMP, sep = "_", into = paste0("BMP", seq_len(nbmpmax)), fill = "right"
      ) %>%
      pivot_longer(
        cols = contains("BMP"), names_to = "BMPnumber", values_to = "BMP"
      ) 
  } %>%
    filter(!is.na(BMP)) %>%
    select(-BMPnumber) %>%
    unique() %>%
    mutate(
      BMP = case_when(
        BMP == "FOURBDP" ~ "Fourstage_Bardenpho",
        BMP == "C" ~ "Chem_add",
        BMP == "DNF" ~ "Denitrification_filters",
        BMP == "F" ~ "Tertiary_filters",
        BMP == "M" ~ "MeOH_addition",
        BMP == "MBR" ~ "Membrane_bioreactors",
        BMP == "MLE" ~ "Modified_Ludzack_Ettinger",
        BMP == "SBR" ~ "Sequencing_batch_reactor"
      )
    )%>%
    group_by(COMID, NPDES_ID, State, Plant_Name, BMP) %>%
    summarize(Implemented = max(Implemented), .groups = "drop") %>%
    pivot_wider(
      names_from = "BMP", values_from = "Implemented"
    )
  
  return(point_plants)
}

merge_graz_results <- function(grazresults, areadf, grazbmps, km2_to_ac) {
  # Merge the COMID to HUC12 crosswalk to the ag BMP results
  graz_results_HUCmerge <- merge(
    grazresults, areadf, by = "COMID", all.x = TRUE
  )
  # Calculate BMP implementation area (acres) by HUC12
  for (i in 1:length(grazbmps)) {
    graz_results_HUCmerge[grazbmps[i]] <- sapply(
      sapply(
        graz_results_HUCmerge[grazbmps[i]], as.character
      ),
      as.numeric
    )
    colname <- paste(grazbmps[i], "areaac", sep = "_")
    graz_results_HUCmerge[colname] <- graz_results_HUCmerge$graz_km2 *
      graz_results_HUCmerge[grazbmps[i]] /
      km2_to_ac
    
  }
  
  foreach(i = 1:length(grazbmps)) %do% {
    attr(graz_results_HUCmerge[, grazbmps[i]], 'dimnames') <- NULL
  } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
  
  return(graz_results_HUCmerge)
}


present_graz_results_HUC <- function(grazresults, grazbmps) {
  
  # Handle case where no grazing BMPs are present
  if(length(grazbmps) == 0 || nrow(grazresults) == 0) {
    # Return empty dataframe with expected columns
    graz_results_HUC <- data.frame(
      COMID = character(0),
      HUC_12 = character(0),
      IncAreaKm2 = numeric(0),
      graz_km2 = numeric(0),
      PctCrop2019Cat = numeric(0),
      StateAbbrev = character(0),
      stringsAsFactors = FALSE
    )
    return(graz_results_HUC)
  }
  
  graz_results_HUC <- data.frame(grazresults) %>%
    rename_with(
      ~ paste0(., "_FracImplement"), any_of(grazbmps)
    ) %>%
    select(
      COMID,
      HUC_12,
      IncAreaKm2,
      graz_km2,
      PctCrop2019Cat,
      StateAbbrev,
      contains(grazbmps)
    )
  
  return(graz_results_HUC)
}

calculate_graz_total <- function(grazresults, grazbmps) {
  
  graz_total <- data.frame(
    lapply(grazresults[paste0(grazbmps, "_areaac")], sum)
  )
  graz_total[paste0(grazbmps, "_areaac")] <- format(
    graz_total[paste0(grazbmps, "_areaac")],
    big.mark = ',',
    scientific = FALSE,
    digits = 2,
    nsmall = 1
  )
  names(graz_total) <-gsub("_", " ", paste0(grazbmps, "_areaac"))
  names(graz_total) <- gsub("areaac", "(ac)", names(graz_total))
  
  return(graz_total)
}


##################################################
# 4. Render Preview UI ####
##################################################

make_streamcat_previews <- function(files, output) {
  iwalk(files, ~{
    output_name <- paste0("Preview_", .y)
    output[[output_name]] <- renderTable(.x)
  }
  )
}

render_streamcat_previews <- function(tables) {
  tabls_list <- imap(
    tables, 
    ~{
      tagList(
        h4(paste(str_sub(.y, 1, 2), "StreamCat File:")), 
        tableOutput(outputId = paste0("Preview_", .y))
      )
    }
  )
  return(tabls_list)
}

render_preview_ui <- function(file, tableobject, tabletitle, output) {
  output_name <- tableobject
  output[[output_name]] <- renderTable(head(x = file))
  tagList(
    h4(paste(tabletitle, ":")), 
    tableOutput(tableobject)
  )
}

#@ SE: made edits to display road and urban previews on 5-22-24
make_postprocessor_previews <- function() {
  return(
    mainPanel(
      titlePanel("File Previews"),
      uiOutput(outputId = "preview_NEOS_results_ui"),
      uiOutput(outputId = "preview_user_loading_ui"),
      uiOutput(outputId = "preview_point_comid_ui"),
      uiOutput(outputId = "preview_sparrow_in_ui"),
      uiOutput(outputId = "preview_urban_in_ui"),
      uiOutput(outputId = "preview_road_in_ui"),
      uiOutput(outputId = "streamcat_preview_ui")
    )
  )
}


#################################################
# 5. Write Report Outputs ####
#################################################

print_costdisplay <- function(totalcost) {
  # Handle case where totalcost is NULL (optimization failed)
  if(is.null(totalcost) || length(totalcost) == 0) {
    return("Cost information not available (optimization may have failed).")
  }
  
  return(
    paste0(
      "The total cost to reduce loads to the limit you provided is ",
      sprintf(
        "$%s",
        formatC(totalcost, format = "f", big.mark = ",", digits = 2)
      ),
      "."
    )
  )
}

render_costdisplay_uncertaintyplot <- function(costbootstraps) {
  return(
    ggplot(costbootstraps, aes(x = Cost)) +
      geom_density(aes(y = ..scaled..), fill = "darkgreen", n = 32) +
      scale_x_continuous(
        name = "Total Annualized Cost (x $1,000,000)",
        labels = function(x) {
          sprintf(
            "$%s",
            formatC(x / 1000000, format = "f", big.mark = ",", digits = 2)
          )
        },
        n.breaks = 4,
        guide = guide_axis(n.dodge = 2)
      )+
      scale_y_continuous(
        name = "Probability Density",
        labels = function(x) {paste0(signif(x, 2))}
      ) +
      theme_classic(base_size = 12)
  )
}

render_loadingdisplay_uncertainty <- function(loadinguncertaintymessage) {
  return(
    loadinguncertaintymessage %>%
      ungroup() %>%
      mutate(success.display = paste0(prob.met * 100, "%")) %>%
      select(
        "Loading Target" = Target.name,
        "Your Specified Target Load" = Limit.display,
        "95% CI of Estimated Annual Total Loading" = LoadingUncertaintyMessage,
        "Likelihood of Meeting Your Loading Target" = success.display
      )
  )
}

render_loadingdisplay_uncertaintyplot <- function(
    loadingbootstraps, loadingtargets_munged, loadingtargets
) {
  
  plotdat <- loadingbootstraps %>%
    left_join(
      ., loadingtargets_munged, by = c("Nutrient", "Number")
    ) %>%
    mutate(
      Target.name = case_when(
        Nutrient == "N" ~ paste0(
          "Total Nitrogen Loading \n to ",
          watershed_name,
          " \n (kg N / yr)"
        ),
        Nutrient == "P" ~ paste0(
          "Total Phosphorus Loading \n to ",
          watershed_name,
          " \n (kg P / yr)"
        )
      )
    )
  targetdat <- loadingtargets %>%
    left_join(
      ., loadingtargets_munged, by = c("Nutrient", "Number")
    ) %>%
    mutate(
      Target.name = case_when(
        Nutrient == "N" ~ paste0(
          "Total Nitrogen Loading \n to ",
          watershed_name,
          " \n (kg N / yr)"
        ),
        Nutrient == "P" ~ paste0(
          "Total Phosphorus Loading \n to ",
          watershed_name,
          " \n (kg P / yr)"
        )
      )
    )
  
  nplots <- length(unique(plotdat$Target.name))
  
  tmp.plot <- ggplot(
    plotdat,
    aes(x = Load, fill = Nutrient)
  ) +
    geom_density(aes(y = ..scaled..), show.legend = FALSE, n = 32) +
    scale_x_continuous(
      name = NULL,
      labels = function(x) {
        formatC(signif(x, 2), format = "f", big.mark = ",", digits = 0)
      },
      n.breaks = 4,
      guide = guide_axis(n.dodge = 2)
    ) +
    scale_y_continuous(
      name = "Probability Density",
      labels = function(x) {paste0(signif(x, 2))}
    ) +
    facet_wrap(
      "Target.name",
      scales = "free",
      strip.position = "bottom",
      ncol = 1
    ) +
    geom_vline(
      data = targetdat,
      mapping = aes(xintercept = Limit),
      lwd = 1,
      lty = "dotted",
      color = "grey80"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme(
      strip.background = element_rect(
        fill = "white", colour = NULL, size = rel(2)
      ),
      strip.placement = "outside",
      text = element_text(size = 12),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(
        fill = "white", color = NULL, size = rel(2)
      ),
      axis.line = element_line(colour = "black", size = rel(1)),
      legend.key = element_blank(),
      aspect.ratio = 1 / nplots
    )
  
  return(tmp.plot)
}

render_loadingsummarymessage <- function(loadinguncertaintymessage) {
  return(
    value = paste0(
      "This suggested plan has an estimated minimum of ",
      signif(min(loadinguncertaintymessage$prob.met * 100), 2),
      "% likelihood of meeting each of the specified loading targets.
        Likelihoods of meeting each specified loading target individually are
        listed in the table below. 'Likelihood' refers to the percentage of the probability
        density of estimated total annual load that falls below the user
        specified loading target."))
}

##################################################
# 6. Render Report UI ###########################
##################################################

# Display results ####
render_results_display <- function(loadingtargets_pulled) {
  mainPanel(
    titlePanel("Scaled-up Optimization Results"),
    em("To save this report, open the RBEROST postprocessor in a web browser and use the print to pdf functionality."),
    wellPanel(
      h4("User Notes:"),
      htmlOutput("usernotes")
    ),
    conditionalPanel(
      condition = "output.solvedstatus == 0",
      {
        fluidRow(
          wellPanel(h3("Your model submission returned an error.")),
          p(
            paste(
              "RBEROST may fail to optimize models for a variety of reasons. 
              Sometimes this may be a result of user error during specification of user inputs (e.g. in the case of nonsensical limits on BMPs) or during NEOS submission (e.g. uploading incorrect files).
              Other times, this is a result of loading targets that are too restrictive, or a result of not including enough BMPs for the model to use. 
              It is also possible to receive an error for reasons other than those listed above.
              Please check your user inputs.
              If problems persist a user might try running RBEROST again with less restrictive loading targets and/or including more BMPs in the optimization."
            ),
            h4("The model produced the following errors:"),
            code(verbatimTextOutput("errordisplay"))
          )
        )
      }
    ),
    conditionalPanel(
      condition = "output.solvedstatus == 2",
      {
        fluidRow(
          wellPanel(h3("Your model was unable to solve - Infeasible Problem")),
          conditionalPanel(
            condition = "output.presolveissue",
            {
              wellPanel(
                h4("Presolve Infeasibility Detected", style = "color: #d9534f;"),
                p(
                  "The optimizer detected that constraints cannot be satisfied during presolve analysis.",
                  "This means the problem is infeasible before optimization even begins.",
                  style = "font-weight: bold;"
                ),
                code(verbatimTextOutput("presolvedisplay"))
              )
            }
          ),
          p(
            paste(
              "RBEROST may fail to optimize models for a variety of reasons. 
              Often, this is a result of loading targets that are too restrictive, or a result of not including enough BMPs for the model to use. 
              It may be informative to run RBEROST again with less restrictive loading targets and/or including more BMPs in the optimization.
              Please check your user inputs."
            )
          )
        )
      }
    ),
    conditionalPanel(
      condition = "output.solvedstatus == 1",
      {
        fluidRow(
          wellPanel(
            h3("Your model has successfully solved."),
            textOutput("costdisplay"),
          ),
          conditionalPanel(
            condition = "output.costuncertaintyresultsavail",
            wellPanel(
              textOutput("costdisplay_uncertainty"),
              p(
                "Below is the probability density distribution of cost 
                 estimates for this scenario."
              ),
              plotOutput("costdisplay_uncertaintyplot")
            )
          ),
          conditionalPanel(
            condition = "output.loadinguncertaintyresultsavail",
            wellPanel(
              textOutput("loadingsummarymessage"),
              tableOutput("loadingdisplay_uncertainty"),
              p(
                "Below are the probability density distributions of 
                total annual load estimates at each of the loading 
                targets specified by the user. The vertical dotted grey
                lines denote the specified target loads at each loading 
                target. Probability densities to the left of the grey 
                line are load estimates that meet the loading target, 
                and probability densities to the right of the grey line are load 
                estimates that exceed the loading target."
              ),
              plotOutput(
                "loadingdisplay_uncertaintyplot",
                width = "100%",
                height = "1500px" # paste0(400*nrow(loadingtargets_pulled),"px"
                )
              )
            ),
          conditionalPanel(
            condition = "output.pointresultsavail",
            fluidRow(
              p(
                "The model chose to implement WWTP retrofits at the following locations:"
              ),
              tableOutput("wwtp"),
                            downloadLink(
                "downloadpointdat", "Download WWTPs BMPs by COMID and NPDES permit number"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.pointresultsavail",
            fluidRow(
              p( "The model did not implement any WWTP retrofits.")
            )
          ),
          tags$hr(),
          conditionalPanel(
            condition = "output.agdownloadavail",
            fluidRow(
              p(
                "The model chose to implement the total area of the following rowcrop BMPs:"
              ),
              tableOutput("ag"),
              downloadLink(
                "downloadagdat", "Download Agricultural (Rowcrop) BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.agdownloadavail",
            p("The model did not implement any agricultural rowcrop BMPs.")
          ),
          tags$hr(),
          conditionalPanel(
            condition = "output.grazdownloadavail",
            fluidRow(
              p(
                "The model chose to implement the total area of the following grazing BMPs:"
              ),
              tableOutput("graz"),
              downloadLink(
                "downloadgrazdat", "Download Grazing BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.grazdownloadavail",
            p("The model did not implement any grazing BMPs.")
          ),
          tags$hr(),
          conditionalPanel(
            condition = "output.urbandownloadavail",
            fluidRow(
              p(
                "The model chose to implement the total area of the following urban BMPs."
              ),
              tableOutput("urban"),
              downloadLink(
                "downloadurbandat", "Download Urban BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.urbandownloadavail",
            p("The model did not implement any urban BMPs.")
          ),
          tags$hr(),
          conditionalPanel(
            condition = "output.roaddownloadavail",
            fluidRow(
              p(
                "The model chose to implement the total area of the following road BMPs."
              ),
              tableOutput("road"),
              downloadLink(
                "downloadroaddat", "Download road BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.roaddownloadavail",
            p("The model did not implement any road BMPs.")
          ),
          tags$hr(),
          conditionalPanel(
            condition = "output.ripbufdownloadavail",
            fluidRow(
              p(
                "The model chose to implement the total length of the following riparian buffer BMPs."
              ),
              tableOutput("ripbuf"),
              downloadLink(
                "downloadripbufdat", "Download Riparian Buffer BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.ripbufdownloadavail",
            p("The model did not implement any riparian buffer BMPs.")
          ),

#### CTC edited: 2/9/2026 - transferred from dynamic LIS RBEROST #### 

          tags$hr(),
          conditionalPanel(
            condition = "output.septicdownloadavail",
            fluidRow(
              p(
                paste("The model chose to implement the total number of",
                      "parcels for the following septic BMPs.")
              ),
              tableOutput("septic"),
              downloadLink(
                "downloadsepticdat", "Download Septic BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.septicdownloadavail",
            p("The model did not implement any septic BMPs.")
          )

#### end edits ####
        )
      }
    )
  )
}

