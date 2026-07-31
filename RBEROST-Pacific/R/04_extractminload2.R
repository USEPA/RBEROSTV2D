# 04_extractminload2.R
# Extract minimum load from NEOS results files in subdirectories within Batch Results folder
# N. Detenbeck (modified from Copilot responses)
# To use on your results, update the following variables: base_dir, search_dir, search_date, output_file
# base_dir - working directory (should end with /RBEROST-Pacific/Preprocessing/Outputs/Batch Results)
# output_file - name for saving summary file
# search_dir - directory in which user has stored NEOS result .txt files
# search_date - date string within NEOS result file name

# 6/17/26
# Last updated to make more generic 7/31/26

library(R.utils)
library(stringr)

# Base directory to search
base_dir <- "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Pacific/Preprocessing/Outputs/Batch Results"

# Directory with result files to be summarized
search_dir <- "/Season4notrib_071926"

# Date string in results file name
search_date <- "_2026-07-21"

# Output file to save results
output_file <- paste0(base_dir,"/Season4notrib_072126.csv")

# Open output file for writing
writeLines("Wshdname,MinLoad\n", output_file)

# Recursively search for matching files
  matches <- findFiles(
    pattern = "NEOSresult_",         
    paths   = paste0(base_dir,search_dir),
    recursive = FALSE,              # search subdirectories
    firstOnly = FALSE,             # return all matches
    allFiles = TRUE                # include hidden files
  )
  
  # Filter by date pattern
  matches <- matches[grep(search_date, basename(matches), ignore.case = FALSE)]
  
  for (file in matches) {
    # Read file content
    content <- readLines(file, warn = FALSE)
    
    # Extract number after "CPLEX 22.1.2: optimal solution objective "
    obj_line <- grep("CPLEX 22.1.2: optimal solution; objective", content, ignore.case = TRUE)
    if (length(obj_line) > 0) {
      wshdname <- str_match(basename(file), paste0("NEOSresult_\\s*(.*?)\\s*",search_date,".txt"))[,2]
      wshdname <- gsub("_", " ", wshdname)
      obj_text <- content[as.numeric(obj_line)]
      num_match <- sub('.*objective ', '', obj_text)
      # Extract number using regex
      # num_match <- regmatches(obj_text, regexpr("[-+]?[0-9]+(?:[.][0-9]+)?", obj_text))
      if (length(num_match) > 0) {
        num <- as.numeric(num_match)
        # Write to output file
        write(paste(wshdname,",", num), output_file, append = TRUE)
      }
    }
  }


cat("Processing complete. Results saved to", output_file, "\n")
