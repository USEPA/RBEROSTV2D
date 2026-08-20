################################################################################
# 04_ReplaceResultTableHeaderAbbreviations_single.R
# This program processes an RBEROST result .txt file to replace abbreviations
# for table headings automatically substituted by AMPL to make column headings
# fit within a page width (80 characters) with the original
# header names then deletes the rows with header definitions.
# This allows the tables to be read in correctly by the postprocessor.
# This should be run once to correct the urban_frac_by_comid_bmp table and once to 
# correct the road_frac_by_comid_bmp table. 
# This should be run after results file is created by AMPL and before RBEROST
# postprocessor is run.
# 7/15/2026
# N. Detenbeck
################################################################################
batchpath <- "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/RBEROSTV2D/RBEROST-Pacific/Preprocessing/Outputs/"
watershed <- ""
resultfile <- "NEOSresult_LowerStillaguamishR_2026-08-19_nounc"
infile1 <- paste0(batchpath,watershed,resultfile,".txt")
outfile1 <- paste0(batchpath,watershed,resultfile,"_corr.txt")
infile2 <- outfile1
outfile2 <- paste0(batchpath,watershed,resultfile,"_corr2.txt")

process_file1 <- function(infile, outfile = infile) {
  x <- readLines(infile, warn = FALSE)
  
  # Find the target line
  i <- grep("^urban_frac_by_comid_bmp \\[\\*,\\*\\]", x)
  if (length(i) == 0) stop("Target line not found.")
  i <- i[1]  # use first match
  
  # Collect abbreviation definition lines immediately after target line
  defs_idx <- integer(0)
  j <- i + 1
  
  while (j <= length(x) && grepl("^#\\s+\\$\\d+\\s*=", x[j])) {
    defs_idx <- c(defs_idx, j)
    j <- j + 1
  }
  
  defs <- x[defs_idx]
  abbr_map <- setNames(
    sub("^#\\s+(\\$\\d+)\\s*=\\s*", "", defs),
    sub("^#\\s+(\\$\\d+)\\s*=\\s*.*$", "\\1", defs)
  )
  
  # Header line is assumed to be the first non-comment line after definitions
  if (j > length(x)) stop("Header line not found.")
  
  header <- x[j]
  
  # Replace only abbreviations, preserving all original spacing
  for (abbr in names(abbr_map)) {
    header <- gsub(
      paste0("(?<!\\S)", gsub("([$\\\\])", "\\\\\\1", abbr), "(?!\\S)"),
      abbr_map[[abbr]],
      header,
      perl = TRUE
    )
  }
  
  # Remove only the definition lines in this matched section
  # Keep all other lines unchanged
  keep <- rep(TRUE, length(x))
  keep[defs_idx] <- FALSE
  x_out <- x[keep]
  
  # Adjust index of header line after removal of defs
  shift <- sum(defs_idx < j)
  j_new <- j - shift
  x_out[j_new] <- header
  
  # Write back to disk
  writeLines(x_out, outfile, sep = "\n", useBytes = TRUE)
  
  invisible(outfile)
}

process_file1(infile1,outfile1)

process_file2 <- function(infile, outfile = infile) {
  x <- readLines(infile, warn = FALSE)
  
  # Find the target line
  i <- grep("^road_frac_by_comid_bmp \\[\\*,\\*\\]", x)
  if (length(i) == 0) stop("Target line not found.")
  i <- i[1]  # use first match
  
  # Collect abbreviation definition lines immediately after target line
  defs_idx <- integer(0)
  j <- i + 1
  
  while (j <= length(x) && grepl("^#\\s+\\$\\d+\\s*=", x[j])) {
    defs_idx <- c(defs_idx, j)
    j <- j + 1
  }
  
  defs <- x[defs_idx]
  abbr_map <- setNames(
    sub("^#\\s+(\\$\\d+)\\s*=\\s*", "", defs),
    sub("^#\\s+(\\$\\d+)\\s*=\\s*.*$", "\\1", defs)
  )
  
  # Header line is assumed to be the first non-comment line after definitions
  if (j > length(x)) stop("Header line not found.")
  
  header <- x[j]
  
  # Replace only abbreviations, preserving all original spacing
  for (abbr in names(abbr_map)) {
    header <- gsub(
      paste0("(?<!\\S)", gsub("([$\\\\])", "\\\\\\1", abbr), "(?!\\S)"),
      abbr_map[[abbr]],
      header,
      perl = TRUE
    )
  }
  
  # Remove only the definition lines in this matched section
  # Keep all other lines unchanged
  keep <- rep(TRUE, length(x))
  keep[defs_idx] <- FALSE
  x_out <- x[keep]
  
  # Adjust index of header line after removal of defs
  shift <- sum(defs_idx < j)
  j_new <- j - shift
  x_out[j_new] <- header
  
  # Write back to disk
  writeLines(x_out, outfile, sep = "\n", useBytes = TRUE)
  
  invisible(outfile)
}

process_file2(infile2,outfile2)
