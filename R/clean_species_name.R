#' Standardize Tree Species Names
#'
#' Converts various field codes and abbreviations for tree species into
#' standardized common names following consistent nomenclature.
#'
#' @param df A data frame containing species information
#' @param species_col Name of the column containing species codes/names (unquoted)
#' @param keep_original Logical. Should the original species column be retained?
#'   If TRUE, creates a new column with suffix "_original". Default is FALSE.
#' @param verbose Logical. Should the function print a summary of changes?
#'   Default is TRUE.
#'
#' @return A data frame with standardized species names
#'
#' @details
#' This function standardizes tree species names commonly found in FBAT (Fire
#' and Burn Area Treatment) plot data. It performs case-insensitive matching
#' and handles multiple variations of species codes.
#'
#' Species Standardization Table:
#'
#' \tabular{ll}{
#'   \strong{Standard Name}  \tab \strong{Accepted Codes} \cr
#'   California Black Oak    \tab CBO, Cal BO, Cal. B.O., C.b.o, QC, Quercus \cr
#'   Incense Cedar           \tab IC, I.c, CAL IC, ICc \cr
#'   Jeffrey Pine            \tab JP, J.p, JFP, Jfp \cr
#'   Sugar Pine              \tab Sugar Pine, S.p \cr
#'   White Fir               \tab SWF, WFIR, Wfir, S.w.f, W \cr
#'   Douglas-fir             \tab DF, Dfir \cr
#'   Ponderosa Pine          \tab PP, Pp, P.p \cr
#'   Lodgepole Pine          \tab LPP, L.p.p, Lp, LGP \cr
#'   Red Fir                 \tab RF, Rfir \cr
#'   Gray Pine               \tab GP \cr
#'   Giant Sequoia           \tab GS \cr
#'   Juniper                 \tab Juniper \cr
#'   Cedar                   \tab Cedar \cr
#'   Unknown                 \tab UNKNOWN, NA, Undo, TBD, DW \cr
#' }
#'
#' @examples
#' example_df <- data.frame(
#'   species = c("CBO", "IC", "UNKNOWN", "J.P", "XCODE"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Basic usage
#' cleaned_data <- clean_species_names(example_df, species)
#'
#' # Keep original column for comparison
#' cleaned_data <- clean_species_names(example_df, species, keep_original = TRUE)
#'
#' # Suppress summary output
#' cleaned_data <- clean_species_names(example_df, species, verbose = FALSE)
#'
#' @importFrom stringr str_to_upper
#'
#' @export
clean_species_names <- function(df, 
                               species_col, 
                               keep_original = FALSE, 
                               verbose = TRUE) {
  
  # ============================================================================
  # 1. PACKAGE DEPENDENCIES
  # ============================================================================
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required.",
         call. = FALSE)
  }
  
  # ============================================================================
  # 2. INPUT VALIDATION
  # ============================================================================
  
  # Check if df is a data frame
  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data frame", call. = FALSE)
  }
  
  # Check if data frame is empty
  if (nrow(df) == 0) {
    warning("Input data frame has 0 rows", call. = FALSE)
    return(df)
  }
  
  # Get column name as string
  species_col_name <- deparse(substitute(species_col))
  
  # Check if column exists
  if (!species_col_name %in% names(df)) {
    stop(paste0("Column '", species_col_name, "' not found in data frame.\n",
                "Available columns: ", paste(names(df), collapse = ", ")),
         call. = FALSE)
  }
  
  # ============================================================================
  # 3. PRESERVE ORIGINAL DATA IF REQUESTED
  # ============================================================================
  
  if (keep_original) {
    original_col_name <- paste0(species_col_name, "_original")
    df[[original_col_name]] <- df[[species_col_name]]
    
    if (verbose) {
      message(sprintf("Original species data saved in column: %s", original_col_name))
    }
  }
  
  # Store original values for summary
  original_species <- df[[species_col_name]]
  
  # ============================================================================
  # 4. STANDARDIZE SPECIES NAMES
  # ============================================================================
  
  species_values <- df[[species_col_name]]
  species_upper <- stringr::str_to_upper(species_values)

  mapping <- c(
    "CBO" = "California Black Oak",
    "CAL BO" = "California Black Oak",
    "CAL. B.O." = "California Black Oak",
    "CAL B.O." = "California Black Oak",
    "C.B.O" = "California Black Oak",
    "QC" = "California Black Oak",
    "QUERCUS" = "California Black Oak",
    "CBO (CAL. B. OAK)" = "California Black Oak",
    "CA. LIVE. OAK" = "California Black Oak",
    "CLOAK" = "California Black Oak",
    "C.L.O" = "California Black Oak",
    "IC" = "Incense Cedar",
    "I.C" = "Incense Cedar",
    "CAL IC" = "Incense Cedar",
    "CAL. IC" = "Incense Cedar",
    "ICC" = "Incense Cedar",
    "CAL CEDOR" = "Incense Cedar",
    "JP" = "Jeffrey Pine",
    "J.P" = "Jeffrey Pine",
    "JFP" = "Jeffrey Pine",
    "JFP." = "Jeffrey Pine",
    "JFO" = "Jeffrey Pine",
    "JEFF" = "Jeffrey Pine",
    "SUGAR PINE" = "Sugar Pine",
    "S.P" = "Sugar Pine",
    "SP" = "Sugar Pine",
    "SUGARP" = "Sugar Pine",
    "SULF" = "Sugar Pine",
    "SWF" = "White Fir",
    "WFIR" = "White Fir",
    "W" = "White Fir",
    "S.W.F" = "White Fir",
    "WF" = "White Fir",
    "S.W.R" = "White Fir",
    "DF" = "Douglas-fir",
    "DFIR" = "Douglas-fir",
    "D.FIR" = "Douglas-fir",
    "PP" = "Ponderosa Pine",
    "P.P" = "Ponderosa Pine",
    "PONDEROSA PINE" = "Ponderosa Pine",
    "P.D" = "Ponderosa Pine",
    "LPP" = "Lodgepole Pine",
    "L.P.P" = "Lodgepole Pine",
    "LP" = "Lodgepole Pine",
    "LGP" = "Lodgepole Pine",
    "LODGEPOLE PINE" = "Lodgepole Pine",
    "RF" = "Red Fir",
    "RFIR" = "Red Fir",
    "R.FIR" = "Red Fir",
    "RED FIR" = "Red Fir",
    "GP" = "Gray Pine",
    "GRAY PINE" = "Gray Pine",
    "GS" = "Giant Sequoia",
    "GIANT SEQUOIA" = "Giant Sequoia",
    "GSEQ" = "Giant Sequoia",
    "JUNIPER" = "Juniper",
    "JU" = "Juniper",
    "JUNI" = "Juniper",
    "CEDAR" = "Cedar",
    "CED" = "Cedar",
    "UNKNOWN" = "Unknown",
    "UNKNOWN (P.P?)" = "Unknown",
    "NA" = "Unknown",
    "UNDO" = "Unknown",
    "TBD" = "Unknown",
    "DW" = "Unknown",
    "UNK" = "Unknown",
    "?" = "Unknown"
  )

  standardized_species <- mapping[species_upper]
  standardized_species <- ifelse(
    is.na(standardized_species),
    species_values,
    unname(standardized_species)
  )

  df[[species_col_name]] <- standardized_species
  
  # ============================================================================
  # 5. GENERATE SUMMARY STATISTICS
  # ============================================================================
  
  if (verbose) {
    
    # Count changes
    changed_indices <- which(original_species != df[[species_col_name]])
    n_changed <- length(changed_indices)
    pct_changed <- round(n_changed / nrow(df) * 100, 2)
    
    # Original unique species
    original_unique <- length(unique(original_species[!is.na(original_species)]))
    
    # Standardized unique species
    standardized_unique <- length(unique(df[[species_col_name]][!is.na(df[[species_col_name]])]))
    
    # Count unknowns
    n_unknown <- sum(df[[species_col_name]] == "Unknown", na.rm = TRUE)
    pct_unknown <- round(n_unknown / nrow(df) * 100, 2)
    
    # Show top species
    species_vals <- df[[species_col_name]]
    species_vals <- species_vals[!is.na(species_vals)]
    top_tab <- sort(table(species_vals), decreasing = TRUE)
    top_tab <- utils::head(top_tab, 10)
    top_species <- data.frame(
      species = names(top_tab),
      count = as.integer(top_tab),
      stringsAsFactors = FALSE
    )

    top_lines <- vapply(
      seq_len(nrow(top_species)),
      function(i) sprintf("  %d. %-25s %d",
                          i, top_species$species[i], top_species$count[i]),
      character(1)
    )

    message(paste(
      "",
      "=======================================================",
      "          SPECIES STANDARDIZATION SUMMARY",
      "=======================================================",
      "",
      sprintf("Total records:              %d", nrow(df)),
      sprintf("Records changed:            %d (%.2f%%)", n_changed, pct_changed),
      sprintf("Original unique species:    %d", original_unique),
      sprintf("Standardized species:       %d", standardized_unique),
      sprintf("Unknown/Missing:            %d (%.2f%%)", n_unknown, pct_unknown),
      "",
      "Top 10 Species (after standardization):",
      "-------------------------------------------------------",
      paste(top_lines, collapse = "\n"),
      "=======================================================",
      sep = "\n"
    ))
  }
  
  # ============================================================================
  # 6. RETURN CLEANED DATA
  # ============================================================================
  
  return(df)
}


#' Get Species Standardization Mapping Table
#'
#' Returns a reference table showing how field codes map to standardized names,
#' including scientific names.
#'
#' @return A data frame with columns: standard_name, field_codes, scientific_name
#'
#' @examples
#' # Get the mapping reference
#' mapping <- get_species_mapping()
#' print(mapping)
#'
#' @export
get_species_mapping <- function() {
  
  mapping <- data.frame(
    standard_name = c(
      "California Black Oak",
      "Incense Cedar",
      "Jeffrey Pine",
      "Sugar Pine",
      "White Fir",
      "Douglas-fir",
      "Ponderosa Pine",
      "Lodgepole Pine",
      "Red Fir",
      "Gray Pine",
      "Giant Sequoia",
      "Juniper",
      "Cedar",
      "Unknown"
    ),
    field_codes = c(
      "CBO, Cal BO, Cal. B.O., C.b.o, QC, Quercus, CBO (Cal. B. Oak)",
      "IC, I.c, CAL IC, CAL. IC, ICc",
      "JP, J.p, JFP, Jfp",
      "Sugar Pine, S.p",
      "SWF, WFIR, Wfir, S.w.f, W",
      "DF, Dfir",
      "PP, Pp, P.p",
      "LPP, L.p.p, Lp, LGP",
      "RF, Rfir",
      "GP",
      "GS",
      "Juniper",
      "Cedar",
      "UNKNOWN, NA, Undo, TBD, DW"
    ),
    scientific_name = c(
      "Quercus kelloggii",
      "Calocedrus decurrens",
      "Pinus jeffreyi",
      "Pinus lambertiana",
      "Abies concolor",
      "Pseudotsuga menziesii",
      "Pinus ponderosa",
      "Pinus contorta",
      "Abies magnifica",
      "Pinus sabiniana",
      "Sequoiadendron giganteum",
      "Juniperus spp.",
      "Cedrus spp.",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  
  return(mapping)
}


#' Check for Unmapped Species Codes
#'
#' Identifies species codes in your data that are not recognized by the
#' standardization function. Useful for quality control and identifying
#' new codes that need to be added.
#'
#' @param df A data frame containing species information
#' @param species_col Name of the column containing species codes (unquoted)
#'
#' @return A data frame of unrecognized codes with their frequencies
#'
#' @examples
#' example_df <- data.frame(
#'   species = c("CBO", "IC", "UNKNOWN", "XCODE"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Check for unmapped codes before standardizing
#' unmapped <- check_unmapped_species(example_df, species)
#'
#' # If codes are found, add them to the clean_species_names function
#'
#' @export
check_unmapped_species <- function(df, species_col) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required.", call. = FALSE)
  }
  
  # Get column name
  species_col_name <- deparse(substitute(species_col))
  
  # Define all known codes (uppercase)
  known_codes <- c(
    # California Black Oak
    "CBO", "CAL BO", "CAL. B.O.", "CAL B.O.", "C.B.O", "QC", "QUERCUS", 
    "CBO (CAL. B. OAK)",
    # Incense Cedar
    "IC", "I.C", "CAL IC", "CAL. IC", "ICC",
    # Jeffrey Pine
    "JP", "J.P", "JFP", "JFP.",
    # Sugar Pine
    "SUGAR PINE", "S.P", "SULF",
    # White Fir
    "SWF", "WFIR", "W", "S.W.F", "WF",
    # Douglas-fir
    "DF", "DFIR", "D.FIR",
    # Ponderosa Pine
    "PP", "P.P", "PONDEROSA PINE",
    # Lodgepole Pine
    "LPP", "L.P.P", "LP", "LGP", "LODGEPOLE PINE",
    # Red Fir
    "RF", "RFIR", "R.FIR", "RED FIR",
    # Gray Pine
    "GP", "GRAY PINE",
    # Giant Sequoia
    "GS", "GIANT SEQUOIA",
    # Juniper
    "JUNIPER", "JU", "JUNI",
    # Cedar
    "CEDAR", "CED",
    # Unknown
    "UNKNOWN", "UNKNOWN (P.P?)", "NA", "UNDO", "TBD", "DW", "UNK", "?"
  )
  
  # Find unmapped codes
  vals <- df[[species_col_name]]
  vals <- vals[!is.na(vals)]
  upper_vals <- stringr::str_to_upper(vals)
  unmapped_vals <- vals[!upper_vals %in% known_codes]

  if (length(unmapped_vals) > 0) {
    tab <- sort(table(unmapped_vals), decreasing = TRUE)
    unmapped <- data.frame(
      species = names(tab),
      frequency = as.integer(tab),
      stringsAsFactors = FALSE
    )
  } else {
    unmapped <- data.frame(species = character(0), frequency = integer(0), stringsAsFactors = FALSE)
  }
  
  # Print results
  if (nrow(unmapped) > 0) {
    message(
      "\nWARNING: Found unmapped species codes\n",
      "=======================================\n\n",
      paste(utils::capture.output(print(unmapped)), collapse = "\n"),
      "\n\nConsider adding these codes to clean_species_names() function."
    )
  } else {
    message("All species codes are recognized")
  }

  invisible(unmapped)
}


#' Compare Original and Standardized Species Names
#'
#' Creates a comparison table showing how species names were changed
#' during standardization.
#'
#' @param df A data frame with both original and standardized species columns
#' @param original_col Name of the original species column (unquoted)
#' @param standardized_col Name of the standardized species column (unquoted)
#'
#' @return A data frame showing unique mappings from original to standardized names
#'
#' @examples
#' example_df <- data.frame(
#'   species = c("CBO", "IC", "J.P", "UNKNOWN"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # First clean with keep_original = TRUE
#' cleaned <- clean_species_names(example_df, species, keep_original = TRUE)
#'
#' # Then compare
#' comparison <- compare_species_changes(cleaned, species_original, species)
#'
#' @export
compare_species_changes <- function(df, original_col, standardized_col) {
  original_name <- deparse(substitute(original_col))
  standardized_name <- deparse(substitute(standardized_col))

  comparison <- df[, c(original_name, standardized_name), drop = FALSE]
  comparison <- comparison[!is.na(comparison[[original_name]]), , drop = FALSE]
  comparison <- unique(comparison)
  comparison <- comparison[comparison[[original_name]] != comparison[[standardized_name]], , drop = FALSE]
  comparison <- comparison[order(comparison[[standardized_name]], comparison[[original_name]]), , drop = FALSE]
  
  if (nrow(comparison) > 0) {
    message(
      "\n=======================================================\n",
      "          SPECIES NAME CHANGES\n",
      "=======================================================\n\n",
      paste(utils::capture.output(print(comparison)), collapse = "\n")
    )
  } else {
    message("No species names were changed")
  }

  invisible(comparison)
}


# ============================================================================
# USAGE EXAMPLES
# ============================================================================

# # 1. Basic usage - standardize species names
# cleaned_data <- clean_species_names(fbat_data, species)
# 
# # 2. Keep original column for comparison
# cleaned_data <- clean_species_names(fbat_data, species, keep_original = TRUE)
# 
# # 3. Check for unmapped codes BEFORE standardizing
# unmapped <- check_unmapped_species(fbat_data, species)
# 
# # 4. Get the species mapping reference table
# mapping <- get_species_mapping()
# write.csv(mapping, "species_reference.csv", row.names = FALSE)
# 
# # 5. Compare changes after standardization
# cleaned <- clean_species_names(fbat_data, species, keep_original = TRUE)
# changes <- compare_species_changes(cleaned, species_original, species)
# 
# # 6. Silent mode (no summary output)
# cleaned_data <- clean_species_names(fbat_data, species, verbose = FALSE)