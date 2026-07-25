# =============================================================================
# Wall-to-wall vegetation for iLand by imputing FIA/field plots to pixels,
# then aggregating to stands -> iLand initialization file.
#
# Method: modified random-forest imputation (yaImpute), following
# Riley, Grenfell & Finney (2016). Each target pixel is assigned the
# best-matching reference plot; the plot's tree list is then inherited.
#
# Pipeline:
#   reference plots (predictors + responses + tree lists)
#        + target pixels (SAME predictors, from rasters)
#   --> impute_plots_to_pixels()  -> pixel : plot_id
#   --> build_iland_init()        -> stand-level init file
# =============================================================================

# Bare column names referenced inside dplyr/tidyr non-standard evaluation.
utils::globalVariables(c(
  "pixel_id", "stand_id", "cell", "stand", "neighbor", "species",
  "dbh_from", "dbh_to", "count", "hd", "count_sum", "n_pixels", "distance"
))

#' Impute Reference Plots to Target Pixels
#'
#' Assigns each target pixel its best-matching reference plot(s) using
#' random-forest nearest-neighbour imputation (\code{yaImpute::yai}). Imputation
#' is performed in chunks so that millions of pixels can be processed without
#' exhausting memory.
#'
#' @param reference Data frame of reference plots with a \code{plot_id} column,
#'   the predictor columns, and the response columns.
#' @param target Data frame of target pixels with \code{pixel_id},
#'   \code{stand_id}, and the same predictor columns as \code{reference}.
#' @param predictors Character vector of predictor column names, present in
#'   \emph{both} \code{reference} and \code{target} (the matching variables).
#' @param responses Character vector of response (forest-attribute) column
#'   names used to grow the random forest; required in \code{reference} only.
#' @param k Integer. Number of neighbours to assign (\code{1} = single
#'   best-matching plot). Default \code{1}.
#' @param ntree Integer. Number of trees in the random forest. Default
#'   \code{500}.
#' @param chunk_size Integer. Number of target pixels imputed per chunk.
#'   Default \code{1e5}.
#' @param seed Integer. Random seed for reproducibility. Default \code{42}.
#'
#' @return A long data frame with columns \code{pixel_id}, \code{stand_id},
#'   \code{neighbor} (1..\code{k}), \code{plot_id}, and \code{distance}.
#'
#' @examples
#' # imputation needs the Suggested packages 'yaImpute' and 'randomForest'
#' if (requireNamespace("yaImpute", quietly = TRUE) &&
#'     requireNamespace("randomForest", quietly = TRUE)) {
#'   set.seed(1)
#'   predictors <- c("elev", "slope", "tmean")
#'   responses  <- c("canopy_ht")
#'
#'   # 20 reference plots with predictors + a response
#'   reference <- data.frame(
#'     plot_id   = 1:20,
#'     elev      = runif(20, 500, 1500),
#'     slope     = runif(20, 0, 30),
#'     tmean     = runif(20, 5, 15),
#'     canopy_ht = runif(20, 10, 35))
#'
#'   # 30 target pixels with the SAME predictors
#'   target <- data.frame(
#'     pixel_id = 1:30,
#'     stand_id = rep(1:3, each = 10),
#'     elev     = runif(30, 500, 1500),
#'     slope    = runif(30, 0, 30),
#'     tmean    = runif(30, 5, 15))
#'
#'   pixel_plot <- impute_plots_to_pixels(reference, target,
#'                                        predictors, responses,
#'                                        k = 1, ntree = 100)
#'   head(pixel_plot)
#' }
#'
#' @importFrom dplyr %>% filter if_all all_of mutate left_join select bind_cols
#'   bind_rows
#' @importFrom tidyr pivot_longer
#' @export
impute_plots_to_pixels <- function(reference, target, predictors, responses,
                                    k = 1, ntree = 500, chunk_size = 1e5,
                                    seed = 42) {

  if (!requireNamespace("yaImpute", quietly = TRUE)) {
    stop("Package 'yaImpute' is required for imputation. ",
         "Install it with install.packages('yaImpute').", call. = FALSE)
  }

  stopifnot(all(predictors %in% names(reference)),
            all(predictors %in% names(target)),
            all(responses  %in% names(reference)))

  # drop reference plots / target pixels with missing predictors
  reference <- reference %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(c(predictors, responses)),
                                ~ !is.na(.)))
  target <- target %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(predictors), ~ !is.na(.)))

  # X = predictors (must match between ref & target); Y = responses (ref only)
  X <- reference[, predictors, drop = FALSE]
  Y <- reference[, responses,  drop = FALSE]
  rownames(X) <- rownames(Y) <- reference$plot_id

  set.seed(seed)
  ya <- yaImpute::yai(x = X, y = Y, method = "randomForest", k = k,
                      ntree = ntree)

  # Impute in chunks so millions of pixels don't blow up memory
  newX_all <- target[, predictors, drop = FALSE]
  rownames(newX_all) <- target$pixel_id
  idx <- split(seq_len(nrow(newX_all)),
               ceiling(seq_len(nrow(newX_all)) / chunk_size))

  out <- lapply(idx, function(rows) {
    imp <- yaImpute::newtargets(ya, newdata = newX_all[rows, , drop = FALSE])
    # neiIdsTrgs: target x k matrix of reference plot_ids; neiDstTrgs: distances
    ids  <- as.data.frame(imp$neiIdsTrgs)
    dist <- as.data.frame(imp$neiDstTrgs)
    ids$pixel_id <- rownames(imp$neiIdsTrgs)
    long_ids <- tidyr::pivot_longer(ids, -pixel_id,
                                    names_to = "neighbor",
                                    values_to = "plot_id")
    long_dist <- tidyr::pivot_longer(
      cbind(pixel_id = rownames(imp$neiDstTrgs), dist),
      -pixel_id, names_to = "neighbor", values_to = "distance")
    dplyr::bind_cols(long_ids, distance = long_dist$distance)
  })

  dplyr::bind_rows(out) %>%
    dplyr::mutate(pixel_id = as.integer(pixel_id)) %>%
    dplyr::left_join(target %>% dplyr::select(pixel_id, stand_id),
                     by = "pixel_id")
}

#' Build a Target Pixel Table from Rasters
#'
#' Converts a predictor raster stack and a stand-ID grid into a one-row-per-
#' forested-pixel data frame suitable for \code{\link{impute_plots_to_pixels}}.
#'
#' @param predictor_stack A \code{terra} \code{SpatRaster} with one layer per
#'   predictor; layer names must equal the predictor names.
#' @param stand_grid A \code{terra} \code{SpatRaster} of integer stand IDs
#'   (\code{> 0} forest; \code{<= 0} non-forest).
#'
#' @return A data frame with one row per forested pixel: \code{pixel_id},
#'   \code{stand_id}, and the predictor columns.
#'
#' @importFrom dplyr %>% rename filter
#' @importFrom terra resample
#' @export
make_target_from_rasters <- function(predictor_stack, stand_grid) {
  stand_grid <- terra::resample(stand_grid, predictor_stack, method = "near")
  df <- as.data.frame(c(predictor_stack, stand = stand_grid),
                      cells = TRUE, na.rm = FALSE)
  df %>%
    dplyr::rename(pixel_id = cell, stand_id = stand) %>%
    dplyr::filter(!is.na(stand_id), stand_id > 0)   # keep only forested stands
}

#' Assemble an iLand Stand Initialization File
#'
#' Builds a stand-level tree-list initialization file for iLand. Each pixel
#' inherits the tree list of its imputed plot (\code{k = 1}); a stand's tree
#' list is the area-weighted (per-pixel) mean of its pixels' lists, so a stand
#' spanning several plots blends them correctly. Final formatting, validation
#' and file writing are delegated to \code{\link{create_init_file}}.
#'
#' @param pixel_plot Output of \code{\link{impute_plots_to_pixels}} (use
#'   \code{k = 1}).
#' @param tree_lists Long data frame with columns \code{plot_id},
#'   \code{species}, \code{dbh_from}, \code{dbh_to}, \code{hd}, and \code{count}
#'   (count = trees/ha; one row per plot x species x DBH bin).
#' @param out_path Character scalar or \code{NULL}. Path of the file to write.
#'   Parent directories are created if needed. When \code{NULL} (default),
#'   nothing is written and the table is only returned. To write to a file,
#'   supply a path (e.g. \code{file.path(tempdir(), "init_trees.txt")}).
#'
#' @return The stand-level initialization table (written to \code{out_path}
#'   when it is non-\code{NULL}), invisibly returned as a data frame.
#'
#' @importFrom dplyr %>% filter first count inner_join group_by summarise
#'   left_join transmute
#' @importFrom stats weighted.mean
#' @export
build_iland_init <- function(pixel_plot, tree_lists,
                             out_path = NULL) {

  # k = 1 only: keep the single best-matching neighbour per pixel
  pixel_plot <- dplyr::filter(pixel_plot, neighbor == dplyr::first(neighbor))

  n_pix <- pixel_plot %>% dplyr::count(stand_id, name = "n_pixels")

  cohorts <- pixel_plot %>%
    # each pixel inherits all of its imputed plot's tree-list rows; plots are
    # shared across pixels, so the join is intentionally many-to-many
    dplyr::inner_join(tree_lists, by = "plot_id",
                      relationship = "many-to-many") %>%
    dplyr::group_by(stand_id, species, dbh_from, dbh_to) %>%
    dplyr::summarise(count_sum = sum(count),
                     hd        = stats::weighted.mean(hd, count),
                     .groups = "drop") %>%
    dplyr::left_join(n_pix, by = "stand_id") %>%
    dplyr::transmute(
      count    = round(count_sum / n_pixels, 2),  # average to per-ha per stand
      species,
      dbh_from = round(dbh_from, 1),
      dbh_to   = round(dbh_to, 1),
      hd       = round(hd, 1),
      stand_id
    )

  # delegate final formatting (age/density, ordering), validation and writing
  invisible(create_init_file(cohorts, cohorts = TRUE, filename = out_path))
}

#' Flag Pixels with Distant (Extrapolated) Imputations
#'
#' Marks target pixels whose imputation distance exceeds a quantile threshold.
#' Large distances flag pixels with no good analogue plot (extrapolation) so
#' they can be inspected or masked.
#'
#' @param pixel_plot Output of \code{\link{impute_plots_to_pixels}}.
#' @param quantile_cut Numeric in (0, 1). Distance quantile above which a pixel
#'   is flagged. Default \code{0.95}.
#'
#' @return \code{pixel_plot} with an added logical column \code{suspect}.
#'
#' @importFrom dplyr %>% mutate
#' @importFrom stats quantile
#' @export
flag_far_imputations <- function(pixel_plot, quantile_cut = 0.95) {
  thr <- stats::quantile(pixel_plot$distance, quantile_cut, na.rm = TRUE)
  pixel_plot %>% dplyr::mutate(suspect = distance > thr)
}
