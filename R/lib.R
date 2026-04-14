#' @import terra
#' @import sf
#' @import ggplot2
#' @import dplyr
#' @import curl
#' @import purrr
#' @import assertthat
#' @import httr
#' @import tmap
#' @import tidymodels
#' @import zoo
#' @import plotly

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  # Obtain the installed package information
  local_version <- utils::packageDescription("ForDis")

  msg <- paste0("Welcome to the Forest Distubance (ForDis) package, version ",
              local_version$Version)
  packageStartupMessage(msg)
}

