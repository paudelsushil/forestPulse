# forestPulse <img src="man/figures/logo.png" align="right" height="139" alt="forestPulse hex logo" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/forestPulse)](https://CRAN.R-project.org/package=forestPulse)
[![CRAN downloads (monthly)](https://cranlogs.r-pkg.org/badges/forestPulse)](https://cran.r-project.org/package=forestPulse)
[![CRAN downloads (total)](https://cranlogs.r-pkg.org/badges/grand-total/forestPulse)](https://cran.r-project.org/package=forestPulse)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

**forestPulse** is a comprehensive toolkit for processing, analyzing, and
monitoring ecological changes in forest ecosystems using field data and
remote-sensing imagery. It is designed for researchers, ecologists, and forest
managers who need to quantify disturbance events and track long-term ecosystem
dynamics.

The package provides a unified workflow for:

- Indexing geotagged raster imagery into a spatial catalog and matching it
  to field plots.
- Computing canopy structure metrics — canopy height models (CHM), canopy
  gaps, and per-plot gap geometry.
- Calculating RGB-derived vegetation indices from drone orthomosaics.
- Locating individual trees from compass bearings and distance measurements.
- Summarising plot-level forest structure (basal area, crown density,
  species composition).

## Installation

### From CRAN (once released)

```r
install.packages("forestPulse")
```

### Development version from GitHub

```r
# install.packages("remotes")
remotes::install_github("paudelsushil/forestPulse")
```

## Quick example

```r
library(forestPulse)

# 1. Build a spatial catalog of geotagged rasters
catalog <- build_catalog(
  input  = "path/to/imagery",
  output = "catalog.geojson"
)

# 2. Match imagery to field plots
plots <- sf::read_sf("plots.shp")
plot_imagery <- match_imagery(catalog, plots)

# 3. Compute a canopy height model from DSM and DTM
chm <- compute_chm("dsm.tif", "dtm.tif", output = "chm.tif")

# 4. Extract per-plot canopy gap metrics
metrics <- extract_plot_metrics(
  chm           = chm,
  plots         = plots,
  radius        = 15.24,
  gap_threshold = 3
)

# 5. Compute an RGB vegetation index
vegetation_index("ExG", image_path = "orthomosaic.tif")
```

## Function reference

| Topic                       | Functions                                                                                          |
| --------------------------- | -------------------------------------------------------------------------------------------------- |
| Raster catalog              | `build_catalog()`, `read_catalog()`, `query_catalog()`, `match_imagery()`, `footprint()`           |
| Canopy height               | `compute_chm()`                                                                                    |
| Canopy gaps                 | `classify_gaps()`, `gap_metrics()`, `extract_plot_metrics()`                                       |
| Vegetation indices          | `vegetation_index()` (37 RGB indices)                                                              |
| Tree location               | `locate_tree()`                                                                                    |
| Basal area & plot summaries | `calculate_basal_area()`, `calculate_species_basal_area()`, `calculate_plot_species_basal_area()`, `calculate_plot_summary()` |
| Crown density               | `calculate_crownDensity()`, `show_calculation_details()`, `plot_summary()`                         |
| Species standardization     | `clean_species_names()`, `get_species_mapping()`, `check_unmapped_species()`, `compare_species_changes()` |
| Modelling features          | `create_binary_features()`                                                                         |

See the package documentation for full usage details:

```r
?forestPulse
help(package = "forestPulse")
```

## Citation

If you use **forestPulse** in published work, please cite it:

```r
citation("forestPulse")
```

## Getting help

- Report bugs or request features through the project's GitHub repository.
- For questions about usage, please include a minimal reproducible example.

## Contributing

Contributions are welcome. Please open an issue to discuss substantial
changes before submitting a pull request. By participating in this project
you agree to abide by its terms of conduct.

## License

MIT © Sushil Paudel. See the [LICENSE](LICENSE) file for details.
