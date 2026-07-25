# forestPulse 0.3.1

CRAN resubmission: addresses feedback from the initial submission.

* Added method references with DOIs/ISBN to the `DESCRIPTION` Description
  field (Allen et al., 1998, ISBN:9251042195; Abatzoglou, 2013,
  doi:10.1002/joc.3413; Riley et al., 2016, doi:10.1002/ecs2.1472).
* Replaced `\dontrun{}` with runnable examples throughout; the only
  remaining `\dontrun{}` is `extract_and_rename_tifs()`, which requires
  external zip archives and a zip utility.
* Functions no longer write to the user's filespace by default:
  `build_catalog(output = NULL)`, `build_iland_init(out_path = NULL)`, and
  `extract_plot_metrics(clip_dir = NULL)`. Examples write only to
  `tempdir()`.

# forestPulse 0.3.0

Major release: the package now spans field/remote-sensing analysis **and**
preparation of inputs for the 'iLand' forest landscape model.

## New features

* **iLand landscape inputs.** Build and cross-validate the spatial and tabular
  inputs for iLand: `create_stand_grid()`, `create_environment_grid()`,
  `create_environment_file()`, `create_init_file()`, `validate_landscape()`,
  and the optional one-call orchestrator `build_iland_landscape()`.

* **Climate database.** `write_iland_climate()` writes a validated, per-cluster
  'SQLite' climate database in iLand's format, with `gridmet_preprocessing()`
  turning a 'gridMET' extraction into a ready-to-write table. Added climate
  calculators `calc_vpd()`, `calc_saturation_vapor_pressure()`, and
  `convert_radiation()`.

* **Plot-to-pixel imputation.** Build wall-to-wall tree lists from field plots
  via random-forest imputation: `impute_plots_to_pixels()`,
  `make_target_from_rasters()`, `build_iland_init()`, and `flag_far_imputations()`.

* **New utilities.** `create_samples()` (generalised spatial sampling),
  `create_timeclass()`, `extractValue2Points()`, and `extract_and_rename_tifs()`.

## Documentation

* Added a Zenodo citation (`inst/CITATION`, doi:10.5281/zenodo.17887640),
  expanded the README for new users, and broadened the package Description.

----

Releases 0.2.x focused on CRAN compatibility and bug fixes.
