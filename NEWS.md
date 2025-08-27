# lcde.toolbox 1.0.0

* Initial release on CRAN.

## Highlights
* **Geospatial helpers**
  - `georef`: utilities to read, transform and work with geospatial objects.
  - `geoleaf`: convenience functions to build Leaflet maps and legends.
  - `geogg`: ggplot2 helpers tailored for geographic visualizations.

* **PCA workflow**
  - `pca`: wrapper around `stats::prcomp()` with tidy accessors.
  - `pcaviz`: plotting helpers for PCA results (scatter, loadings, explained variance).
  - `vizsize`: presets for consistent figure sizing and typography.

* **Reporting**
  - `ppt`: helpers to export plots and tables to PowerPoint via `officer` and `flextable`.

## Notes
* No network or file writes in examples.
* Package passes local checks with no Errors or Warnings.
