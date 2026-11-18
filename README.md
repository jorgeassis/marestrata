# marestrata

MareStrata is a dataset of ocean climate layers that extend [Bio-ORACLE](https://www.bio-oracle.org) across the water column and individual Earth System Models.

This R package queries the MareStrata catalog and opens the matching layers with [terra](https://rspatial.github.io/terra/). The GeoTIFFs are tiled, so an extent is read as a window. Over HTTP only that region is transferred. Locally, only that window is read from disk.

## Install

```r
install.packages("terra")
devtools::install("marestrata")
```

## Use

Set the data source once. It can be an HTTP(S) URL or a local directory that contains the `MareStrata …` folders. The catalog at that source is `catalog.csv`. `marestrata_layers()` only filters the catalog. `marestrata_load()` opens the GeoTIFFs.

```r
# Local archive
marestrata_source("/data/marestrata")

# Or a web domain
# marestrata_source("https://data.example.org/marestrata")

layers <- marestrata_layers(
  variable = "sea_water_temperature",
  predictor = c("mean", "max"),
  depth = c(0, 50),
  model = "ensemble",
  experiment = "ssp585",
  period = "2090_2099",
  resolution = 0.25
)

r <- marestrata_load(
  variable = "sea_water_temperature",
  predictor = c("mean", "max"),
  depth = c(0, 50),
  model = "ensemble",
  experiment = "ssp585",
  period = "2090_2099",
  resolution = 0.25,
  extent = c(-15, -5, 35, 45)
)
```

`extent` is `xmin, xmax, ymin, ymax` in longitude and latitude, or a terra `SpatExtent`. With no extent, terra opens the grid and reads cells only when values are requested. An extent that crosses the antimeridian (`xmin > xmax`) is read as two windows and merged. Pass `filename` to write the subset as one multi-layer GeoTIFF.

A request that matches more than 24 files stops unless `allow_many = TRUE`.

`variable` is the package name (`sea_water_temperature`, `sea_water_salinity`). Long names, standard names, and short CF tokens (`thetao`) still work as aliases. `predictor` accepts `min`, `ltmin`, `mean`, `ltmax`, `max`, `range`, or labels such as `"Long-term minimum"`. `depth` accepts metres, `"surface"`, `"bottom"`, `"bottom_min"`, `"bottom_mean"`, or `"bottom_max"`. `period` accepts `"2010_2019"` or `c(2010, 2019)`. `resolution` is `0.05` or `0.25`.

`MARESTRATA_URL` is used when `marestrata_source()` has not been called. It may be a URL or a local directory. `marestrata_catalog(refresh = TRUE)` loads a new catalog from `{source}/catalog.csv`. Otherwise the package uses the session copy, then the last cache for this source, then the catalog shipped with the package.

## Catalog

One row per `*_wgs84.tif`. Mollweide files are not listed.

- `path` — relative path, including the `MareStrata 005` or `MareStrata 025` folder and the filename
- `resolution` — `0.05` or `0.25`
- `period_type` — `decadal` or `annual`
- `variable` — package name, e.g. `sea_water_temperature`
- `source_variable` — short CF token from the filename, e.g. `thetao`
- `long_name`, `standard_name`, `units`, `folder`
- `model`, `experiment`, `period`, `year_start`, `year_end`
- `layer` — `layer0`, `layer25`, … or `bottomlayermin`, `bottomlayermean`, `bottomlayermax`
- `depth_m` — metres, empty for bottom layers
- `predictor` — `min`, `ltmin`, `mean`, `ltmax`, `max`, or `range`
- `file` — GeoTIFF file name

HTTP paths are joined one folder at a time, so `MareStrata 005` is requested as `MareStrata%20005`. Local paths keep the space.

Rebuild the snapshot from `viewer/catalog.json` with:

```bash
Rscript marestrata/data-raw/build_catalog.R
```

That writes `marestrata/inst/extdata/catalog.csv.gz` and `marestrata/data-raw/catalog.csv`. Place the uncompressed file at the source root as `catalog.csv`. To build the catalog from the data volume instead, set `MARESTRATA_ROOT` to that volume before running the script.
