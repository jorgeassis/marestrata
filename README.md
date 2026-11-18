# marestrata

MareStrata is a dataset of ocean climate layers that extend [Bio-ORACLE](https://www.bio-oracle.org) across the water column and individual Earth System Models.

This R package queries the MareStrata catalog and opens the matching layers with [terra](https://rspatial.github.io/terra/). The GeoTIFFs are tiled, so an extent is read as a window. Over HTTP only that region is transferred. Locally, only that window is read from disk.

## Install

```r
install.packages("terra")
devtools::install("marestrata")
```

## Use

Set the data source once. It can be an HTTP(S) URL or a local directory that contains the MareStrata folders. The catalog at that source is `catalog.csv`. `marestrata_layers()` only filters the catalog. `marestrata_load()` opens the GeoTIFFs.

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
  period = "2090_2099"
)

r <- marestrata_load(
  variable = "sea_water_temperature",
  predictor = c("mean", "max"),
  depth = c(0, 50),
  model = "ensemble",
  experiment = "ssp585",
  period = "2090_2099",
  extent = c(-15, -5, 35, 45)
)
```

`extent` is `xmin, xmax, ymin, ymax` in longitude and latitude, or a terra `SpatExtent`. With no extent, terra opens the grid and reads cells only when values are requested. An extent that crosses the antimeridian (`xmin > xmax`) is read as two windows and merged. Pass `filename` to write the subset as one multi-layer GeoTIFF.

A request that matches more than 24 files stops unless `allow_many = TRUE`.

`variable` is the package name (`sea_water_temperature`, `sea_water_salinity`). Long names, standard names, and short filename tokens (`thetao`) still work as aliases. Geomorphologic products are under `geomorphologic_features`; product stems such as `slope` or `bathymetry_mean` are also accepted, and `layer` can select them. `predictor` accepts `min`, `ltmin`, `mean`, `ltmax`, `max`, `range`, or labels such as `"Long-term minimum"`. `depth` accepts metres, `"surface"`, `"bottom"`, `"bottom_min"`, `"bottom_mean"`, or `"bottom_max"`. `period` accepts `"2010_2019"` or `c(2010, 2019)`.

```r
slope <- marestrata_load(
  variable = "geomorphologic_features",
  layer = "slope",
  extent = c(-15, -5, 35, 45)
)
```

`MARESTRATA_URL` is used when `marestrata_source()` has not been called. It may be a URL or a local directory. `marestrata_catalog(refresh = TRUE)` loads a new catalog from `{source}/catalog.csv`. Otherwise the package uses the session copy, then the last cache for this source, then the catalog shipped with the package.

## Catalog

One row per published GeoTIFF. Mollweide climate files are not listed.

- `path` — relative path to where the raster files are located
- `variable` — package name, e.g. `sea_water_temperature`, or `geomorphologic_features`
- `source_variable` — short token from the filename, e.g. `thetao`, or the geomorphologic product stem
- `long_name`, `standard_name`, `units`, `folder`
- `model`, `experiment`, `period`, `year_start`, `year_end` — empty for most static geomorphologic products
- `layer` — depth token (`layer0`, `bottomlayermin`, …) or a geomorphologic product stem (`slope`)
- `depth_m` — metres, empty for bottom layers and static products
- `predictor` — `min`, `ltmin`, `mean`, `ltmax`, `max`, or `range` (empty for static products)
- `file` — GeoTIFF file name

Rebuild the snapshot from `viewer/catalog.json` with:

```bash
Rscript marestrata/data-raw/build_catalog.R
```

That writes `marestrata/inst/extdata/catalog.csv.gz` and `marestrata/data-raw/catalog.csv`. Place the uncompressed file at the source root as `catalog.csv`. To build the catalog from the data volume instead, set `MARESTRATA_ROOT` to that volume before running the script.

## Variables

Climate variables and the short token used in filenames (`source_variable`):

| Variable | Filename token | Long name | Units |
| --- | --- | --- | --- |
| `air_temperature` | `tas` | Air temperature | degrees_C |
| `concentration_of_chlorophyll` | `chl` | Chlorophyll | mg m-3 |
| `concentration_of_dissolved_iron` | `fe` | Dissolved iron | mmol m-3 |
| `concentration_of_dissolved_molecular_oxygen` | `o2` | Dissolved molecular oxygen | mmol m-3 |
| `concentration_of_nitrate` | `no3` | Nitrate | mmol m-3 |
| `concentration_of_phosphate` | `po4` | Phosphate | mmol m-3 |
| `concentration_of_phytoplankton` | `phyc` | Total phytoplankton | mmol m-3 |
| `concentration_of_silicate` | `si` | Silicate | mmol m-3 |
| `diffuse_attenuation_coefficient` | `KDPAR_mean` | Diffuse attenuation coefficient (PAR) | m-1 |
| `eastward_sea_water_velocity` | `uo` | Eastward sea-water velocity | m s-1 |
| `mixed_layer_thickness` | `mlotst` | Mixed-layer thickness | m |
| `northward_sea_water_velocity` | `vo` | Northward sea-water velocity | m s-1 |
| `photosynthetically_available_radiation` | `PAR_mean` | Photosynthetically available radiation | mol m-2 day-1 |
| `photosynthetically_available_radiation_at_bottom` | `parbottom` | Light at the bottom | mol m-2 day-1 |
| `sea_ice_area_fraction` | `siconc` | Sea-ice area fraction | 1 |
| `sea_ice_thickness` | `sithick` | Sea-ice thickness | m |
| `sea_water_ph` | `ph` | Sea-water pH | 1 |
| `sea_water_salinity` | `so` | Salinity | 1e-3 |
| `sea_water_temperature` | `thetao` | Ocean temperature | degrees_C |
| `total_precipitation` | `pr` | Precipitation | m day-1 |

Geomorphologic products (`variable = "geomorphologic_features"`). The product stem is both `layer` and `source_variable`:

| Product | Meaning |
| --- | --- |
| `area` | Cell area |
| `aspect` | Terrain aspect |
| `bathymetry_max` | Maximum bathymetry |
| `bathymetry_mean` | Mean bathymetry |
| `bathymetry_min` | Minimum bathymetry |
| `coastline` | Coastline mask |
| `distace_to_delta` | Distance to river delta |
| `distace_to_major_cities` | Distance to major cities |
| `distace_to_ports` | Distance to ports |
| `distance_to_land` | Distance to land |
| `eulittoral_zone` | Eulittoral zone (also filtered by `experiment`) |
| `fishing_effort` | Fishing effort |
| `landmass` | Land mask |
| `ocean` | Ocean mask |
| `slope` | Seafloor slope |
| `terrain_ruggedness_index` | Terrain ruggedness index (TRI) |
| `tidal_amplitude` | Tidal amplitude |
| `topographic_position_index` | Topographic position index (TPI) |
| `wave_exposure` | Wave exposure |
| `wave_exposure_exp` | Exponential wave exposure |

## Models

| Model | Meaning |
| --- | --- |
| `copernicus` | Observation-based / Copernicus ocean product |
| `ecmwf` | ECMWF product |
| `ensemble` | Multi-model ensemble mean |
| `ensemble_sd` | Multi-model ensemble standard deviation |
| `access-esm1-5` | ACCESS-ESM1-5 |
| `canesm5` | CanESM5 |
| `cesm2-waccm` | CESM2-WACCM |
| `cnrm-esm2-1` | CNRM-ESM2-1 |
| `gfdl-esm4` | GFDL-ESM4 |
| `giss-e2-1-g` | GISS-E2-1-G |
| `ipsl-cm6a-lr` | IPSL-CM6A-LR |
| `miroc-es2l` | MIROC-ES2L |
| `mpi-esm1-2-lr` | MPI-ESM1-2-LR |
| `mri-esm2-0` | MRI-ESM2-0 |
| `ukesm1-0-ll` | UKESM1-0-LL |

## Layers

Climate depth layers in the filename (`layer`), also selectable with `depth`:

| Layer | Meaning |
| --- | --- |
| `layer0` | Surface (0 m); also `"surface"` or `depth = 0` |
| `layer25` | 25 m |
| `layer50` | 50 m |
| `layer75` | 75 m |
| `layer100` | 100 m |
| `layer150` | 150 m |
| `layer200` | 200 m |
| `layer250` | 250 m |
| `layer500` | 500 m |
| `layer750` | 750 m |
| `layer1000` | 1000 m |
| `layer1500` | 1500 m |
| `layer2000` | 2000 m |
| `layer2500` | 2500 m |
| `layer5000` | 5000 m |
| `bottomlayermin` | Minimum value over the bottom layer; also `"bottom_min"` |
| `bottomlayermean` | Mean value over the bottom layer; also `"bottom_mean"` |
| `bottomlayermax` | Maximum value over the bottom layer; also `"bottom_max"` |

`"bottom"` selects all three bottom layers. Geomorphologic product stems are also stored in `layer` (see above).

## Predictors

| Predictor | Meaning |
| --- | --- |
| `min` | Minimum |
| `ltmin` | Long-term minimum |
| `mean` | Mean |
| `ltmax` | Long-term maximum |
| `max` | Maximum |
| `range` | Range |
