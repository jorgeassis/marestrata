test_that("variable aliases match package names, long names, standard names, and short CF tokens", {
  catalog <- fixture_catalog()
  package_name <- marestrata_layers(variable = "sea_water_temperature", catalog = catalog)
  expect_true(all(package_name$variable == "sea_water_temperature"))
  expect_true(all(package_name$source_variable %in% c("thetao", "tos")))

  standard <- marestrata_layers(variable = "sea_water_potential_temperature", catalog = catalog)
  expect_true(all(standard$variable == "sea_water_temperature"))
  expect_true(all(standard$source_variable == "thetao"))

  long_name <- marestrata_layers(variable = "OceanTemperature", catalog = catalog)
  expect_true(all(long_name$variable == "sea_water_temperature"))

  short <- marestrata_layers(variable = "thetao", catalog = catalog)
  expect_true(all(short$variable == "sea_water_temperature"))
  expect_true(all(short$source_variable == "thetao"))

  salinity <- marestrata_layers(variable = "sea_water_salinity", catalog = catalog)
  expect_true(all(salinity$variable == "sea_water_salinity"))
  expect_true(all(salinity$source_variable == "so"))
})

test_that("unknown variables and predictors report the accepted choices", {
  catalog <- fixture_catalog()
  expect_error(marestrata_layers(variable = "nope", catalog = catalog), "sea_water_temperature")
  expect_error(marestrata_layers(predictor = "median", catalog = catalog), "mean")
})

test_that("predictor labels and depth aliases select the same layers as tokens", {
  catalog <- fixture_catalog()
  labelled <- marestrata_layers(variable = "sea_water_temperature", predictor = "Long-term minimum", catalog = catalog)
  expect_true(all(labelled$predictor == "ltmin"))
  expect_gt(nrow(labelled), 0)

  surface <- marestrata_layers(variable = "sea_water_temperature", predictor = "mean", depth = "surface", catalog = catalog)
  numeric_depth <- marestrata_layers(variable = "sea_water_temperature", predictor = "mean", depth = 0, catalog = catalog)
  token <- marestrata_layers(variable = "sea_water_temperature", predictor = "mean", depth = "layer0", catalog = catalog)
  expect_equal(surface$path, numeric_depth$path)
  expect_equal(surface$path, token$path)
  expect_true(all(surface$layer == "layer0"))

  bottom <- marestrata_layers(variable = "sea_water_temperature", depth = "bottom", catalog = catalog)
  expect_true(all(grepl("^bottomlayer", bottom$layer)))
  bottom_min <- marestrata_layers(variable = "sea_water_temperature", depth = "bottom_min", catalog = catalog)
  expect_equal(bottom_min$layer, "bottomlayermin")

  both <- marestrata_layers(variable = "sea_water_temperature", predictor = "mean", depth = c(0, 50), model = "copernicus", catalog = catalog)
  expect_setequal(both$depth_m, c(0, 50))
  expect_false(any(grepl("^bottomlayer", both$layer)))
})

test_that("period, resolution, model, and experiment filters are case insensitive", {
  catalog <- fixture_catalog()
  window <- marestrata_layers(variable = "sea_water_temperature", period = c(2010, 2019), catalog = catalog)
  expect_true(all(window$period == "2010_2019"))
  expect_false(any(window$period == "2090_2099"))

  hyphen <- marestrata_layers(variable = "sea_water_temperature", period = "2010-2019", catalog = catalog)
  expect_equal(hyphen$path, window$path)

  annual <- marestrata_layers(variable = "sea_water_temperature", period = 2010, period_type = "annual", catalog = catalog)
  expect_true(all(annual$period == "2010"))
  expect_true(all(annual$period_type == "annual"))

  coarse <- marestrata_layers(variable = "sea_water_temperature", resolution = 0.25, catalog = catalog)
  expect_true(all(abs(coarse$resolution - 0.25) < 1e-6))

  model <- marestrata_layers(variable = "sea_water_temperature", model = "Copernicus", experiment = "BASELINE", catalog = catalog)
  expect_true(all(model$model == "copernicus"))
  expect_true(all(model$experiment == "baseline"))
})

test_that("a combined query returns one file and encodes the url by path segment", {
  catalog <- fixture_catalog()
  old_option <- getOption("marestrata.base_url")
  on.exit(options(marestrata.base_url = old_option), add = TRUE)
  marestrata_source("https://data.example.org/marestrata/")

  layers <- marestrata_layers(
    variable = "thetao",
    predictor = c("mean", "max"),
    depth = 0,
    model = "copernicus",
    experiment = "baseline",
    period = "2010_2019",
    resolution = 0.05,
    period_type = "decadal",
    catalog = catalog
  )
  expect_equal(sort(layers$predictor), c("max", "mean"))
  mean_row <- layers[layers$predictor == "mean", , drop = FALSE]
  expect_equal(nrow(mean_row), 1)
  expect_equal(mean_row$source_variable, "thetao")
  expect_equal(
    mean_row$url,
    "https://data.example.org/marestrata/MareStrata%20005/decadal/sea_water_temperature/clim_copernicus_thetao_baseline_2010_2019_layer0_mean_wgs84.tif"
  )
})

test_that("urls stay empty until a web domain is set", {
  layers <- without_source(marestrata_layers(variable = "sea_water_salinity", catalog = fixture_catalog()))
  expect_equal(nrow(layers), 1)
  expect_true(is.na(layers$url))
})

test_that("the source must be http(s) or an existing directory", {
  expect_error(marestrata_source("ftp://example.org/marestrata"), "http")
  expect_error(marestrata_source(file.path(tempdir(), "missing-marestrata")), "directory")
})

test_that("a local directory is joined as a file path", {
  root <- file.path(tempdir(), "marestrata-local-root")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  old_option <- getOption("marestrata.base_url")
  on.exit(options(marestrata.base_url = old_option), add = TRUE)
  marestrata_source(root)

  layers <- marestrata_layers(
    variable = "sea_water_salinity",
    catalog = fixture_catalog()
  )
  expect_equal(nrow(layers), 1)
  expect_equal(
    layers$url,
    file.path(normalizePath(root, winslash = "/"), "MareStrata 005", "decadal", "sea_water_salinity", layers$file)
  )
})

test_that("the bundled catalog has one row per published layer", {
  catalog <- marestrata:::.bundled_catalog()
  expect_gt(nrow(catalog), 1000)
  expect_true("sea_water_temperature" %in% catalog$variable)
  expect_false("thetao" %in% catalog$variable)
  expect_true("thetao" %in% catalog$source_variable)
  expect_true("geomorphologic_features" %in% catalog$variable)
  expect_true("slope" %in% catalog$layer)
  expect_true(all(catalog$resolution %in% c(0.05, 0.25)))
  expect_true(all(grepl("^MareStrata ", catalog$path)))
  expect_false(any(grepl("BLUE-ARRAY", catalog$path)))
  climate <- catalog$variable != "geomorphologic_features"
  # One published bottom-light file is named with a repeated .tif suffix.
  expect_true(all(grepl("_wgs84\\.tif(\\.tif)?$", catalog$file[climate])))
  expect_true(all(grepl("\\.tif$", catalog$file[!climate], ignore.case = TRUE)))
})

test_that("missing named-vector filters are ignored for static products", {
  catalog <- fixture_catalog()
  # Mimic data_layers[[p]]["predictor"] when the key is absent.
  spec <- c(
    resolution = "0.05",
    period_type = "decadal",
    variable = "slope"
  )
  layers <- without_source(marestrata_layers(
    variable = spec["variable"],
    predictor = spec["predictor"],
    depth = spec["layer"],
    experiment = "baseline",
    period = "2010_2019",
    resolution = spec["resolution"],
    catalog = catalog
  ))
  expect_equal(nrow(layers), 1)
  expect_equal(layers$layer, "slope")
})

test_that("geomorphologic features can be selected by folder or product stem", {
  catalog <- fixture_catalog()
  all_features <- without_source(marestrata_layers(
    variable = "geomorphologic_features",
    catalog = catalog
  ))
  expect_equal(nrow(all_features), 3)
  expect_true(all(abs(all_features$resolution - 0.05) < 1e-6))
  expect_true(all(all_features$period_type == "decadal"))
  slope <- without_source(marestrata_layers(
    variable = "geomorphologic_features",
    layer = "slope",
    catalog = catalog
  ))
  expect_equal(nrow(slope), 1)
  expect_equal(slope$file, "slope.tif")
  by_stem <- without_source(marestrata_layers(variable = "bathymetry_mean", catalog = catalog))
  expect_equal(nrow(by_stem), 1)
  eulittoral <- without_source(marestrata_layers(
    variable = "eulittoral_zone",
    experiment = "ssp585",
    catalog = catalog
  ))
  expect_equal(nrow(eulittoral), 1)
  expect_equal(eulittoral$file, "eulittoral_zone_SSP585.tif")
})

test_that("local MareStrata paths fall back to previous folder names when needed", {
  root <- file.path(tempdir(), "marestrata-legacy-root")
  legacy <- file.path(root, "BLUE-ARRAY 005", "decadal", "sea_water_salinity")
  dir.create(legacy, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- "clim_copernicus_so_baseline_2010_2019_layer0_mean_wgs84.tif"
  writeLines("tif", file.path(legacy, file))
  branded <- file.path(root, "MareStrata 005", "decadal", "sea_water_salinity", file)
  expect_false(file.exists(branded))
  expect_equal(marestrata:::.resolve_local_path(branded), file.path(legacy, file))
})
