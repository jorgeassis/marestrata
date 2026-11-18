test_that("more than 24 layers stop before any file is opened", {
  catalog <- fixture_catalog()
  one <- marestrata_layers(variable = "sea_water_temperature", predictor = "mean", depth = 0, resolution = 0.05, period = "2010_2019", model = "copernicus", experiment = "baseline", period_type = "decadal", catalog = catalog)
  one <- one[one$source_variable == "thetao", , drop = FALSE]
  layers <- one[rep(1L, 25L), , drop = FALSE]
  layers$url <- paste0("https://data.example.org/marestrata/file-", seq_len(25), ".tif")
  expect_error(marestrata_load(layers), "25")
  expect_error(marestrata_load(layers), "allow_many")
})

test_that("a layers table and filter arguments cannot be combined", {
  catalog <- fixture_catalog()
  layers <- marestrata_layers(variable = "sea_water_salinity", catalog = catalog)
  expect_error(marestrata_load(layers, variable = "sea_water_temperature"), "not both")
})

test_that("an empty match and a missing domain stop before opening files", {
  catalog <- fixture_catalog()
  empty <- marestrata_layers(variable = "sea_water_temperature", depth = 9999, catalog = catalog)
  expect_equal(nrow(empty), 0)
  expect_error(marestrata_load(empty), "No layers matched")

  layers <- without_source(marestrata_layers(variable = "sea_water_salinity", catalog = catalog))
  expect_error(marestrata_load(layers), "data source")
})

test_that("layer names add model, experiment, and period only when they differ", {
  catalog <- fixture_catalog()
  same <- marestrata_layers(
    variable = "sea_water_temperature",
    predictor = c("mean", "max"),
    depth = 0,
    model = "copernicus",
    experiment = "baseline",
    period = "2010_2019",
    resolution = 0.05,
    period_type = "decadal",
    catalog = catalog
  )
  same <- same[same$source_variable == "thetao", , drop = FALSE]
  expect_equal(
    marestrata:::.layer_names(same),
    c("sea_water_temperature_0m_mean", "sea_water_temperature_0m_max")
  )

  varied <- marestrata_layers(
    variable = "sea_water_temperature",
    predictor = "mean",
    depth = 0,
    resolution = 0.05,
    period_type = "decadal",
    catalog = catalog
  )
  varied <- varied[varied$source_variable == "thetao" & varied$period %in% c("2010_2019", "2090_2099"), , drop = FALSE]
  expect_true(all(grepl("^sea_water_temperature_0m_mean_", marestrata:::.layer_names(varied))))
})
