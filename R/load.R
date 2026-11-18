.layer_limit <- 24L

.configure_gdal <- function() {
  # Left in place because terra reads the window later, when values are used.
  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
  terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif")
  terra::setGDALconfig("GDAL_HTTP_MULTIRANGE", "YES")
  terra::setGDALconfig("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES", "YES")
  invisible(NULL)
}

.extent_windows <- function(extent) {
  if (is.null(extent)) return(NULL)
  if (inherits(extent, "SpatExtent")) {
    box <- c(terra::xmin(extent), terra::xmax(extent), terra::ymin(extent), terra::ymax(extent))
  } else if (is.numeric(extent) && length(extent) == 4L) {
    box <- as.numeric(extent)
  } else {
    stop("`extent` must be xmin, xmax, ymin, ymax, or a SpatExtent.", call. = FALSE)
  }
  if (any(!is.finite(box))) stop("`extent` must be finite.", call. = FALSE)
  xmin <- box[1]
  xmax <- box[2]
  ymin <- box[3]
  ymax <- box[4]
  if (ymin >= ymax) stop("`extent` ymin must be less than ymax.", call. = FALSE)
  if (ymax <= -90 || ymin >= 90 || xmin == xmax) {
    stop("`extent` does not overlap the layer grid (-180, 180, -90, 90).", call. = FALSE)
  }
  ymin <- max(ymin, -90)
  ymax <- min(ymax, 90)
  window <- function(left, right, shift) {
    list(xmin = left, xmax = right, ymin = ymin, ymax = ymax, shift = shift)
  }
  if (xmin < xmax) {
    if (xmax <= -180 || xmin >= 180) {
      stop("`extent` does not overlap the layer grid (-180, 180, -90, 90).", call. = FALSE)
    }
    xmin <- max(xmin, -180)
    xmax <- min(xmax, 180)
    if (xmin >= xmax) {
      stop("`extent` does not overlap the layer grid (-180, 180, -90, 90).", call. = FALSE)
    }
    return(list(window(xmin, xmax, 0)))
  }
  windows <- list()
  if (xmin < 180) windows[[length(windows) + 1L]] <- window(max(xmin, -180), 180, 0)
  if (xmax > -180) windows[[length(windows) + 1L]] <- window(-180, min(xmax, 180), 360)
  if (!length(windows)) {
    stop("`extent` does not overlap the layer grid (-180, 180, -90, 90).", call. = FALSE)
  }
  windows
}

.open_source <- function(location, windows) {
  remote <- grepl("^https?://", location, ignore.case = TRUE)
  src <- if (remote) paste0("/vsicurl/", location) else location
  if (is.null(windows)) return(terra::rast(src))
  pieces <- lapply(windows, function(window) {
    raster <- terra::rast(
      src,
      win = terra::ext(window$xmin, window$xmax, window$ymin, window$ymax),
      snap = "out"
    )
    if (window$shift != 0) raster <- terra::shift(raster, dx = window$shift)
    raster
  })
  if (length(pieces) == 1L) return(pieces[[1L]])
  terra::merge(pieces[[1L]], pieces[[2L]])
}

.layer_names <- function(layers) {
  depth <- ifelse(
    grepl("^bottomlayer", layers$layer),
    sub("^bottomlayer", "bottom_", layers$layer),
    ifelse(!is.na(layers$depth_m), paste0(layers$depth_m, "m"), layers$layer)
  )
  labels <- paste(layers$variable, depth, layers$predictor, sep = "_")
  append_when_varied <- function(labels, values, prefix) {
    present <- values[!is.na(values)]
    if (length(unique(present)) <= 1L) return(labels)
    paste0(labels, "_", prefix, values)
  }
  labels <- append_when_varied(labels, layers$model, "")
  labels <- append_when_varied(labels, layers$experiment, "")
  labels <- append_when_varied(labels, layers$period, "")
  labels <- append_when_varied(labels, formatC(layers$resolution, format = "f", digits = 2), "r")
  labels <- append_when_varied(labels, layers$period_type, "")
  make.unique(labels, sep = "_")
}

.require_layers <- function(layers) {
  if (!is.data.frame(layers)) {
    stop("`layers` must be a data frame from marestrata_layers().", call. = FALSE)
  }
  required <- c(
    "url", "variable", "layer", "depth_m", "predictor", "model", "experiment",
    "period", "units", "resolution", "period_type"
  )
  missing <- setdiff(required, names(layers))
  if (length(missing)) {
    stop(
      "`layers` must come from marestrata_layers(). Missing columns: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!nrow(layers)) stop("No layers matched.", call. = FALSE)
  layers
}

#' Open MareStrata layers
#'
#' Each matching GeoTIFF is opened with terra. Remote URLs use `/vsicurl/`.
#' Local paths are opened directly. `extent` is applied as a read window, so
#' only that longitude and latitude range is read. An extent with
#' `xmin > xmax` crosses the antimeridian: the two sides are read separately,
#' the eastern side is shifted by 360 degrees, and the pieces are merged.
#'
#' @param layers Data frame from [marestrata_layers()]. Omit it to pass filter arguments in `...`.
#' @param extent `xmin, xmax, ymin, ymax` in degrees, or a `SpatExtent`. Omit it to open the whole grid without reading cells yet.
#' @param filename Optional path. The subset is written there as one multi-layer GeoTIFF.
#' @param ... Filter arguments passed to [marestrata_layers()] when `layers` is omitted.
#' @param allow_many Logical. A request for more than 24 files stops unless this is `TRUE`.
#' @return A `SpatRaster`. The source table is attached as the `marestrata` attribute.
#' @export
#' @examples
#' \dontrun{
#' marestrata_source("/Volumes/JellyFish")
#' marestrata_load(
#'   variable = "sea_water_temperature",
#'   predictor = c("mean", "max"),
#'   depth = c(0, 50),
#'   model = "ensemble",
#'   experiment = "ssp585",
#'   period = "2090_2099",
#'   resolution = 0.25,
#'   extent = c(-15, -5, 35, 45)
#' )
#' }
marestrata_load <- function(layers = NULL, extent = NULL, filename = NULL, ..., allow_many = FALSE) {
  dots <- list(...)
  if (is.null(layers)) {
    layers <- do.call(marestrata_layers, dots)
  } else if (length(dots)) {
    stop("Pass a layers table or filter arguments, not both.", call. = FALSE)
  }
  if (!is.logical(allow_many) || length(allow_many) != 1L || is.na(allow_many)) {
    stop("`allow_many` must be TRUE or FALSE.", call. = FALSE)
  }
  layers <- .require_layers(layers)
  if (nrow(layers) > .layer_limit && !isTRUE(allow_many)) {
    stop(
      "The query matches ", nrow(layers),
      " files. Narrow the filters, or set allow_many = TRUE.",
      call. = FALSE
    )
  }
  if (anyNA(layers$url) || any(!nzchar(layers$url))) {
    stop("Set the data source with marestrata_source() before opening layers.", call. = FALSE)
  }
  if (!is.null(filename)) {
    if (!is.character(filename) || length(filename) != 1L || !nzchar(filename)) {
      stop("`filename` must be a single path.", call. = FALSE)
    }
  }
  windows <- .extent_windows(extent)
  if (any(grepl("^https?://", layers$url, ignore.case = TRUE))) .configure_gdal()
  rasters <- lapply(layers$url, .open_source, windows = windows)
  out <- if (length(rasters) == 1L) rasters[[1L]] else do.call(c, rasters)
  if (terra::nlyr(out) != nrow(layers)) {
    stop("The opened files do not have one band per catalog row.", call. = FALSE)
  }
  names(out) <- .layer_names(layers)
  layer_units <- layers$units
  layer_units[is.na(layer_units)] <- ""
  terra::units(out) <- layer_units
  attr(out, "marestrata") <- as.data.frame(layers, stringsAsFactors = FALSE)
  if (!is.null(filename)) terra::writeRaster(out, filename, overwrite = TRUE)
  out
}
