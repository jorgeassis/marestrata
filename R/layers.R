.predictor_labels <- c(
  min = "minimum",
  ltmin = "long-term minimum",
  mean = "mean",
  ltmax = "long-term maximum",
  max = "maximum",
  range = "range"
)

.bottom_layers <- c(
  bottom_min = "bottomlayermin",
  bottom_mean = "bottomlayermean",
  bottom_max = "bottomlayermax"
)

.require_text <- function(values, arg_name) {
  if (length(values) == 0L) {
    stop("`", arg_name, "` must contain at least one value.", call. = FALSE)
  }
  if (anyNA(values)) {
    stop("`", arg_name, "` must not contain missing values.", call. = FALSE)
  }
  text <- trimws(as.character(values))
  if (any(!nzchar(text))) {
    stop("`", arg_name, "` must not contain blank values.", call. = FALSE)
  }
  text
}

.choice_error <- function(label, unknown, choices) {
  stop(
    "Unknown ", label, ": ", paste(unique(unknown), collapse = ", "),
    ". Choices: ", paste(choices, collapse = ", "), ".",
    call. = FALSE
  )
}

.match_catalog_tokens <- function(requested, available, label) {
  requested <- .require_text(requested, label)
  available <- unique(available[!is.na(available) & nzchar(available)])
  found <- available[match(tolower(requested), tolower(available))]
  missing <- requested[is.na(found)]
  if (length(missing)) .choice_error(label, missing, sort(available))
  unique(found)
}

.column_equals <- function(values, folded) {
  !is.na(values) & nzchar(values) & tolower(values) == folded
}

# Match package names broadly. Short CF tokens, geomorphologic product stems,
# and standard names stay row-specific.
.resolve_variable_rows <- function(variable, catalog) {
  if (is.null(variable)) return(NULL)
  requested <- .require_text(variable, "variable")
  keep <- rep(FALSE, nrow(catalog))
  unknown <- character()
  for (key in requested) {
    folded <- tolower(key)
    hit <- .column_equals(catalog$variable, folded) |
      .column_equals(catalog$folder, folded) |
      .column_equals(catalog$long_name, folded) |
      .column_equals(catalog$layer, folded)
    if ("source_variable" %in% names(catalog)) {
      hit <- hit | .column_equals(catalog$source_variable, folded)
    }
    if ("standard_name" %in% names(catalog)) {
      hit <- hit | .column_equals(catalog$standard_name, folded)
    }
    if (!any(hit)) unknown <- c(unknown, key) else keep <- keep | hit
  }
  if (length(unknown)) {
    choices <- sort(unique(c(catalog$variable, catalog$source_variable, catalog$layer)))
    choices <- choices[nzchar(choices)]
    .choice_error("variable", unknown, choices)
  }
  keep
}

.canonical_predictor <- function(value) {
  key <- tolower(trimws(value))
  key <- gsub("-", " ", key, fixed = TRUE)
  key <- gsub("_", " ", key, fixed = TRUE)
  key <- gsub("[[:space:]]+", " ", key)
  tokens <- names(.predictor_labels)
  if (key %in% tokens) return(key)
  labels <- gsub("-", " ", unname(.predictor_labels), fixed = TRUE)
  if (key %in% labels) return(tokens[match(key, labels)])
  NA_character_
}

.resolve_predictors <- function(predictor, catalog) {
  if (is.null(predictor)) return(NULL)
  requested <- .require_text(predictor, "predictor")
  catalog_tokens <- unique(catalog$predictor[!is.na(catalog$predictor) & nzchar(catalog$predictor)])
  resolved <- vapply(requested, .canonical_predictor, character(1), USE.NAMES = FALSE)
  unmatched <- is.na(resolved)
  if (any(unmatched)) {
    folded <- tolower(catalog_tokens)
    for (index in which(unmatched)) {
      hit <- match(tolower(requested[index]), folded)
      if (!is.na(hit)) resolved[index] <- catalog_tokens[hit]
    }
  }
  missing <- requested[is.na(resolved)]
  if (length(missing)) {
    choices <- unique(c(names(.predictor_labels), catalog_tokens))
    .choice_error("predictor", missing, choices)
  }
  unique(resolved)
}

.resolve_depth <- function(depth) {
  if (is.null(depth)) return(NULL)
  if (is.numeric(depth)) {
    if (length(depth) == 0L || any(!is.finite(depth))) {
      stop("`depth` must be finite metres, \"surface\", \"bottom\", \"bottom_min\", \"bottom_mean\", or \"bottom_max\".", call. = FALSE)
    }
    return(list(metres = unique(as.numeric(depth)), layers = character(), bottom = FALSE))
  }
  requested <- .require_text(depth, "depth")
  metres <- numeric()
  layers <- character()
  bottom <- FALSE
  for (value in requested) {
    key <- tolower(value)
    key <- gsub("-", "_", key, fixed = TRUE)
    key <- gsub("[[:space:]]+", "_", key)
    if (key %in% c("surface", "0", "layer0")) {
      metres <- c(metres, 0)
    } else if (key == "bottom") {
      bottom <- TRUE
    } else if (key %in% c("bottom_min", "bottom_minimum", "bottomlayermin")) {
      layers <- c(layers, .bottom_layers[["bottom_min"]])
    } else if (key %in% c("bottom_mean", "bottomlayermean")) {
      layers <- c(layers, .bottom_layers[["bottom_mean"]])
    } else if (key %in% c("bottom_max", "bottom_maximum", "bottomlayermax")) {
      layers <- c(layers, .bottom_layers[["bottom_max"]])
    } else if (grepl("^layer[0-9]+(\\.[0-9]+)?$", key)) {
      metres <- c(metres, as.numeric(sub("^layer", "", key)))
    } else if (grepl("^[0-9]+(\\.[0-9]+)?$", key)) {
      metres <- c(metres, as.numeric(key))
    } else {
      stop(
        "Unknown depth: ", value,
        ". Use a depth in metres, \"surface\", \"bottom\", \"bottom_min\", \"bottom_mean\", or \"bottom_max\".",
        call. = FALSE
      )
    }
  }
  list(metres = unique(metres), layers = unique(layers), bottom = bottom)
}

.normalize_period <- function(period, available) {
  if (is.null(period)) return(NULL)
  if (is.numeric(period)) {
    if (length(period) == 0L || any(!is.finite(period))) {
      stop("`period` must be a window such as \"2010_2019\" or c(2010, 2019).", call. = FALSE)
    }
    years <- as.integer(period)
    tokens <- if (length(years) == 2L) paste0(years[1], "_", years[2]) else as.character(years)
  } else {
    tokens <- gsub("-", "_", .require_text(period, "period"), fixed = TRUE)
  }
  .match_catalog_tokens(tokens, available, "period")
}

.resolve_resolution <- function(resolution) {
  if (is.null(resolution)) return(NULL)
  if (is.numeric(resolution)) {
    if (length(resolution) == 0L || any(!is.finite(resolution))) {
      stop("`resolution` must be 0.05 or 0.25.", call. = FALSE)
    }
    requested <- as.character(resolution)
  } else {
    requested <- .require_text(resolution, "resolution")
  }
  resolved <- vapply(requested, function(value) {
    key <- tolower(trimws(value))
    number <- suppressWarnings(as.numeric(key))
    if (!is.na(number) && abs(number - 0.05) < 1e-6) return("0.05")
    if (!is.na(number) && abs(number - 0.25) < 1e-6) return("0.25")
    if (key %in% c("005", "5")) return("0.05")
    if (key %in% c("025", "25")) return("0.25")
    NA_character_
  }, character(1), USE.NAMES = FALSE)
  missing <- requested[is.na(resolved)]
  if (length(missing)) .choice_error("resolution", missing, c("0.05", "0.25"))
  as.numeric(unique(resolved))
}

.layer_location <- function(path, base) {
  base <- sub("/+$", "", base)
  if (.is_remote_source(base)) {
    return(vapply(path, function(one) {
      if (is.na(one) || !nzchar(one)) return(NA_character_)
      parts <- strsplit(one, "/", fixed = TRUE)[[1]]
      parts <- parts[nzchar(parts)]
      encoded <- vapply(parts, utils::URLencode, character(1), reserved = TRUE, USE.NAMES = FALSE)
      paste0(base, "/", paste(encoded, collapse = "/"))
    }, character(1), USE.NAMES = FALSE))
  }
  vapply(path, function(one) {
    if (is.na(one) || !nzchar(one)) return(NA_character_)
    parts <- strsplit(one, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    do.call(file.path, as.list(c(base, parts)))
  }, character(1), USE.NAMES = FALSE)
}

#' List MareStrata layers
#'
#' Filters the catalog. Nothing is downloaded. `variable` is the package name
#' such as `sea_water_temperature` or `sea_water_salinity`. Long names,
#' standard names, folder names, and short CF tokens (`thetao`) are accepted as
#' aliases. Geomorphologic products are listed under
#' `geomorphologic_features`; product stems such as `slope` or
#' `bathymetry_mean` are also accepted. `predictor` accepts tokens (`ltmin`)
#' and labels (`"Long-term minimum"`). `depth` accepts metres, `"surface"`,
#' `"bottom"`, `"bottom_min"`, `"bottom_mean"`, and `"bottom_max"`. `period`
#' accepts `"2010_2019"` or `c(2010, 2019)`.
#'
#' @param variable Package variable name (`sea_water_temperature`), geomorphologic
#'   product stem (`slope`), or an alias.
#' @param predictor Predictor token or label.
#' @param depth Depth in metres, or a bottom-layer name.
#' @param layer Climate depth token (`layer0`) or geomorphologic product stem
#'   (`slope`). Matching ignores case.
#' @param model Climate model name. Matching ignores case.
#' @param experiment Experiment name, such as `"baseline"` or `"ssp126"`.
#' @param period Period token, such as `"2010_2019"`, or `c(start, end)`.
#' @param resolution Unused in most workflows; defaults to `0.05`.
#' @param period_type Unused in most workflows; defaults to `"decadal"`.
#' @param catalog Optional catalog from [marestrata_catalog()]. The package catalog is used when this is omitted.
#' @return A data frame of matching layers, with a `url` column. That column
#'   holds an HTTP URL or a local file path. It is missing until
#'   [marestrata_source()] is set.
#' @export
#' @examples
#' \dontrun{
#' marestrata_source("https://data.example.org/marestrata")
#' marestrata_layers(
#'   variable = "sea_water_temperature",
#'   predictor = "mean",
#'   depth = 0,
#'   model = "ensemble",
#'   experiment = "ssp585",
#'   period = "2090_2099"
#' )
#' marestrata_layers(variable = "geomorphologic_features", layer = "slope")
#' }
marestrata_layers <- function(
    variable = NULL,
    predictor = NULL,
    depth = NULL,
    layer = NULL,
    model = NULL,
    experiment = NULL,
    period = NULL,
    resolution = 0.05,
    period_type = "decadal",
    catalog = NULL
) {
  catalog <- if (is.null(catalog)) marestrata_catalog() else .normalize_catalog(catalog)
  keep <- rep(TRUE, nrow(catalog))

  variable_rows <- .resolve_variable_rows(variable, catalog)
  if (!is.null(variable_rows)) keep <- keep & variable_rows

  predictors <- .resolve_predictors(predictor, catalog)
  if (!is.null(predictors)) keep <- keep & tolower(catalog$predictor) %in% tolower(predictors)

  depths <- .resolve_depth(depth)
  if (!is.null(depths)) {
    depth_keep <- rep(FALSE, nrow(catalog))
    if (length(depths$metres)) {
      depth_keep <- depth_keep | (!is.na(catalog$depth_m) & catalog$depth_m %in% depths$metres)
    }
    if (length(depths$layers)) depth_keep <- depth_keep | catalog$layer %in% depths$layers
    if (isTRUE(depths$bottom)) depth_keep <- depth_keep | grepl("^bottomlayer", catalog$layer)
    keep <- keep & depth_keep
  }

  if (!is.null(layer)) {
    layers <- .match_catalog_tokens(layer, catalog$layer, "layer")
    keep <- keep & catalog$layer %in% layers
  }
  if (!is.null(model)) {
    models <- .match_catalog_tokens(model, catalog$model, "model")
    keep <- keep & catalog$model %in% models
  }
  if (!is.null(experiment)) {
    experiments <- .match_catalog_tokens(experiment, catalog$experiment, "experiment")
    keep <- keep & catalog$experiment %in% experiments
  }
  periods <- .normalize_period(period, catalog$period)
  if (!is.null(periods)) keep <- keep & catalog$period %in% periods

  resolutions <- .resolve_resolution(resolution)
  if (!is.null(resolutions)) {
    resolution_keep <- rep(FALSE, nrow(catalog))
    for (value in resolutions) resolution_keep <- resolution_keep | abs(catalog$resolution - value) < 1e-6
    keep <- keep & resolution_keep
  }
  if (!is.null(period_type)) {
    period_types <- .match_catalog_tokens(period_type, catalog$period_type, "period_type")
    keep <- keep & catalog$period_type %in% period_types
  }

  out <- catalog[keep, , drop = FALSE]
  rownames(out) <- NULL
  base_url <- marestrata_source()
  out$url <- if (is.null(base_url) || !nrow(out)) rep(NA_character_, nrow(out)) else .layer_location(out$path, base_url)
  class(out) <- c("marestrata_layers", "data.frame")
  out
}
