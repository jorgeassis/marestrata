.marestrata <- new.env(parent = emptyenv())

.catalog_columns <- c(
  "path", "resolution", "period_type", "variable", "source_variable", "long_name",
  "standard_name", "units", "folder", "model", "experiment", "period",
  "year_start", "year_end", "layer", "depth_m", "predictor", "file"
)

.clear_session_catalog <- function() {
  if (exists("catalog", envir = .marestrata, inherits = FALSE)) {
    rm("catalog", envir = .marestrata)
  }
  invisible(NULL)
}

.session_catalog <- function() {
  if (!exists("catalog", envir = .marestrata, inherits = FALSE)) return(NULL)
  .marestrata$catalog
}

.store_session_catalog <- function(catalog) {
  .marestrata$catalog <- catalog
  invisible(catalog)
}

.copy_catalog <- function(catalog) {
  out <- as.data.frame(catalog, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

.is_gzip <- function(path) {
  info <- file.info(path)
  if (is.na(info$size) || info$size < 2) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  magic <- readBin(con, "raw", n = 2L)
  length(magic) == 2L && identical(magic, as.raw(c(0x1f, 0x8b)))
}

.read_catalog_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("Catalog file not found: ", path, call. = FALSE)
  }
  con <- if (.is_gzip(path)) gzfile(path, "rt") else file(path, "rt")
  on.exit(close(con), add = TRUE)
  catalog <- utils::read.csv(
    con,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    check.names = FALSE
  )
  .normalize_catalog(catalog)
}

.normalize_catalog <- function(catalog) {
  if (!is.data.frame(catalog)) {
    stop("The catalog must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(.catalog_columns, names(catalog))
  if (length(missing)) {
    stop("Catalog is missing columns: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  catalog <- catalog[.catalog_columns]
  character_columns <- c(
    "path", "period_type", "variable", "source_variable", "long_name",
    "standard_name", "units", "folder", "model", "experiment", "period",
    "layer", "predictor", "file"
  )
  for (column in character_columns) {
    catalog[[column]] <- trimws(as.character(catalog[[column]]))
  }
  catalog$resolution <- as.numeric(catalog$resolution)
  catalog$depth_m <- suppressWarnings(as.numeric(catalog$depth_m))
  catalog$year_start <- suppressWarnings(as.integer(catalog$year_start))
  catalog$year_end <- suppressWarnings(as.integer(catalog$year_end))
  rownames(catalog) <- NULL
  catalog
}

.cache_dir <- function() {
  dir <- tools::R_user_dir("marestrata", which = "cache")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

.catalog_cache_path <- function() file.path(.cache_dir(), "catalog.csv")

.catalog_source_path <- function() file.path(.cache_dir(), "catalog.source")

.user_catalog <- function(base_url) {
  cache <- .catalog_cache_path()
  if (!file.exists(cache)) return(NULL)
  source_file <- .catalog_source_path()
  cached_source <- if (file.exists(source_file)) trimws(readLines(source_file, n = 1L, warn = FALSE)) else ""
  if (length(cached_source) != 1L) cached_source <- ""
  if (!is.null(base_url) && nzchar(cached_source) && !identical(cached_source, base_url)) return(NULL)
  .read_catalog_file(cache)
}

.store_catalog_cache <- function(catalog, source) {
  dest <- .catalog_cache_path()
  connection <- file(dest, "wt")
  on.exit(close(connection), add = TRUE)
  utils::write.csv(catalog, connection, row.names = FALSE, na = "")
  writeLines(source, .catalog_source_path(), useBytes = TRUE)
  invisible(catalog)
}

.fetch_local_catalog <- function(base) {
  candidates <- c(file.path(base, "catalog.csv"), file.path(base, "catalog.csv.gz"))
  for (path in candidates) {
    if (!file.exists(path)) next
    catalog <- tryCatch(.read_catalog_file(path), error = function(e) NULL)
    if (!is.null(catalog)) {
      .store_catalog_cache(catalog, base)
      return(catalog)
    }
  }
  stop(
    "Could not read ", file.path(base, "catalog.csv"),
    ". Place catalog.csv in that directory, or rebuild it with data-raw/build_catalog.R.",
    call. = FALSE
  )
}

.fetch_remote_catalog <- function(base_url) {
  dest <- .catalog_cache_path()
  tmp <- paste0(dest, ".download")
  on.exit(unlink(tmp), add = TRUE)
  urls <- c(paste0(base_url, "/catalog.csv"), paste0(base_url, "/catalog.csv.gz"))
  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)
  catalog <- NULL
  problem <- "download failed"
  for (url in urls) {
    status <- tryCatch(
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE),
      error = function(e) {
        problem <<- conditionMessage(e)
        1L
      }
    )
    if (!identical(as.integer(status), 0L) || !file.exists(tmp) || file.info(tmp)$size < 1) next
    catalog <- tryCatch(
      .read_catalog_file(tmp),
      error = function(e) {
        problem <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(catalog)) break
  }
  if (is.null(catalog)) {
    stop("Could not download the catalog from ", base_url, "/catalog.csv. ", problem, call. = FALSE)
  }
  if (!file.copy(tmp, dest, overwrite = TRUE)) {
    stop("Could not store the downloaded catalog at ", dest, ".", call. = FALSE)
  }
  writeLines(base_url, .catalog_source_path(), useBytes = TRUE)
  catalog
}

.fetch_catalog <- function(source) {
  if (.is_remote_source(source)) .fetch_remote_catalog(source) else .fetch_local_catalog(source)
}

.bundled_catalog <- function() {
  bundled <- system.file("extdata", "catalog.csv.gz", package = "marestrata")
  if (!nzchar(bundled)) stop("The bundled catalog is missing.", call. = FALSE)
  .read_catalog_file(bundled)
}

#' Read the MareStrata layer catalog
#'
#' The catalog has one row per GeoTIFF. A refresh loads
#' `{source}/catalog.csv` (a gzip file is accepted) from the current HTTP
#' domain or local directory into the user cache. Without a refresh, the
#' session copy is returned, then the last cache for this source, then the
#' catalog shipped with the package.
#'
#' @param refresh Logical. When `TRUE`, load the catalog from the current source.
#' @return A data frame with one row per layer.
#' @export
#' @seealso [marestrata_layers()], [marestrata_source()]
marestrata_catalog <- function(refresh = FALSE) {
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop("`refresh` must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(refresh)) {
    base_url <- marestrata_source()
    if (is.null(base_url)) {
      stop(
        "Set the data source with marestrata_source() or MARESTRATA_URL before refreshing the catalog.",
        call. = FALSE
      )
    }
    catalog <- .fetch_catalog(base_url)
    .store_session_catalog(catalog)
    return(.copy_catalog(catalog))
  }
  session <- .session_catalog()
  if (!is.null(session)) return(.copy_catalog(session))
  cached <- .user_catalog(marestrata_source())
  if (!is.null(cached)) {
    .store_session_catalog(cached)
    return(.copy_catalog(cached))
  }
  catalog <- .bundled_catalog()
  .store_session_catalog(catalog)
  .copy_catalog(catalog)
}
