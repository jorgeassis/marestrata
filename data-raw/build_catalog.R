# Build the MareStrata layer catalog.
#
# By default this unpacks viewer/catalog.json. Set MARESTRATA_ROOT to a data
# volume to scan MareStrata folders instead. Local volumes that still use the
# previous folder names are scanned, but catalog paths are written as
# MareStrata 005 / MareStrata 025.
#
# Writes:
#   marestrata/inst/extdata/catalog.csv.gz
#   marestrata/data-raw/catalog.csv

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  "marestrata/data-raw"
}
pkg_dir <- normalizePath(file.path(script_dir, ".."))
repo_dir <- normalizePath(file.path(pkg_dir, ".."))

variables_path <- file.path(repo_dir, "Dependencies", "listVariables.csv")
if (!file.exists(variables_path)) stop("Variable table not found: ", variables_path)

read_variables <- function(path) {
  variables <- utils::read.csv(path, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
  variables[] <- lapply(variables, function(column) if (is.character(column)) trimws(column) else column)
  variables
}

known_variables <- function(variables) {
  names <- trimws(variables$name)
  names <- names[nzchar(names)]
  # An environment returns NULL for a missing name. A named character
  # vector throws "subscript out of bounds" with [[.
  list2env(
    as.list(stats::setNames(names, tolower(names))),
    parent = emptyenv()
  )
}

.split_lookup <- function(known, candidate) {
  value <- known[[candidate]]
  if (is.null(value) || !nzchar(value)) return(NULL)
  value
}

split_model_variable <- function(tokens, known) {
  if (length(tokens) < 2L) return(NULL)
  for (size in seq.int(length(tokens) - 1L, 1L)) {
    start <- length(tokens) - size + 1L
    candidate <- tolower(paste(tokens[start:length(tokens)], collapse = "_"))
    canonical <- .split_lookup(known, candidate)
    if (is.null(canonical)) next
    model <- paste(tokens[seq_len(length(tokens) - size)], collapse = "_")
    if (nzchar(model)) return(list(model = model, variable = canonical))
  }
  model <- paste(tokens[-length(tokens)], collapse = "_")
  variable <- tokens[length(tokens)]
  if (!nzchar(model) || !nzchar(variable)) return(NULL)
  list(model = model, variable = variable)
}

parse_clim_filename <- function(filename, known) {
  stem <- basename(filename)
  original <- stem
  while (grepl("\\.tif$", stem, ignore.case = TRUE)) stem <- substr(stem, 1L, nchar(stem) - 4L)
  if (identical(stem, original)) return(NULL)
  parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
  if (length(parts) < 6L) return(NULL)
  if (!parts[1] %in% c("clim", "anom")) return(NULL)
  proj <- parts[length(parts)]
  if (!proj %in% c("wgs84", "moll")) return(NULL)
  predictor <- parts[length(parts) - 1L]
  layer <- parts[length(parts) - 2L]
  if (!grepl("^layer[0-9]+$", layer) && !grepl("^bottomlayer(min|mean|max)$", layer)) return(NULL)
  rest <- parts[2:(length(parts) - 3L)]
  if (length(rest) < 2L) return(NULL)
  n <- length(rest)
  if (grepl("^[0-9]{4}$", rest[n]) && grepl("^[0-9]{4}$", rest[n - 1L])) {
    y0 <- rest[n - 1L]
    y1 <- rest[n]
    head <- if (n > 2L) rest[seq_len(n - 2L)] else character()
  } else if (grepl("^[0-9]{4}$", rest[n])) {
    y0 <- rest[n]
    y1 <- NA_character_
    head <- if (n > 1L) rest[seq_len(n - 1L)] else character()
  } else {
    return(NULL)
  }
  if (length(head) < 3L) return(NULL)
  experiment <- head[length(head)]
  identity <- split_model_variable(head[-length(head)], known)
  if (is.null(identity) || !nzchar(experiment)) return(NULL)
  variable <- identity$variable
  if (tolower(variable) == "par" && grepl("^bottomlayer(min|mean|max)$", layer)) {
    canonical <- .split_lookup(known, "parbottom")
    if (!is.null(canonical)) variable <- canonical
  }
  list(
    prefix = parts[1],
    model = identity$model,
    variable = variable,
    experiment = experiment,
    period = if (is.na(y1)) y0 else paste0(y0, "_", y1),
    layer = layer,
    predictor = predictor,
    proj = proj
  )
}

resolution_number <- function(code) {
  ifelse(code %in% c("005", "5", "0.05"), 0.05, ifelse(code %in% c("025", "25", "0.25"), 0.25, NA_real_))
}

.product_dir <- function(resolution_code) {
  sprintf("MareStrata %s", resolution_code)
}

.scan_product_dir <- function(root, resolution_code) {
  candidates <- c(
    .product_dir(resolution_code),
    sprintf("BLUE-ARRAY %s", resolution_code)
  )
  for (name in candidates) {
    if (dir.exists(file.path(root, name))) return(name)
  }
  NULL
}

empty_records <- function() {
  data.frame(
    resolution_code = character(),
    period_type = character(),
    folder = character(),
    file = character(),
    variable = character(),
    model = character(),
    experiment = character(),
    period = character(),
    layer = character(),
    predictor = character(),
    stringsAsFactors = FALSE
  )
}

records_from_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Install jsonlite to unpack viewer/catalog.json.")
  }
  packed <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  fields <- packed$fields
  records <- packed$records
  strings <- packed$strings
  if (!is.matrix(records)) stop("viewer/catalog.json records are not a matrix.")
  decoded <- strings[records + 1L]
  dim(decoded) <- dim(records)
  table <- as.data.frame(decoded, stringsAsFactors = FALSE)
  names(table) <- fields
  data.frame(
    resolution_code = table$resolution,
    period_type = table$periodType,
    folder = table$folder,
    file = table$file,
    variable = table$variable,
    model = table$model,
    experiment = table$experiment,
    period = table$period,
    layer = table$layer,
    predictor = table$predictor,
    stringsAsFactors = FALSE
  )
}

records_from_disk <- function(root, known) {
  resolutions <- c("005", "025")
  period_types <- c("decadal", "annual")
  rows <- list()
  for (resolution in resolutions) {
    product <- .scan_product_dir(root, resolution)
    if (is.null(product)) next
    for (period_type in period_types) {
      directory <- file.path(root, product, period_type)
      if (!dir.exists(directory)) next
      folders <- list.dirs(directory, full.names = FALSE, recursive = FALSE)
      folders <- folders[!startsWith(folders, ".")]
      for (folder in folders) {
        files <- list.files(file.path(directory, folder), pattern = "\\.tif$", ignore.case = TRUE)
        for (file in files) {
          parsed <- parse_clim_filename(file, known)
          if (is.null(parsed) || parsed$prefix != "clim" || parsed$proj != "wgs84") next
          rows[[length(rows) + 1L]] <- data.frame(
            resolution_code = resolution,
            period_type = period_type,
            folder = folder,
            file = file,
            variable = parsed$variable,
            model = parsed$model,
            experiment = parsed$experiment,
            period = parsed$period,
            layer = parsed$layer,
            predictor = parsed$predictor,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (!length(rows)) return(empty_records())
  do.call(rbind, rows)
}

assemble_catalog <- function(records, variables) {
  if (!nrow(records)) stop("No layers were found for the catalog.")
  index <- match(tolower(records$variable), tolower(variables$name))
  missing <- sort(unique(records$variable[is.na(index)]))
  if (length(missing)) {
    message("Variables not in listVariables.csv: ", paste(missing, collapse = ", "))
  }
  period_parts <- strsplit(records$period, "_", fixed = TRUE)
  year_start <- as.integer(vapply(period_parts, function(parts) parts[1], character(1)))
  year_end <- as.integer(vapply(period_parts, function(parts) {
    if (length(parts) >= 2L) parts[2] else NA_character_
  }, character(1)))
  depth_m <- rep(NA_integer_, nrow(records))
  is_depth <- grepl("^layer[0-9]+$", records$layer)
  depth_m[is_depth] <- as.integer(sub("^layer", "", records$layer[is_depth]))
  # Package variable names are the on-disk folder names (sea_water_temperature).
  # The short CF token from the filename is kept as source_variable (thetao).
  catalog <- data.frame(
    path = paste(.product_dir(records$resolution_code), records$period_type, records$folder, records$file, sep = "/"),
    resolution = resolution_number(records$resolution_code),
    period_type = records$period_type,
    variable = records$folder,
    source_variable = records$variable,
    long_name = variables$longName[index],
    standard_name = variables$standardName[index],
    units = variables$units[index],
    folder = records$folder,
    model = records$model,
    experiment = records$experiment,
    period = records$period,
    year_start = year_start,
    year_end = year_end,
    layer = records$layer,
    depth_m = depth_m,
    predictor = records$predictor,
    file = records$file,
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(catalog$path)) {
    message("Dropping ", sum(duplicated(catalog$path)), " duplicate paths.")
    catalog <- catalog[!duplicated(catalog$path), , drop = FALSE]
  }
  predictor_order <- c("min", "ltmin", "mean", "ltmax", "max", "range")
  predictor_rank <- match(catalog$predictor, predictor_order)
  predictor_rank[is.na(predictor_rank)] <- length(predictor_order) + 1L
  bottom_rank <- match(catalog$layer, c("bottomlayermin", "bottomlayermean", "bottomlayermax"))
  depth_rank <- ifelse(grepl("^layer[0-9]+$", catalog$layer), catalog$depth_m, 1000000 + ifelse(is.na(bottom_rank), 9L, bottom_rank))
  period_rank <- match(catalog$period_type, c("decadal", "annual"))
  catalog <- catalog[order(catalog$variable, catalog$resolution, period_rank, catalog$model, catalog$experiment, catalog$period, depth_rank, predictor_rank, catalog$path), , drop = FALSE]
  rownames(catalog) <- NULL
  catalog
}

write_catalog <- function(catalog, pkg_dir) {
  gz_path <- file.path(pkg_dir, "inst", "extdata", "catalog.csv.gz")
  csv_path <- file.path(pkg_dir, "data-raw", "catalog.csv")
  dir.create(dirname(gz_path), recursive = TRUE, showWarnings = FALSE)
  write_one <- function(path, compress) {
    connection <- if (compress) gzfile(path, "wt") else file(path, "wt")
    on.exit(close(connection), add = TRUE)
    utils::write.csv(catalog, connection, row.names = FALSE, na = "")
  }
  write_one(csv_path, compress = FALSE)
  write_one(gz_path, compress = TRUE)
  message("Wrote ", nrow(catalog), " layers")
  message(csv_path)
  message(gz_path)
}

variables <- read_variables(variables_path)
data_root <- Sys.getenv("MARESTRATA_ROOT", unset = "")
if (nzchar(data_root)) {
  if (!dir.exists(data_root)) stop("MARESTRATA_ROOT is not a directory: ", data_root)
  message("Scanning ", data_root)
  records <- records_from_disk(data_root, known_variables(variables))
} else {
  json_path <- file.path(repo_dir, "viewer", "catalog.json")
  if (!file.exists(json_path)) stop("Catalog not found: ", json_path)
  message("Reading ", json_path)
  records <- records_from_json(json_path)
}

write_catalog(assemble_catalog(records, variables), pkg_dir)
