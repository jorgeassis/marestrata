#' Set or return the MareStrata data source
#'
#' Layers are read from this location. Pass an `http://` or `https://` URL, or
#' a local directory that contains the `BLUE-ARRAY …` folders. The catalog at
#' the source is `{source}/catalog.csv`. The environment variable
#' `MARESTRATA_URL` is used when no source has been set in the session.
#'
#' @param base_url Character URL or local directory. Omit it to return the
#'   source currently in use.
#' @return The source, without a trailing slash, or `NULL` when none is set.
#' @export
#' @examples
#' marestrata_source("https://data.example.org/marestrata")
#' \dontrun{
#' marestrata_source("/Volumes/JellyFish")
#' }
#' marestrata_source()
marestrata_source <- function(base_url = NULL) {
  if (!is.null(base_url)) {
    base_url <- .normalize_source(base_url)
    options(marestrata.base_url = base_url)
    .clear_session_catalog()
  }
  current <- getOption("marestrata.base_url", default = NULL)
  if (is.null(current) || !is.character(current) || length(current) != 1L || !nzchar(current)) {
    env <- Sys.getenv("MARESTRATA_URL", unset = "")
    if (!nzchar(env)) return(NULL)
    return(.normalize_source(env))
  }
  current
}

.is_remote_source <- function(source) {
  is.character(source) && length(source) == 1L && grepl("^https?://", source, ignore.case = TRUE)
}

.normalize_source <- function(base_url) {
  if (!is.character(base_url) || length(base_url) != 1L || !nzchar(trimws(base_url))) {
    stop("`base_url` must be a single URL or local directory.", call. = FALSE)
  }
  base_url <- trimws(base_url)
  if (.is_remote_source(base_url)) {
    return(sub("/+$", "", base_url))
  }
  if (grepl("^file://", base_url, ignore.case = TRUE)) {
    base_url <- sub("^file://", "", base_url, ignore.case = TRUE)
  }
  if (!dir.exists(base_url)) {
    stop(
      "`base_url` must be an existing directory, or start with http:// or https://. Got: ",
      base_url,
      call. = FALSE
    )
  }
  normalizePath(base_url, winslash = "/", mustWork = TRUE)
}
