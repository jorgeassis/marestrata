fixture_catalog <- function() {
  marestrata:::.read_catalog_file(testthat::test_path("fixtures", "catalog.csv"))
}

without_source <- function(code) {
  old_option <- getOption("marestrata.base_url")
  old_env <- Sys.getenv("MARESTRATA_URL", unset = NA_character_)
  options(marestrata.base_url = NULL)
  Sys.unsetenv("MARESTRATA_URL")
  on.exit({
    options(marestrata.base_url = old_option)
    if (is.na(old_env)) Sys.unsetenv("MARESTRATA_URL") else Sys.setenv(MARESTRATA_URL = old_env)
  }, add = TRUE)
  force(code)
}
