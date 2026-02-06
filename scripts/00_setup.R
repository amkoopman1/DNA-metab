#----------------------------01 Restore environment ---------------------------------
rm(list=ls()) # clear working memory
if (!requireNamespace("renv", quietly = TRUE)) {install.packages("renv")}
library(renv) # load renv library
renv::restore() # restore the packages in the renv.lock file

#----------------------------02 Load libraries ---------------------------------
suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
  library(purrr)
  library(tidyverse)
  library(patchwork)
  library(httpuv)
})

#---------------------------03 Define user functions -------------------------------

#### 1) Function read_gsdb to read specific sheets from a gsheets database or the whole database
# 
# Usage examples : 
#
# 1 - Read all sheets in a database, no filter, all sheets are put in a list object (default)
# database_link <- "https://docs.google.com/spreadsheets/d/<SHEET_ID>"
# database <- read_gsdb(database_link)
# names(database)
# 
# 2- Read two specific sheets:
# subset <- read_gsdb(database, sheets = c("Species", "Transects"))
# 
# 3 - Read three specific sheets, with a specific filter for each sheet
# filtered <- read_gsdb(database_link,sheets = c("Species", "Transects", "Observations"),
#    filter = list(
#                  Species      = ~ !is.na(Species_ID),
#                  Transects    = ~ Region == "Loita",
#                  Observations = ~ cover > 0))


library(googlesheets4)
library(purrr)

read_gsdb <- function(database, sheets = NULL, separate = FALSE, verbose = TRUE) {
  if (is.null(sheets)) {
    sheets <- googlesheets4::sheet_names(database)
  }
  
  n <- length(sheets)
  start_time <- Sys.time()
  
  # always print duration at the end
  on.exit({
    elapsed <- as.integer(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("\nDone in %d:%02d (mm:ss)\n", elapsed %/% 60, elapsed %% 60))
  }, add = TRUE)
  
  if (verbose) {
    cat(sprintf("Reading %d sheet%s...\n", n, if (n == 1) "" else "s"))
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    on.exit(close(pb), add = TRUE)
  }
  
  res <- vector("list", n)
  names(res) <- sheets
  
  for (i in seq_along(sheets)) {
    sh <- sheets[i]
    if (verbose) {
      utils::setTxtProgressBar(pb, i - 1)
      cat(sprintf("\r[%d/%d] %s ...", i, n, sh))
      flush.console()
    }
    res[[i]] <- googlesheets4::read_sheet(database, sheet = sh)
    if (verbose) utils::setTxtProgressBar(pb, i)
  }
  
  if (separate) {
    list2env(res, envir = .GlobalEnv)
    invisible(res)
  } else {
    res
  }
}


## --- Google Sheets auth (googlesheets4) ----------------------------
suppressPackageStartupMessages(library(googlesheets4))

# Helper you can call from any script after sourcing 00-setup.R:
gsheets_auth <- function(
    mode = c("auto", "public", "user", "service"),
    email = Sys.getenv("GS_EMAIL", ""),                     # for user OAuth
    sa_json = Sys.getenv("GS_SERVICE_ACCOUNT_JSON", ""),    # for service account
    cache = Sys.getenv("GARGLE_OAUTH_CACHE", "~/.R/gargle"),
    verbose = TRUE
) {
  mode <- match.arg(mode)
  
  # Where to cache OAuth tokens (user mode). Create the folder if needed.
  cache <- path.expand(cache)
  if (!dir.exists(cache)) dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  options(gargle_oauth_cache = cache)
  
  if (mode == "public") {
    googlesheets4::gs4_deauth()
    if (verbose) message("gs4: deauthed (public read-only).")
    return(invisible(TRUE))
  }
  
  if (mode == "service" || (mode == "auto" && nzchar(sa_json))) {
    sa_json <- path.expand(sa_json)
    if (!file.exists(sa_json)) stop("GS_SERVICE_ACCOUNT_JSON not found: ", sa_json)
    googlesheets4::gs4_auth(path = sa_json)
  } else if (mode == "user" || (mode == "auto" && nzchar(email))) {
    googlesheets4::gs4_auth(email = email)   # if email="", Google will prompt interactively
  } else if (interactive()) {
    # No env vars set; interactively pick an account in your browser
    googlesheets4::gs4_auth()
  } else {
    stop("No credentials available. Set GS_SERVICE_ACCOUNT_JSON or GS_EMAIL, ",
         "or call gsheets_auth(mode = 'public') for public sheets.")
  }
  
  if (verbose) {
    message("gs4: ", paste(capture.output(googlesheets4::gs4_user()), collapse = " "),
            " | cache: ", normalizePath(cache, winslash = "/"))
  }
  invisible(TRUE)
}

