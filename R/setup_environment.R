#' Setup Project Environment
#'
#' Loads project settings, creates working directories (both under a fast scratch
#' area and in the project), sets useful `options()`, fixes the RNG seed, and
#' stores the analysis year range.
#'
#' @param year_beg Integer. Beginning year of the analysis (default: 2015).
#' @param year_end Integer. Ending year of the analysis (default: 2024).
#' @param seed Integer. Random seed for reproducibility (default: 1980632).
#' @param fastscratch_root Optional character. Root directory where intermediate
#'   files from simulations and estimations will be written for later aggregation.
#'   If `NULL`, it is set automatically based on the operating system:
#'   - Windows: `"C:/fastscratch"`
#'   - Linux/macOS: `"/fastscratch/<username>"`
#'
#' @details
#' Creates these directories (if absent):
#' \itemize{
#'   \item Fast scratch tree (for large, intermediate outputs):
#'     \code{<fastscratch_root>/HiddenSafetynet2025/output/} with subfolders
#'     \code{sims}, \code{expected}, \code{draw_farm}, \code{draw_cost}.
#'   \item Project-local (for smaller, version-controlled artifacts):
#'     \code{data/}, \code{data/output/}, \code{data/cleaned_agents_data/}.
#' }
#'
#' Sets:
#' \itemize{
#'   \item \code{options(scipen = 999)}
#'   \item \code{options(future.globals.maxSize = 8 * 1024^3)}  (= 8 GiB)
#'   \item \code{options(dplyr.summarise.inform = FALSE)}
#'   \item \code{set.seed(seed)}
#' }
#'
#' Requires the packages \pkg{future.apply}, \pkg{rfcip}, \pkg{data.table},
#' and \pkg{rfcipCalcPass}.
#'
#' @return A list with:
#' \describe{
#'   \item{wd}{Named list of working directories (fastscratch root and subfolders).}
#'   \item{year_beg}{Starting year (integer).}
#'   \item{year_end}{Ending year (integer).}
#' }
#'
#' @export
setup_environment <- function(
    year_beg = 2015, year_end = 2024, seed = 1980632,
    fastscratch_root = NULL) {

  # ---- Package availability check ----
  needed <- c("future.apply", "rfcip", "data.table")
  missing <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) {
    stop(sprintf("Missing required packages: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }

  # ---- Validate inputs ----
  stopifnot(is.numeric(year_beg), is.numeric(year_end), is.numeric(seed), length(seed) == 1)
  year_beg <- as.integer(year_beg); year_end <- as.integer(year_end); seed <- as.integer(seed)
  if (year_beg > year_end) stop("`year_beg` must be <= `year_end`.", call. = FALSE)

  # ---- Determine OS + username ----
  if (is.null(fastscratch_root)) {
    sysname <- tolower(as.character(Sys.info()[["sysname"]]))
    user    <- Sys.getenv("USER", unset = Sys.getenv("USERNAME", unset = "unknown"))
    if (identical(user, "") || is.na(user)) user <- "unknown"

    fastscratch_root <- if (grepl("windows", sysname)) "C:/fastscratch" else file.path("/fastscratch", user)
  }

  # Ensure root exists (or create)
  if (!dir.exists(fastscratch_root)) dir.create(fastscratch_root, recursive = TRUE, showWarnings = FALSE)

  # ---- Define subdirectories ----
  project_root <- file.path(fastscratch_root, "HiddenSafetynet2025")
  wd <- list(
    fastscratch  = fastscratch_root,
    dir_sim      = file.path(project_root, "output", "sims"),
    dir_expected = file.path(project_root, "output", "expected"),
    dir_drawfarm = file.path(project_root, "output", "draw_farm"),
    dir_drawcost = file.path(project_root, "output", "draw_cost")
  )

  # ---- Create directories ----
  invisible(lapply(wd, function(p) dir.create(p, recursive = TRUE, showWarnings = FALSE)))

  # Project-local dirs (standardized under data/)
  dir.create("data", recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("output"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("data", "cleaned_agents_data"), recursive = TRUE, showWarnings = FALSE)

  # ---- Options ----
  options(scipen = 999L)
  options(future.globals.maxSize = 8 * 1024^3)  # bytes (~8 GiB)
  options(dplyr.summarise.inform = FALSE)

  # ---- RNG seed ----
  set.seed(seed)

  # ---- Return environment ----
  list(
    wd = wd,
    year_beg = year_beg,
    year_end = year_end
  )
}
