#' Setup Project Environment
#'
#' Loads project settings, creates directories, sets R options and a random seed,
#' and stores the analysis year range for the supplemental protection project.
#'
#' @param year_beg Integer. Beginning year of the analysis (default: 2015).
#' @param year_end Integer. Ending year of the analysis (default: 2024).
#' @param seed Integer. Random seed for reproducibility (default: 1980632).
#'
#' @details
#' This function:
#' \itemize{
#'   \item Detects the operating system and sets a root \code{fastscratch} directory:
#'         \itemize{
#'           \item Windows - \code{"C:/fastscratch"}
#'           \item Linux/macOS - \code{"/fastscratch/<username>"}
#'         }
#'   \item Creates subdirectories for simulation results, expected values, draw-farm,
#'         and draw-cost outputs under \code{fastscratch/HiddenSafetynet2025/output/}.
#'   \item Ensures \code{data-raw/data}, \code{data-raw/output}, and
#'         \code{data-raw/data/agentdata} exist in the project root.
#'   \item Sets R options:
#'         \itemize{
#'           \item \code{scipen = 999} - prefer fixed notation over scientific
#'           \item \code{future.globals.maxSize = 8 * 1024^3} (~8 GiB)
#'           \item \code{dplyr.summarise.inform = FALSE} - suppress summarise messages
#'         }
#'   \item Sets the global RNG seed via \code{set.seed(seed)}.
#' }
#'
#' @return A named list with:
#' \describe{
#'   \item{wd}{List of working directory paths (fastscratch + subfolders).}
#'   \item{year_beg}{The starting year.}
#'   \item{year_end}{The ending year.}
#' }
#'
#' @examples
#' env <- setup_environment(year_beg = 2015, year_end = 2024, seed = 42)
#' env$wd$dir_sim
#' env$year_beg
#'
#' @export
setup_environment <- function(year_beg = 2015, year_end = 2024, seed = 1980632) {

  needed <- c("future.apply", "rfcip", "data.table", "rfcipCalcPass")
  missing <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) {
    stop(sprintf("Missing required packages: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }

  # ---- Validate inputs ----
  stopifnot(is.numeric(year_beg), is.numeric(year_end))
  year_beg <- as.integer(year_beg); year_end <- as.integer(year_end)
  if (year_beg > year_end) stop("`year_beg` must be <= `year_end`.", call. = FALSE)

  # ---- Determine OS + username ----
  sysname <- tolower(as.character(Sys.info()[["sysname"]]))
  user    <- Sys.getenv("USER", unset = Sys.getenv("USERNAME", unset = "unknown"))
  if (identical(user, "") || is.na(user)) user <- "unknown"

  # ---- Root fastscratch directory ----
  fastscratch_root <- if (grepl("windows", sysname)) "C:/fastscratch" else file.path("/fastscratch", user)

  # Ensure root exists (or create)
  if (!dir.exists(fastscratch_root)) dir.create(fastscratch_root, recursive = TRUE, showWarnings = FALSE)

  # ---- Define subdirectories (use file.path to avoid double slashes) ----
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

  # Project-local dirs
  dir.create(file.path("data-raw", "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("data-raw", "output"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("data-raw", "data", "agentdata"), recursive = TRUE, showWarnings = FALSE)

  # ---- Options ----
  options(scipen = 999L)
  options(future.globals.maxSize = 8 * 1024^3)  # bytes (~8 GiB)
  options(dplyr.summarise.inform = FALSE)

  # ---- RNG seed (intentional global side effect) ----
  set.seed(seed)

  # ---- Return environment ----
  list(
    wd = wd,
    year_beg = year_beg,
    year_end = year_end
  )
}
