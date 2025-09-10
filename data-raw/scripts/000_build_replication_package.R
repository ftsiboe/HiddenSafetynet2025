# Hard reset of workspace
rm(list = ls(all = TRUE)); gc()

# Clean generated artifacts
unlink(c(
  "NAMESPACE",
  #list.files("./data", full.names = TRUE),
  list.files("./man",  full.names = TRUE)
))

for(i in c("setup_environment","clean_rma_sobtpu","clean_supplemental_plan_shares","ers_theme")){
  download.file(
    paste0("https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/refs/heads/main/R/",i,".R"),
    paste0("./R/",i,".R"), mode = "wb", quiet = TRUE)
}

# Sanity pass through R/ sources: shows any non-ASCII characters per file
for (i in list.files("R", full.names = TRUE)) {
  print(paste0("********************", i, "********************"))
  tools::showNonASCIIfile(i)
}

# Rebuild documentation from roxygen comments
devtools::document()

# Check man pages only (faster than full devtools::check)
devtools::check_man()

# Build PDF manual into the current working directory
devtools::build_manual(path = getwd())

# Optional: run tests / full package check (uncomment when needed)
devtools::test()
devtools::check()
