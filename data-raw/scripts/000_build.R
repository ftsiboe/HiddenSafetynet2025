
rm(list=ls(all=TRUE));gc()

library(data.table);library(rfcip);library(rfcipCalcPass)

unlink(c("NAMESPACE",list.files("./data", full.names = TRUE),list.files("./man", full.names = TRUE)))

devtools::document()

for(i in list.files("R",full.names = T)){
  print(paste0("********************",i,"********************"))
  tools::showNonASCIIfile(i)
}

devtools::check_man()

devtools::build_manual(path = getwd())

#devtools::test()

#devtools::check()

study_env <- setup_environment()

clean_rma_sobtpu()
