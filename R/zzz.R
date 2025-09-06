.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

}




