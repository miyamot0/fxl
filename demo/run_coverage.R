detach_package <- function(pkg, character.only = FALSE) {
  if (!character.only) {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while (search_item %in% search()) {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

detach_package(fxl)

if ("here" %in% installed.packages()) {
  require(here)
  setwd(paste(here("coverage")))

  if ("covr" %in% installed.packages()) {
    require(covr)

    report(
      file = "fxl-coverage-report.html"
    )
  }
}
