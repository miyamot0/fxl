oldwd <- getwd()

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

if (require("here")) {
  require(here)
  setwd(paste(here("coverage")))

  if (require("covr")) {
    require(covr)

    report(
      file = "fxl-coverage-report.html"
    )
  }
}

setwd(oldwd)
