if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

files_in_dir <- list.files(
  path = ".",
  pattern = "*.R",
  full.names = FALSE,
  ignore.case = FALSE
)

files_in_dir <- files_in_dir[files_in_dir != "run_all_demos.R"]
files_in_dir <- files_in_dir[files_in_dir != "beezdemandInd.R"]
files_in_dir <- files_in_dir[files_in_dir != "atd_concurrentoperant.R"]

lapply(files_in_dir, function(x) try(source(x)))

