ig <- function(pkg) {
  try(devtools::install_github(pkg, threads = parallel::detectCores()))
}
pkgs <- c("DavisVaughan/furrr", "Ironholds/humaniformat", "r-lib/fs",
  "r-lib/memoise", "r-lib/rlang", "ropensci/europepmc", "tidyverse/readxl",
  "tidyverse/tidyverse", "tidyverse/lubridate")
ig(pkgs)
install.packages("64bits")
update.packages(Ncpus = parallel::detectCores())
