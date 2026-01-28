library(targets)

tar_option_set(
  packages = c("dplyr", "tibble")
)

# Carrega o smoke pipeline que você já criou
source("R/targets/pipeline_smoke.R")

list(
  tar_smoke_targets()
)
