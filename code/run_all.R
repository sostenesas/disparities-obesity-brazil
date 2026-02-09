# run_all.R
# Pipeline completo (R-only) para PNS 2013/2019 com desenho amostral
source("R/00_config.R")
source("R/01_prepare_pns2013.R")
source("R/02_prepare_pns2019.R")
source("R/03_build_analytic.R")
source("R/04_analysis_means.R")
source("R/05_analysis_fx_imc_composition.R")
source("R/06_analysis_prevalence.R")
message("(ok) Pipeline concluído.")
