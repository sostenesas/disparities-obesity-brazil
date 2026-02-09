# run_all.R
# Pipeline completo (R-only) para PNS 2013/2019 com desenho amostral
source("R/R/00_config", encoding = "UTF-8")
source("R/R/01_prepare_pns2013", encoding = "UTF-8")
source("R/R/02_prepare_pns2019", encoding = "UTF-8")
source("R/R/03_build_analytic", encoding = "UTF-8")
source("R/R/04_analysis_means", encoding = "UTF-8")
source("R/R/05_analysis_fx_imc_composition", encoding = "UTF-8")
source("R/R/06_analysis_prevalence", encoding = "UTF-8")
source("R/R/07_models", encoding = "UTF-8")
source("R/R/08_marginal_effects", encoding = "UTF-8")
source("R/R/09_tables_for_paper", encoding = "UTF-8")
source("R/R/10_strobe_flow", encoding = "UTF-8")
message("(ok) Pipeline concluído.")