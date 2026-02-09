# R/00_config.R
# Configuração: pacotes, opções survey e caminhos reprodutíveis
suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(readr)
  library(stringr)
  library(survey)
  library(srvyr)
  library(ggplot2)
})

options(survey.lonely.psu = "adjust")  # análogo a singleunit(centered) no Stata
theme_set(theme_bw())

dir.create(here("data"), showWarnings = FALSE)
dir.create(here("data", "raw"), showWarnings = FALSE)
dir.create(here("data", "derived"), showWarnings = FALSE)
dir.create(here("outputs"), showWarnings = FALSE)
dir.create(here("outputs", "tables"), showWarnings = FALSE)
dir.create(here("outputs", "figures"), showWarnings = FALSE)

# >>>>>>> AJUSTE AQUI (apenas uma vez) <<<<<<<
# Coloque os seus arquivos brutos (RData) em data/raw/
PNS2013_RAW_RDATA <- here("data", "raw", "pns_2013.Rdata")
PNS2019_RAW_RDATA <- here("data", "raw", "pns_2019.rdata")

# Se você quiser replicar exatamente a reescala antiga de pesos (por constante),
# ligue esta opção. Para publicação, prefira manter o peso original.
USE_LEGACY_WEIGHT_SCALING <- FALSE
