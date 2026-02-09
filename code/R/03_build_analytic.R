# R/03_build_analytic.R
source("R/00_config.R")

df13 <- readRDS(here("data","derived","pns2013_clean.rds"))
df19 <- readRDS(here("data","derived","pns2019_clean.rds"))

df <- bind_rows(df13, df19)

# Normalização opcional (não muda estimativas, só escala numérica)
df <- df %>%
  mutate(weight_norm = weight / mean(weight, na.rm = TRUE))

saveRDS(df, here("data","derived","pns_2013_2019_analytic.rds"))

# Auditorias mínimas que um revisor espera
audit <- list(
  n_total = nrow(df),
  n_by_year = df %>% count(ano),
  missing_key = df %>% summarize(
    missing_psu = sum(is.na(psu)),
    missing_strata = sum(is.na(strata)),
    missing_weight = sum(is.na(weight)),
    missing_imc = sum(is.na(IMC))
  ),
  imc_summary = df %>% summarize(
    p1 = quantile(IMC, 0.01, na.rm=TRUE),
    p50 = quantile(IMC, 0.50, na.rm=TRUE),
    p99 = quantile(IMC, 0.99, na.rm=TRUE)
  )
)

write_lines(capture.output(str(audit, max.level = 3)),
            here("outputs","tables","audit_pipeline.txt"))
message("✅ Analytic dataset + auditoria: data/derived/pns_2013_2019_analytic.rds")
