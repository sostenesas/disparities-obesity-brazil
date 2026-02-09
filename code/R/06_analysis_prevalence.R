# R/06_analysis_prevalence.R
source("R/00_config.R")

df <- readRDS(here("data","derived","pns_2013_2019_analytic.rds"))

des <- svydesign(
  id = ~psu,
  strata = ~strata,
  weights = ~weight_norm,
  data = df,
  nest = TRUE
) %>% srvyr::as_survey_design()

# Prevalência de sobrepeso/obesidade (>=25) e obesidade (>=30)
prev_main <- des %>%
  group_by(ano) %>%
  summarize(
    prev_exc_peso = survey_mean(exc_peso, vartype=c("se","ci"), na.rm=TRUE),
    prev_obesity  = survey_mean(obesity,  vartype=c("se","ci"), na.rm=TRUE),
    .groups="drop"
  )

write_csv(prev_main, here("outputs","tables","prev_main_by_year.csv"))

# Agora CORRIGINDO o seu gráfico/região: nada de weighted.mean().
prev_region <- des %>%
  group_by(ano, regiao, sit_cens) %>%
  summarize(
    prev_exc_peso = survey_mean(exc_peso, vartype=c("ci"), na.rm=TRUE),
    .groups="drop"
  )

write_csv(prev_region, here("outputs","tables","prev_exc_peso_by_region_sitcens.csv"))

p_reg <- ggplot(prev_region, aes(x=reorder(regiao, prev_exc_peso), y=prev_exc_peso, group=factor(ano))) +
  geom_point() +
  geom_line() +
  coord_flip() +
  facet_wrap(~sit_cens) +
  labs(x="Região", y="Prevalência (>=25) - ponderada", title="Prevalência de excesso de peso por região e situação censitária") +
  theme(legend.position="bottom")

ggsave(here("outputs","figures","fig_prev_region_sitcens.png"), p_reg, width=9, height=5, dpi=300)
message("✅ Prevalências (survey) prontas.")
