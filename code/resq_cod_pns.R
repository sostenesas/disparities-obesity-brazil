load("pns_2013_2019.RData")

# Carregar pacotes necessários
library(survey)
library(dplyr)
library(ggplot2)
library(tidyr)
library(MatchIt)

# Objeto de desenho:
desPNSW13_19 = svydesign(id = ~UPA_PNS, strat = ~V0024, 
                         weight = ~peso_morador_selec, nest = TRUE, data = df)

# Atualiza o objeto de desenho para incluir:
# - o log da renda domiciliar (log_rend_dom)
# - a categorização do log em quintis (cat_log_rend)
# - uma nova variável "over" que é 1 se fx_imc for "sobrepeso" ou "obesidade", 0 caso contrário
desPNSW13_19 <- update(desPNSW13_19,
                       log_rend_dom = log(rend_dom),
                       cat_log_rend = cut(log(rend_dom),
                                          breaks = quantile(log(rend_dom), probs = seq(0, 1, by = 0.2), na.rm = TRUE),
                                          include.lowest = TRUE,
                                          labels = c("1º Quintil", "2º Quintil", "3º Quintil", "4º Quintil", "5º Quintil")
                       ),
                       over = ifelse(fx_imc %in% c("sobrepeso", "obesidade"), 1, 0)
)

# Agora, usamos svyby() para calcular, para cada quintil do log da renda (cat_log_rend),
# o valor médio (ponderado) de log_rend_dom e a prevalência (ponderada) de sobrepeso/obesidade (over).
aggregated <- svyby(
  formula = ~ over + rend_dom,
  by = ~ sexo + fx_esc + cor,
  design = desPNSW13_19,
  FUN = svymean,
  na.rm = TRUE
)

# Converte o resultado para data frame
df_agg <- as.data.frame(aggregated)
rm(aggregated)
gc()
# Visualiza o data frame resultante
print(df_agg)

# Extrair os dados do objeto de desenho para um data frame
df_weighted <- as.data.frame(desPNSW13_19$variables)

ggplot(df_weighted, aes(x = log_rend_dom, y = IMC)) +
  geom_point(aes(size = peso_morador_selec), alpha = 0.6) +
  labs(x = "Log da renda domiciliar",
       y = "IMC",
       color = "Faixa de IMC",
       size = "Peso Amostral") +
  theme_bw()+
  facet_wrap(vars(ano))

ggplot(df_agg, aes(x = rend_dom,
                          y = over)) +
  geom_point(alpha = 0.6) +
  geom_line()+
  labs(x = "Renda domiciliar",
       y = "Prevalência",
       color = "Ano") +
  theme_bw()+
  facet_wrap(vars(sexo))

# Calcular a prevalência ponderada de sobrepeso/obesidade por ano e região
df_prevalence <- df_weighted %>%
  group_by(ano, regiao, sit_cens) %>%
  summarise(prevalence = weighted.mean(over, w = peso_morador_selec), .groups = "drop")

# Criando o gráfico de pirulito
ggplot(df_prevalence, aes(x = reorder(regiao, prevalence),
                          y = prevalence, color = factor(ano), group = regiao)) +
  geom_line(linewidth = 1) +  # Linha do pirulito
  geom_point(size = 4) +  # Ponto do pirulito
  scale_color_manual(values = c("2013" = "darkgreen", "2019" = "gold")) +  # Cores do ano
  labs(
    title = "",
    x = "Região",
    y = "Prevalência (%)",
    color = "Ano"
  ) +
  theme_bw() +
  coord_flip()+
  facet_grid(vars(sit_cens))

ggplot(df2_tab) +
  aes(x = IMC, y = idade, colour = fx_imc) +
  geom_point(size = 0.5) +
  scale_color_brewer(palette = "BrBG", direction = 1) +
  labs(x = "IMC", y = "Idade", colour = "Faixa de IMC") +
  theme_bw() +
  facet_wrap(vars(ano))

df9

df10 <- df %>%
  filter(!is.na(IMC)) %>%
  ggplot() +
  aes(x = IMC, y = rend_dom, colour = fx_imc) +
  geom_point(size = 0.5) +
  scale_color_brewer(palette = "BrBG", direction = 1) +
  labs(x = "IMC", y = "Renda domiciliar", colour = "Faixa de IMC") +
  theme_bw() +
  facet_wrap(vars(ano, sexo, est_civil, sit_cens,))

df10

options(survey.lonely.psu = "adjust")

dataset15 <- dataset%>%
  dplyr::select(uf,exc_peso)

dataset15 <- dataset%>%arrange(mean(dataset$exc_peso), decreasing = T)

dataset15 <- dataset %>%
  filter(área %in% c("Rural")) %>%
  filter(!is.na(exc_peso))%>%
  ggplot(aes(x = uf, y = exc_peso))+
  geom_bar(stat = "summary",fun = "mean", fill = "#56a780")+
  labs(x = "Estado", y = "Percentual de excesso de peso", 
       title = "Prevalência de excesso de peso por estado", colour = "#34a689")+
  geom_errorbar(stat = "summary",
                fun.data = "mean_ci",
                width = 0.3)+
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(área))

dataset15


df <- df %>% 
  mutate(fx_imc_num = ifelse(fx_imc == "Sim", 1, 0))


modeloLog <- svyglm(
  formula = fx_imc_num ~ regiao +sexo + homem + urbano + idade + branco +
    fx_esc + solteiro + rend_per_capita + rend_dom + total_morador + internet + 
    diabetes + h_arterial + col_alto + AVC + depr_ + atv_fis + 
    min_atv_fis + p_saude +fumo, 
  design  = desPNSW13_19, 
  family  = "binomial",
  control = list(maxit = 100)  # aumentamos o número máximo de iterações
)


summary(modeloLog)

#gerando dataset para análise
dataset <- pns2019%>%
  dplyr::select(uf,sexo,área, homem, solteiro, solteiro1, idade, fx_idade,cor, est_civil, branco, negro, pardo, n_residentes,
                rend_dom, log_rendom,fx_renda_dom, or_sexual, d_cronic, imc,fx_imc, obesidade, exc_peso, 
                fx_esc, esc_fund, ac_net,cons_verd, seg_saude, atv_fis, fumo, cons_alc, V0024, V00301)

model_mult <- glm(formula = fx_imc ~ regiao +sexo + homem + urbano + idade + branco +
                    fx_esc + solteiro + rend_per_capita + rend_dom + total_morador + internet + 
                    diabetes + h_arterial + col_alto + AVC + depr_ + atv_fis + 
                    min_atv_fis + p_saude +fumo, family = binomial, data = df)

plot_model(model_mult)

result <- oaxaca(formula=exc_peso~uf+n_residentes+homem+idade+branco+rend_dom+d_cronic+ac_net+
                   cons_verd+seg_saude+atv_fis+fumo+cons_alc| solteiro | homem, data = dataset)


dataset <- dataset[complete.cases(dataset),]
svy <- svydesign(id=~1, strata = dataset$V0024, weights = dataset$V00301, data = dataset)

mlog <- svyglm(obesidade~uf+n_residentes+urbano+homem+solteiro+idade+cor+
                 fx_renda_dom+fx_esc+d_cronic+ac_net+cons_verd+seg_saude+atv_fis+fumo+cons_alc,
               design = svy, family=binomial)

coef1 <- coef(mlog)


coef_df1|> esquisser()

# Criando a variável de tratamento
df_weighted <- df_weighted %>% mutate(tratado = ifelse(grupo == "tratado", 1, 0))

psm_model <- glm(over ~ rend_dom + as.factor(ano) + regiao + sexo + fx_idade + fx_esc, 
                 family = binomial, data = df_weighted)
df_weighted$pscore <- predict(psm_model, type = "response")

psm_model
