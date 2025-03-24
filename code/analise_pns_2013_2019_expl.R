library(dplyr)
library(ggplot2)
library(survey)

load("pns_2013_2019.RData")

desPNSW13_19 = svydesign(id=~UPA_PNS, strat=~V0024, weight=~peso_morador_selec, 
                         nest=TRUE, data=df)

# Cria a tabela ponderada para fx_imc por faixa de idade, ano e sexo
tabela_df1 <- svytable(~ ano + fx_imc + regiao + sexo + sit_cens + fx_idade + cor +
                         fx_esc + est_civil + rend_per_capita, design = desPNSW13_19)
df1_tab <- as.data.frame(tabela_df1)

gc()

# Calcular a proporção dentro de cada grupo (faixa etária por ano e sexo)
df1_tab <- df1_tab %>%
  group_by(fx_idade, ano, sexo, cor, fx_esc) %>%
  mutate(prop = Freq / sum(Freq)) %>%
  ungroup()

ggplot(df1_tab, aes(x = fx_idade, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Faixa de idade",
       y = "Percentual",
       fill = "Faixa de IMC") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_wrap(vars(ano, sexo, sit_cens), nrow = 4, ncol = 2)

ggplot(df1_tab, aes(x = regiao, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Região",
       y = "Percentual",
       fill = "Faixa de IMC") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_wrap(vars(sexo, ano))

gc()

ggplot(df1_tab, aes(x = rend_per_capita, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Percentual",
       y = "Renda per capita",
       fill = "Faixa de IMC") +
  theme_bw() +
  coord_flip()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_wrap(vars(ano, sexo))

ggplot(df1_tab, aes(x = fx_esc, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Escolaridade",
       y = "Percentual",
       fill = "Faixa de IMC") +
  coord_flip()+
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_wrap(vars(ano, sexo))

gc()

ggplot(df1_tab, aes(x = est_civil, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "",
       y = "Percentual",
       fill = "Faixa de IMC") +
  theme_bw() +
  coord_flip () +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_wrap(vars(ano, sexo))

df1_tab1 <- df1_tab%>%
  filter(!(cor %in% "Ignorado")) %>%
  ggplot() +
  aes(x = cor, y = prop, fill = fx_imc) +
  geom_col(position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Percentual",
       y = "Cor",
       fill = "Faixa de IMC")+
  theme_bw() +
  coord_flip()+
  facet_wrap(vars(ano, sexo), nrow = 4, ncol = 2)

df1_tab1
gc()