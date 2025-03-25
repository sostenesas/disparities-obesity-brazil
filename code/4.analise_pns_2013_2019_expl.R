# Script: analise_pns_2013_2019_expl.R
# Descrição: Este script carrega os dados combinados da PNS 2013 e 2019, cria uma tabela
# ponderada com proporções de faixas de IMC por diferentes variáveis (faixa etária, região,
# sexo, situação censitária, escolaridade, estado civil, renda, cor/raça), e gera gráficos
# de barras empilhadas para visualização das proporções.

# --- Carrega os Pacotes Necessários ---
library(dplyr)      # Para manipulação de dados (group_by, mutate, etc.)
library(ggplot2)    # Para criação de gráficos
library(survey)     # Para análises com pesos amostrais

# --- Carrega os Dados Combinados ---
# Carrega o dataframe combinado de 2013 e 2019, gerado anteriormente pelos scripts
# data_preparation_pns2013.R e data_preparation_pns2019.R
load("pns_2013_2019.RData")

# --- Cria o Objeto de Desenho Amostral ---
# Define o desenho amostral usando o pacote survey para aplicar os pesos amostrais
# id = ~UPA_PNS: Identificador de unidade primária de amostragem
# strat = ~V0024: Estrato da amostra
# weight = ~peso_morador_selec: Peso amostral ajustado (calculado nos scripts de preparação)
# nest = TRUE: Indica que as unidades de amostragem são aninhadas
desPNSW13_19 <- svydesign(id = ~UPA_PNS, strat = ~V0024, weight = ~peso_morador_selec,
                          nest = TRUE, data = df)

# --- Cria a Tabela Ponderada ---
# Usa svytable para calcular frequências ponderadas de faixas de IMC (fx_imc) por várias variáveis
# As variáveis incluídas são: ano, região, sexo, situação censitária, faixa etária, cor/raça,
# escolaridade, estado civil e renda per capita
tabela_df1 <- svytable(~ ano + fx_imc + regiao + sexo + sit_cens + fx_idade + cor +
                         fx_esc + est_civil + rend_per_capita, design = desPNSW13_19)

# Converte a tabela ponderada em um dataframe para facilitar a manipulação
df1_tab <- as.data.frame(tabela_df1)

# Libera memória após a criação da tabela
gc()

# --- Calcula Proporções ---
# Calcula a proporção de cada faixa de IMC dentro de cada grupo definido por
# faixa etária, ano, sexo, cor/raça e escolaridade
df1_tab <- df1_tab %>%
  group_by(fx_idade, ano, sexo, cor, fx_esc) %>%
  mutate(prop = Freq / sum(Freq)) %>%  # Proporção = frequência do grupo / total do grupo
  ungroup()

# --- Gera Gráficos de Barras Empilhadas ---

# Gráfico 1: Proporções de faixas de IMC por faixa etária, ano, sexo e situação censitária
p1 <- ggplot(df1_tab, aes(x = fx_idade, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +  # Cria barras empilhadas com proporções
  scale_fill_viridis_d(option = "viridis", direction = -1) +  # Usa a paleta viridis para as cores
  labs(x = "Faixa Etária",
       y = "Percentual",
       fill = "Faixa de IMC",
       title = "Proporções de Faixas de IMC por Faixa Etária",
       subtitle = "Segmentado por Ano, Sexo e Situação Censitária") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona rótulos do eixo X
        legend.position = "bottom") +  # Posiciona a legenda na parte inferior
  facet_wrap(vars(ano, sexo, sit_cens), nrow = 4, ncol = 2)  # Cria facetas para ano, sexo e situação censitária

# Gráfico 2: Proporções de faixas de IMC por região, sexo e ano
p2 <- ggplot(df1_tab, aes(x = regiao, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Região",
       y = "Percentual",
       fill = "Faixa de IMC",
       title = "Proporções de Faixas de IMC por Região",
       subtitle = "Segmentado por Sexo e Ano") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_wrap(vars(sexo, ano))

# Gráfico 3: Proporções de faixas de IMC por renda per capita, ano e sexo
p3 <- ggplot(df1_tab, aes(x = rend_per_capita, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Renda per Capita",
       y = "Percentual",
       fill = "Faixa de IMC",
       title = "Proporções de Faixas de IMC por Renda per Capita",
       subtitle = "Segmentado por Ano e Sexo") +
  theme_bw() +
  coord_flip() +  # Inverte os eixos para melhor legibilidade
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(vars(ano, sexo))

# Gráfico 4: Proporções de faixas de IMC por escolaridade, ano e sexo
p4 <- ggplot(df1_tab, aes(x = fx_esc, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Escolaridade",
       y = "Percentual",
       fill = "Faixa de IMC",
       title = "Proporções de Faixas de IMC por Escolaridade",
       subtitle = "Segmentado por Ano e Sexo") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(vars(ano, sexo))

# Gráfico 5: Proporções de faixas de IMC por estado civil, ano e sexo
p5 <- ggplot(df1_tab, aes(x = est_civil, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Estado Civil",
       y = "Percentual",
       fill = "Faixa de IMC",
       title = "Proporções de Faixas de IMC por Estado Civil",
       subtitle = "Segmentado por Ano e Sexo") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(vars(ano, sexo))

# Gráfico 6: Proporções de faixas de IMC por cor/raça, ano e sexo
# Filtra a categoria "Ignorado" da variável cor para evitar ruído nos resultados
p6 <- df1_tab %>%
  filter(!(cor %in% "Ignorado")) %>%
  ggplot(aes(x = cor, y = prop, fill = fx_imc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(x = "Cor/Raça",
       y = "Percentual",
       fill = "Faixa de IMC",
       title = "Proporções de Faixas de IMC por Cor/Raça",
       subtitle = "Segmentado por Ano e Sexo") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(vars(ano, sexo), nrow = 4, ncol = 2)

# --- Salva os Gráficos como Arquivos PNG ---
# Salva os gráficos no diretório figures/ (crie o diretório se não existir)
# As dimensões (8x6 polegadas) e resolução (300 DPI) são adequadas para publicação
ggsave("figures/fig_proportions_imc_age.png", plot = p1, width = 8, height = 8, dpi = 300)
ggsave("figures/fig_proportions_imc_region.png", plot = p2, width = 8, height = 6, dpi = 300)
ggsave("figures/fig_proportions_imc_income.png", plot = p3, width = 8, height = 6, dpi = 300)
ggsave("figures/fig_proportions_imc_education.png", plot = p4, width = 8, height = 6, dpi = 300)
ggsave("figures/fig_proportions_imc_marital_status.png", plot = p5, width = 8, height = 6, dpi = 300)
ggsave("figures/fig_proportions_imc_race.png", plot = p6, width = 8, height = 8, dpi = 300)

# --- Salva a Tabela de Proporções como CSV ---
# Salva a tabela de proporções no diretório tables/ para uso posterior
write.csv(df1_tab, file = "tables/proportions_imc.csv", row.names = FALSE)

# --- Opcional: Exibe os Gráficos no R ---
# Descomente as linhas abaixo para visualizar os gráficos no RStudio ou console
# print(p1)
# print(p2)
# print(p3)
# print(p4)
# print(p5)
# print(p6)

# Libera memória após a execução
gc()