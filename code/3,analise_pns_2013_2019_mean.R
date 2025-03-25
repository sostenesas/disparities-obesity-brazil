# Script: analise_pns_2013_2019.R
# Descrição: Este script combina os dados da PNS 2013 e 2019, aplica pesos amostrais,
# calcula médias de IMC por faixas etárias e outras variáveis, e gera gráficos de linhas
# para análise de tendências ao longo do tempo.

# --- Carrega os Pacotes Necessários ---
library(dplyr)      # Para manipulação de dados
library(survey)     # Para análises com pesos amostrais
library(ggplot2)    # Para criação de gráficos
library(tidyr)      # Para manipulação de dados (usado implicitamente por dplyr)

# --- Carrega e Combina os Dados ---
# Carrega os conjuntos de dados preparados anteriormente (pns2013.2 e pns2019.2)
# Descomente estas linhas se precisar recriar o arquivo combinado
# load("pns2013.2.RData")
# load("pns2019.2.RData")

# Combina os dados de 2013 e 2019 em um único dataframe
# df <- rbind(pns2013.2, pns2019.2)

# Salva o dataframe combinado como um arquivo RData
# save(df, file = "pns_2013_2019.RData")

# Carrega o dataframe combinado (se já foi criado anteriormente)
load("pns_2013_2019.RData")

# Opcional: Exporta o dataframe combinado como CSV para uso em outros softwares
# write.csv(df, file = "pns_2013_2019.csv", row.names = FALSE)

# --- Cria o Objeto de Desenho Amostral ---
# Define o desenho amostral usando o pacote survey
# id = ~UPA_PNS: Identificador de unidade primária de amostragem
# strat = ~V0024: Estrato da amostra
# weight = ~peso_morador_selec: Peso amostral ajustado
# nest = TRUE: Indica que as unidades de amostragem são aninhadas
desPNSW13_19 <- svydesign(id = ~UPA_PNS, strat = ~V0024,
                          weight = ~peso_morador_selec, nest = TRUE, data = df)

# Extrai as variáveis do desenho amostral como um dataframe para facilitar a análise
df_weighted <- as.data.frame(desPNSW13_19$variables)

# --- Calcula Médias de IMC por Diferentes Variáveis ---
# Calcula a média do IMC por faixa etária, sexo e ano
media_imc1 <- aggregate(IMC ~ fx_idade + sexo + ano, data = df_weighted, FUN = mean)

# Calcula a média do IMC por faixa etária, situação censitária (urbano/rural) e ano
media_imc2 <- aggregate(IMC ~ fx_idade + sit_cens + ano, data = df_weighted, FUN = mean)

# Calcula a média do IMC por faixa etária, estado civil (solteiro ou não) e ano
media_imc3 <- aggregate(IMC ~ fx_idade + solteiro + ano, data = df_weighted, FUN = mean)

# Calcula a média do IMC por faixa etária, cor/raça (branco ou não) e ano
media_imc4 <- aggregate(IMC ~ fx_idade + branco + ano, data = df_weighted, FUN = mean)

# --- Gera Gráficos de Linhas para Visualização ---

# Gráfico 1: Média de IMC por faixa etária, sexo e ano
p1 <- ggplot(media_imc1, aes(x = fx_idade, y = IMC,
                             colour = sexo,
                             group = interaction(ano, sexo),
                             linetype = factor(ano))) +
  geom_point(size = 3) +  # Adiciona pontos para cada média
  geom_line(linewidth = 0.5) +  # Adiciona linhas conectando os pontos
  scale_color_hue(direction = 1) +  # Define a paleta de cores para o sexo
  scale_linetype_manual(values = c("2013" = "dashed", "2019" = "solid")) +  # Define o tipo de linha por ano
  theme_bw() +  # Usa um tema limpo (branco com bordas pretas)
  labs(x = "Faixa Etária",  # Rótulo do eixo X
       y = "IMC Médio",  # Rótulo do eixo Y
       color = "Sexo",  # Legenda para a cor
       linetype = "Ano",  # Legenda para o tipo de linha
       title = "Média de IMC por Faixa Etária e Sexo (2013 e 2019)") +  # Título do gráfico
  theme(legend.position = "bottom",  # Posiciona a legenda na parte inferior
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos do eixo X para melhor legibilidade

# Gráfico 2: Média de IMC por faixa etária, situação censitária e ano
p2 <- ggplot(media_imc2, aes(x = fx_idade, y = IMC,
                             colour = sit_cens,
                             group = interaction(ano, sit_cens),
                             linetype = factor(ano))) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.5) +
  scale_color_hue(direction = 1) +
  scale_linetype_manual(values = c("2013" = "dashed", "2019" = "solid")) +
  theme_bw() +
  labs(x = "Faixa Etária",
       y = "IMC Médio",
       color = "Situação Censitária",
       linetype = "Ano",
       title = "Média de IMC por Faixa Etária e Situação Censitária (2013 e 2019)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 3: Média de IMC por faixa etária, estado civil (solteiro) e ano
p3 <- ggplot(media_imc3, aes(x = fx_idade, y = IMC,
                             colour = as.factor(solteiro),
                             group = interaction(ano, solteiro),
                             linetype = factor(ano))) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.5) +
  scale_color_hue(direction = 1) +
  scale_linetype_manual(values = c("2013" = "dashed", "2019" = "solid")) +
  theme_bw() +
  labs(x = "Faixa Etária",
       y = "IMC Médio",
       color = "Estado Civil (Solteiro)",
       linetype = "Ano",
       title = "Média de IMC por Faixa Etária e Estado Civil (2013 e 2019)") +
  scale_color_discrete(labels = c("Não Solteiro", "Solteiro")) +  # Ajusta os rótulos da legenda para maior clareza
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 4: Média de IMC por faixa etária, cor/raça (branco) e ano
p4 <- ggplot(media_imc4, aes(x = fx_idade, y = IMC,
                             colour = as.factor(branco),
                             group = interaction(ano, branco),
                             linetype = factor(ano))) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.5) +
  scale_color_hue(direction = 1) +
  scale_linetype_manual(values = c("2013" = "dashed", "2019" = "solid")) +
  theme_bw() +
  labs(x = "Faixa Etária",
       y = "IMC Médio",
       color = "Cor/Raça (Branco)",
       linetype = "Ano",
       title = "Média de IMC por Faixa Etária e Cor/Raça (2013 e 2019)") +
  scale_color_discrete(labels = c("Não Branco", "Branco")) +  # Ajusta os rótulos da legenda para maior clareza
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# --- Salva os Gráficos como Arquivos PNG ---
# Salva os gráficos no diretório figures/ (crie o diretório se não existir)
ggsave("figures/fig_imc_age_sex.png", plot = p1, width = 8, height = 6, dpi = 300)
ggsave("figures/fig_imc_age_sit_cens.png", plot = p2, width = 8, height = 6, dpi = 300)
ggsave("figures/fig_imc_age_marital_status.png", plot = p3, width = 8, height = 6, dpi = 300)
ggsave("figures/fig_imc_age_race.png", plot = p4, width = 8, height = 6, dpi = 300)

# --- Opcional: Exibe os Gráficos no R ---
# Descomente as linhas abaixo para visualizar os gráficos no RStudio ou console
# print(p1)
# print(p2)
# print(p3)
# print(p4)