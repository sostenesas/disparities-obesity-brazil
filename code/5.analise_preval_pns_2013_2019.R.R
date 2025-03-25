# Script: code/prevalence_analysis_pns_2013_2019.R
# Descrição: Este script carrega os dados combinados da PNS 2013 e 2019, atualiza o desenho
# amostral com novas variáveis (log da renda, quintis de renda, sobrepeso/obesidade), calcula
# prevalências ponderadas de sobrepeso/obesidade por diferentes variáveis, e gera gráficos para
# visualização das relações entre renda, IMC, idade e prevalência.

# --- Carrega os Pacotes Necessários ---
library(survey)     # Para análises com pesos amostrais
library(dplyr)      # Para manipulação de dados (group_by, summarise, etc.)
library(ggplot2)    # Para criação de gráficos
library(tidyr)      # Para manipulação de dados (usado implicitamente por dplyr)

# --- Cria os Diretórios Necessários ---
# Cria os diretórios figures/, tables/ e data/ na raiz do projeto, se não existirem
# O script está em code/, então usamos ../ para subir um nível até a raiz
dir.create("../figures", showWarnings = FALSE)
dir.create("../tables", showWarnings = FALSE)
dir.create("../data", showWarnings = FALSE)

# --- Carrega os Dados Combinados ---
# Carrega o dataframe combinado de 2013 e 2019, gerado anteriormente pelos scripts
# data_preparation_pns2013.R e data_preparation_pns2019.R, e combinado em analysis_pns_2013_2019.R
# Usa o caminho absoluto inicialmente para evitar erros
load("C:/Users/Sóstenes/OneDrive - Insper - Instituto de Ensino e Pesquisa/Documentos/Projeto diss/inequalities-bmi-brazil/data/pns_2013_2019.RData")

# --- Cria o Objeto de Desenho Amostral ---
# Define o desenho amostral usando o pacote survey para aplicar os pesos amostrais
# id = ~UPA_PNS: Identificador de unidade primária de amostragem
# strat = ~V0024: Estrato da amostra
# weight = ~peso_morador_selec: Peso amostral ajustado (calculado nos scripts de preparação)
# nest = TRUE: Indica que as unidades de amostragem são aninhadas
desPNSW13_19 <- svydesign(id = ~UPA_PNS, strat = ~V0024,
                          weight = ~peso_morador_selec, nest = TRUE, data = df)

# --- Atualiza o Objeto de Desenho Amostral com Novas Variáveis ---
# Adiciona três novas variáveis ao desenho amostral:
# - log_rend_dom: Logaritmo da renda domiciliar (rend_dom)
# - cat_log_rend: Quintis do log da renda domiciliar
# - over: Indicador de sobrepeso/obesidade (1 se fx_imc for "sobrepeso" ou "obesidade", 0 caso contrário)
desPNSW13_19 <- update(desPNSW13_19,
                       log_rend_dom = log(rend_dom),  # Calcula o logaritmo da renda domiciliar
                       cat_log_rend = cut(log(rend_dom),
                                          breaks = quantile(log(rend_dom), probs = seq(0, 1, by = 0.2), na.rm = TRUE),
                                          include.lowest = TRUE,
                                          labels = c("1º Quintil", "2º Quintil", "3º Quintil", "4º Quintil", "5º Quintil")),  # Cria quintis de renda
                       over = ifelse(fx_imc %in% c("sobrepeso", "obesidade"), 1, 0))  # Cria variável dummy para sobrepeso/obesidade

# --- Calcula Prevalências Ponderadas ---
# Usa svyby para calcular a média ponderada de sobrepeso/obesidade (over) e renda domiciliar (rend_dom)
# por sexo, escolaridade e cor/raça
aggregated <- svyby(
  formula = ~ over + rend_dom,
  by = ~ sexo + fx_esc + cor,
  design = desPNSW13_19,
  FUN = svymean,  # Calcula a média ponderada
  na.rm = TRUE  # Remove valores ausentes
)

# Converte o resultado para um dataframe
df_agg <- as.data.frame(aggregated)

# Remove o objeto temporário e libera memória
rm(aggregated)
gc()

# Visualiza o dataframe resultante (opcional)
print(df_agg)

# --- Extrai os Dados do Desenho Amostral ---
# Extrai as variáveis do desenho amostral como um dataframe para facilitar a análise
df_weighted <- as.data.frame(desPNSW13_19$variables)

# --- Gera Gráficos para Visualização ---

# Gráfico 1: Dispersão entre log da renda domiciliar e IMC, por ano
p1 <- ggplot(df_weighted, aes(x = log_rend_dom, y = IMC)) +
  geom_point(aes(size = peso_morador_selec), alpha = 0.6) +  # Tamanho dos pontos proporcional ao peso amostral
  labs(x = "Log da Renda Domiciliar",
       y = "IMC",
       size = "Peso Amostral",
       title = "Relação entre Log da Renda Domiciliar e IMC",
       subtitle = "Segmentado por Ano") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(vars(ano))

# Gráfico 2: Prevalência de sobrepeso/obesidade por renda domiciliar e sexo
p2 <- ggplot(df_agg, aes(x = rend_dom, y = over)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_line(linewidth = 0.5) +
  labs(x = "Renda Domiciliar Média",
       y = "Prevalência de Sobrepeso/Obesidade",
       title = "Prevalência de Sobrepeso/Obesidade por Renda Domiciliar",
       subtitle = "Segmentado por Sexo") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(vars(sexo))

# Gráfico 3: Prevalência de sobrepeso/obesidade por ano, região e situação censitária
# Calcula a prevalência ponderada de sobrepeso/obesidade por ano, região e situação censitária
df_prevalence <- df_weighted %>%
  group_by(ano, regiao, sit_cens) %>%
  summarise(prevalence = weighted.mean(over, w = peso_morador_selec) * 100, .groups = "drop")  # Multiplica por 100 para obter percentual

p3 <- ggplot(df_prevalence, aes(x = reorder(regiao, prevalence), y = prevalence, color = factor(ano), group = regiao)) +
  geom_line(linewidth = 1) +  # Linha do pirulito
  geom_point(size = 4) +  # Ponto do pirulito
  scale_color_manual(values = c("2013" = "darkgreen", "2019" = "gold")) +  # Cores personalizadas para os anos
  labs(x = "Região",
       y = "Prevalência de Sobrepeso/Obesidade (%)",
       color = "Ano",
       title = "Prevalência de Sobrepeso/Obesidade por Região",
       subtitle = "Segmentado por Ano e Situação Censitária") +
  theme_bw() +
  coord_flip() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_grid(vars(sit_cens))

# Gráfico 4: Dispersão entre IMC e idade, colorido por faixa de IMC e segmentado por ano
p4 <- ggplot(df_weighted, aes(x = IMC, y = idade, colour = fx_imc)) +
  geom_point(size = 0.5, alpha = 0.6) +
  scale_color_brewer(palette = "BrBG", direction = 1) +  # Usa a paleta BrBG para as faixas de IMC
  labs(x = "IMC",
       y = "Idade",
       colour = "Faixa de IMC",
       title = "Relação entre IMC e Idade",
       subtitle = "Segmentado por Ano e Colorido por Faixa de IMC") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(vars(ano))

# --- Salva os Gráficos como Arquivos PNG ---
# Salva os gráficos no diretório figures/ na raiz do projeto
# O script está em code/, então usamos ../ para subir um nível até a raiz
# As dimensões (8x6 polegadas) e resolução (300 DPI) são adequadas para publicação
ggsave("../figures/fig_scatter_income_imc.png", plot = p1, width = 8, height = 6, dpi = 300)
ggsave("../figures/fig_prevalence_income_sex.png", plot = p2, width = 8, height = 6, dpi = 300)
ggsave("../figures/fig_prevalence_region_sit_cens.png", plot = p3, width = 8, height = 6, dpi = 300)
ggsave("../figures/fig_scatter_imc_age.png", plot = p4, width = 8, height = 6, dpi = 300)

# --- Salva a Tabela de Prevalências como CSV ---
# Salva as tabelas de prevalências no diretório tables/ na raiz do projeto
write.csv(df_agg, file = "../tables/prevalence_by_sex_education_race.csv", row.names = FALSE)
write.csv(df_prevalence, file = "../tables/prevalence_by_region_sit_cens.csv", row.names = FALSE)

# --- Opcional: Exibe os Gráficos no R ---
# Descomente as linhas abaixo para visualizar os gráficos no RStudio ou console
# print(p1)
# print(p2)
# print(p3)
# print(p4)

# Libera memória após a execução
gc()