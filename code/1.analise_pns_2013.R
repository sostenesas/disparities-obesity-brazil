# Script: analise_pns2013.R
# Descrição: Este script carrega os dados da PNS 2013, realiza a limpeza e transformação
# das variáveis, e cria um conjunto de dados final para análise de desigualdades em IMC.

# Carrega os pacotes necessários
library(dplyr)      # Para manipulação de dados
library(ggplot2)    # Para visualizações (não usado diretamente aqui, mas pode ser útil)
library(tidyverse)  # Conjunto de pacotes para manipulação e visualização de dados
library(survey)     # Para análises com pesos amostrais

# Carrega os dados brutos da PNS 2013
# Altere o caminho para o local onde o arquivo está no seu computador
load("C:\\Users\\Sóstenes\\OneDrive - Insper - Instituto de Ensino e Pesquisa\\Documentos\\Projeto diss\\inequalities-bmi-brazil\\data\\pns_2013.Rdata")

# --- Limpeza Inicial e Cálculo de Pesos Amostrais ---
# Filtra registros válidos para o módulo P (M001 == 1 indica participação no módulo)
pns2013.1 <- pns2013 %>% filter(M001 == 1)

# Calcula o peso amostral ajustado para os moradores selecionados
# V00291 é o peso original; ajusta para a população total (60202/145572211)
pns2013.1 <- pns2013.1 %>% mutate(peso_morador_selec = ((V00291 * (60202 / 145572211))))

# Remove registros com peso amostral ausente (NA)
pns2013.1 <- pns2013.1 %>% filter(!is.na(peso_morador_selec))

# Filtra apenas indivíduos com 18 anos ou mais (C008 é a idade)
pns2013.1 <- pns2013.1 %>% filter(C008 >= 18)

# Verifica a distribuição do peso amostral para garantir que está correto
summary(pns2013.1$peso_morador_selec)

# --- Renomeação e Transformação de Variáveis ---

# Ano
# Renomeia a variável V0020 para "ano"
pns2013.1 <- pns2013.1 %>% rename(ano = V0020)

# Sexo
# Renomeia C006 para "sexo" e recodifica os valores (1 = homem, 2 = mulher)
pns2013.1 <- pns2013.1 %>% rename(sexo = C006)
pns2013.1 <- pns2013.1 %>% mutate(sexo = recode_factor(sexo, "1" = "homem", "2" = "mulher"))

# Cria uma variável dummy: homem = 1 se sexo for "homem", 0 caso contrário
pns2013.1 <- pns2013.1 %>% mutate(homem = ifelse(sexo == "homem", 1, 0))

# Situação Censitária
# Renomeia V0026 para "sit_cens" e recodifica como fator (1 = urbano, 2 = rural)
pns2013.1 <- pns2013.1 %>% rename(sit_cens = V0026)
pns2013.1$sit_cens <- factor(pns2013.1$sit_cens, levels = c(1, 2), labels = c("urbano", "rural"))

# Cria uma variável dummy: urbano = 1 se sit_cens for "urbano", 0 se "rural"
pns2013.1 <- pns2013.1 %>% mutate(urbano = recode_factor(sit_cens, "urbano" = 1, "rural" = 0))

# Estados (Unidades Federativas - UF)
# Renomeia V0001 para "UF"
pns2013.1 <- pns2013.1 %>% rename(UF = V0001)

# Cria uma variável "regiao" com base nos códigos de UF
# 10-19: Norte, 20-29: Nordeste, 30-39: Sudeste, 40-49: Sul, 50-59: Centro-Oeste
pns2013.1 <- pns2013.1 %>%
  mutate(UF = as.numeric(UF),  # Garante que UF seja numérico
         regiao = case_when(
           UF >= 10 & UF < 20 ~ "Norte",
           UF >= 20 & UF < 30 ~ "Nordeste",
           UF >= 30 & UF < 40 ~ "Sudeste",
           UF >= 40 & UF < 50 ~ "Sul",
           UF >= 50 & UF < 60 ~ "Centro-Oeste",
           TRUE ~ NA_character_  # Para valores inesperados
         ))

# Recodifica UF como fator com rótulos para cada estado
pns2013.1$UF <- factor(pns2013.1$UF,
                       levels = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
                       labels = c("Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", "Tocantins",
                                  "Maranhao", "Piaui", "Ceara", "Rio Grande do Norte", "Paraiba",
                                  "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais",
                                  "Espirito Santo", "Rio de Janeiro", "Sao Paulo", "Parana",
                                  "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul",
                                  "Mato Grosso", "Goias", "Distrito Federal"))

# Verifica a distribuição das UFs
summary(pns2013.1$UF)

# Faixas Etárias
# Renomeia C008 para "idade"
pns2013.1 <- pns2013.1 %>% rename(idade = C008)

# Cria faixas etárias a partir da idade
pns2013.1 <- pns2013.1 %>% mutate(fx_idade = cut(idade,
                                                 breaks = c(18, 30, 45, 60, 75, Inf),
                                                 labels = c("18 a 29 anos", "30 a 44 anos", "45 a 59 anos", "60 a 74 anos", "75 anos ou mais"),
                                                 ordered_result = TRUE, right = FALSE))

# Verifica a distribuição das faixas etárias
summary(pns2013.1$fx_idade)

# Cor/Raça
# Renomeia C009 para "cor" e recodifica os valores
pns2013.1 <- pns2013.1 %>% rename(cor = C009)
pns2013.1 <- pns2013.1 %>% mutate(cor = recode_factor(cor,
                                                      "1" = "Branco",
                                                      "2" = "Negro",
                                                      "3" = "Amarelo",
                                                      "4" = "Pardo",
                                                      "5" = "Indígena",
                                                      "9" = "Ignorado"))

# Cria uma variável dummy: branco = 1 se cor for "Branco", 0 caso contrário
pns2013.1 <- pns2013.1 %>% mutate(branco = ifelse(cor == "Branco", 1, 0))

# Escolaridade
# Renomeia VDD004A para "fx_esc" e recodifica os níveis de escolaridade
pns2013.1 <- pns2013.1 %>% rename(fx_esc = VDD004A)
pns2013.1 <- pns2013.1 %>% mutate(fx_esc = recode_factor(fx_esc,
                                                         "1" = "Sem instrução",
                                                         "2" = "Fundamental incompleto",
                                                         "3" = "Fundamental completo",
                                                         "4" = "Médio incompleto",
                                                         "5" = "Médio completo",
                                                         "6" = "Superior incompleto",
                                                         "7" = "Superior completo"))

# Verifica a distribuição da escolaridade
summary(pns2013.1$fx_esc)

# Rendimento Domiciliar per Capita
# Remove valores ausentes (NA) de VDF003 (rendimento) e cria faixas de renda
pns2013.1 <- pns2013.1 %>% drop_na(VDF003) %>% mutate(rend_per_capita = cut(VDF003,
                                                                            breaks = c(-Inf, 339, 678, 1356, 2034, Inf),
                                                                            labels = c("Até 1/2 SM", "1/2 até 1 SM", "1 até 2 SM", "2 até 3 SM", "Mais de 3 SM"),
                                                                            ordered_result = TRUE, right = TRUE))

# Verifica a distribuição das faixas de renda
summary(pns2013.1$rend_per_capita)

# Renomeia VDF003 para "rend_dom" e cria uma variável logarítmica do rendimento
pns2013.1 <- pns2013.1 %>% rename(rend_dom = VDF003)
pns2013.1 <- pns2013.1 %>% mutate(log_rendom = log(rend_dom))

# Estado Civil
# Renomeia C011 para "est_civil" e recodifica os valores
pns2013.1 <- pns2013.1 %>% mutate(est_civil = C011)
pns2013.1 <- pns2013.1 %>% mutate(est_civil = recode_factor(est_civil,
                                                            "1" = "Casado(a)",
                                                            "2" = "Separado(a)*",
                                                            "4" = "Separado(a)*",
                                                            "3" = "Viúvo(a)",
                                                            "5" = "Solteiro(a)",
                                                            "9" = "Ignorado"))

# Cria uma variável dummy: solteiro = 1 se est_civil for "Solteiro(a)", 0 caso contrário
pns2013.1 <- pns2013.1 %>% mutate(solteiro = ifelse(est_civil == "Solteiro(a)", 1, 0))

# Plano de Saúde
# Renomeia I001 para "p_saude" e recodifica (1 = sem plano, 0 = com plano)
pns2013.1 <- pns2013.1 %>% rename(p_saude = I001)
pns2013.1 <- pns2013.1 %>% mutate(p_saude = ifelse(p_saude == 1, 0, 1))

# Acesso à Internet
# Renomeia A019 para "internet" e recodifica (1 = sem acesso, 0 = com acesso)
pns2013.1 <- pns2013.1 %>% rename(internet = A019)
pns2013.1 <- pns2013.1 %>% mutate(internet = ifelse(internet == 1, 0, 1))

# Total de Moradores
# Renomeia V0022 para "total_morador"
pns2013.1 <- pns2013.1 %>% rename(total_morador = V0022)

# --- Condições de Saúde Autorreferidas ---

# Hipertensão Arterial (Q002P)
# Cria Q002P com base em Q002 (1 = sim, 2 = não, missing = 2)
pns2013.1 <- pns2013.1 %>% mutate(Q002P = if_else(Q002 == 1, 1, 2, missing = 2))
pns2013.1$Q002P <- factor(pns2013.1$Q002P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2013.1$Q002P)

# Cria uma variável dummy: h_arterial = 1 se "Sim", 0 se "Não"
pns2013.1 <- pns2013.1 %>% mutate(h_arterial = recode_factor(Q002P, "Sim" = 1, "Não" = 0))

# Diabetes (Q004P)
# Cria Q004P com base em Q030 (1 = sim, 2 = não, missing = 2)
pns2013.1 <- pns2013.1 %>% mutate(Q004P = if_else(Q030 == 1, 1, 2, missing = 2))
pns2013.1$Q004P <- factor(pns2013.1$Q004P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2013.1$Q004P)

# Cria uma variável dummy: diabetes = 1 se "Sim", 0 se "Não"
pns2013.1 <- pns2013.1 %>% mutate(diabetes = recode_factor(Q004P, "Sim" = 1, "Não" = 0))

# Colesterol Alto (Q006P)
# Cria Q006P com base em Q060 (1 = sim, 2 = não, missing = 2)
pns2013.1 <- pns2013.1 %>% mutate(Q006P = if_else(Q060 == 1, 1, 2, missing = 2))
pns2013.1$Q006P <- factor(pns2013.1$Q006P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2013.1$Q006P)

# Cria uma variável dummy: col_alto = 1 se "Sim", 0 se "Não"
pns2013.1 <- pns2013.1 %>% mutate(col_alto = recode_factor(Q006P, "Sim" = 1, "Não" = 0))

# Acidente Vascular Cerebral (AVC) (Q008P)
# Cria Q008P com base em Q068 (1 = sim, 2 = não)
pns2013.1 <- pns2013.1 %>% mutate(Q008P = ifelse(Q068 == 1, 1, 2))
pns2013.1$Q008P <- factor(pns2013.1$Q008P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2013.1$Q008P)

# Cria uma variável dummy: AVC = 1 se "Sim", 0 se "Não"
pns2013.1 <- pns2013.1 %>% mutate(AVC = recode_factor(Q008P, "Sim" = 1, "Não" = 0))

# Depressão (Q014P)
# Cria Q014P com base em Q092 (1 = sim, 2 = não)
pns2013.1 <- pns2013.1 %>% mutate(Q014P = ifelse(Q092 == 1, 1, 2))
pns2013.1$Q014P <- factor(pns2013.1$Q014P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2013.1$Q014P)

# Cria uma variável dummy: depr_ = 1 se "Sim", 0 se "Não"
pns2013.1 <- pns2013.1 %>% mutate(depr_ = recode_factor(Q014P, "Sim" = 1, "Não" = 0))

# --- Excesso de Peso e IMC ---

# Filtra registros com altura válida (W00203 >= 1)
pns2013.1 <- pns2013.1 %>% filter(W00203 >= 1)

# Converte altura de centímetros para metros
pns2013.1 <- pns2013.1 %>% mutate(altura_metro = W00203 / 100)

# Calcula o IMC (peso em kg / altura em metros ao quadrado)
pns2013.1 <- pns2013.1 %>% mutate(IMC = W00103 / (altura_metro * altura_metro))

# Cria a variável de excesso de peso (W001P): 1 = IMC >= 25, 2 = IMC < 25
pns2013.1 <- pns2013.1 %>% mutate(W001P = ifelse(IMC >= 24.999999999999, 1, 2))
pns2013.1$W001P <- factor(pns2013.1$W001P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição de excesso de peso
summary(pns2013.1$W001P)

# Cria uma variável dummy: exc_peso = 1 se "Sim", 0 se "Não"
pns2013.1 <- pns2013.1 %>% mutate(exc_peso = recode_factor(W001P, "Sim" = 1, "Não" = 0))

# Cria faixas de IMC (abaixo do peso, normal, sobrepeso, obesidade)
pns2013.1 <- pns2013.1 %>% mutate(fx_imc = cut(pns2013.1$IMC,
                                               breaks = c(0, 18.5, 24.9, 29.9, Inf),
                                               labels = c("abaixo do peso", "normal", "sobrepeso", "obesidade")))

# Atividade Física
# Renomeia P034 para "atv_fis" e recodifica (1 = sem atividade, 0 = com atividade)
pns2013.1 <- pns2013.1 %>% rename(atv_fis = P034)
pns2013.1 <- pns2013.1 %>% mutate(atv_fis = ifelse(atv_fis == 1, 0, 1))

# Renomeia P03702 para "min_atv_fis" (minutos de atividade física)
pns2013.1 <- pns2013.1 %>% rename(min_atv_fis = P03702)

# Fumo
# Renomeia P050 para "fumo" e recodifica (1 ou 2 = fuma, 0 = não fuma)
pns2013.1 <- pns2013.1 %>% rename(fumo = P050)
pns2013.1 <- pns2013.1 %>% mutate(fumo = ifelse(fumo == 1 | fumo == 2, 1, 0))

# --- Seleção Final das Variáveis ---
# Seleciona apenas as variáveis relevantes para a análise
pns2013.2 <- pns2013.1 %>% select(UPA_PNS, V0024, peso_morador_selec, UF, regiao,
                                  ano, sexo, W00203, W00103, homem, sit_cens, urbano, idade,
                                  fx_idade, IMC, fx_imc, cor, branco, fx_esc, est_civil, solteiro,
                                  rend_per_capita, rend_dom, log_rendom,
                                  total_morador, internet, Q002P, Q004P, Q006P,
                                  Q008P, Q014P, diabetes, h_arterial, col_alto, AVC,
                                  depr_, atv_fis, min_atv_fis, p_saude, fumo)

# Salva o conjunto de dados final como um arquivo RData
save(pns2013.2, file = "pns2013.2.RData")

# Libera memória
gc()