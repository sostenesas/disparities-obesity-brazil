# Script: code/analise_pns_2019.R
# Descrição: Este script carrega os dados da PNS 2019, realiza a limpeza e transformação
# das variáveis, e cria um conjunto de dados final para análise de desigualdades em IMC.
# O conjunto de dados final será salvo na pasta data/ do projeto inequalities-bmi-brazil.

# Carrega os pacotes necessários
library(tidyr)     # Para manipulação de dados (e.g., drop_na, pivot)
library(survey)    # Para análises com pesos amostrais

# Cria o diretório data/ na raiz do projeto, se não existir
# O script está em code/, então usamos ../ para subir um nível até a raiz
dir.create("../data", showWarnings = FALSE)

# Carrega os dados brutos da PNS 2019
# Usa o caminho absoluto fornecido originalmente
load("C:/Users/Sóstenes/OneDrive - Insper - Instituto de Ensino e Pesquisa/Documentos/Projeto diss/inequalities-bmi-brazil/data/pns_2019.rdata")

# --- Limpeza Inicial e Cálculo de Pesos Amostrais ---
# Filtra registros de moradores selecionados para antropometria (V0025B == 1)
pns2019.1 <- PNS_2019 %>% filter(V0025B == 1)

# Calcula o peso amostral ajustado para os moradores selecionados
# V00301 é o peso original; ajusta para a população total (7060/168426190)
pns2019.1 <- pns2019.1 %>% mutate(peso_morador_selec = (V00301 * (7060 / 168426190)))

# Remove registros com peso amostral ausente (NA)
pns2019.1 <- pns2019.1 %>% filter(!is.na(peso_morador_selec))

# Verifica a distribuição do peso amostral para garantir que está correto
summary(pns2019.1$peso_morador_selec)

# Filtra apenas indivíduos com 18 anos ou mais (C008 é a idade)
pns2019.1 <- pns2019.1 %>% filter(C008 >= 18)

# --- Renomeação e Transformação de Variáveis ---

# Ano
# Renomeia a variável V0020 para "ano"
pns2019.1 <- pns2019.1 %>% rename(ano = V0020)

# Unidades Federativas (UF)
# Renomeia V0001 para "UF"
pns2019.1 <- pns2019.1 %>% rename(UF = V0001)

# Cria uma variável "regiao" com base nos códigos de UF
# 10-19: Norte, 20-29: Nordeste, 30-39: Sudeste, 40-49: Sul, 50-59: Centro-Oeste
pns2019.1 <- pns2019.1 %>%
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
pns2019.1 <- pns2019.1 %>% mutate(UF = recode_factor(UF,
                                                     "11" = "Rondônia",
                                                     "12" = "Acre",
                                                     "13" = "Amazonas",
                                                     "14" = "Roraima",
                                                     "15" = "Pará",
                                                     "16" = "Amapá",
                                                     "17" = "Tocantins",
                                                     "21" = "Maranhão",
                                                     "22" = "Piauí",
                                                     "23" = "Ceará",
                                                     "24" = "Rio Grande do Norte",
                                                     "25" = "Paraíba",
                                                     "26" = "Pernambuco",
                                                     "27" = "Alagoas",
                                                     "28" = "Sergipe",
                                                     "29" = "Bahia",
                                                     "31" = "Minas Gerais",
                                                     "32" = "Espírito Santo",
                                                     "33" = "Rio de Janeiro",
                                                     "35" = "São Paulo",
                                                     "41" = "Paraná",
                                                     "42" = "Santa Catarina",
                                                     "43" = "Rio Grande do Sul",
                                                     "50" = "Mato Grosso do Sul",
                                                     "51" = "Mato Grosso",
                                                     "52" = "Goiás",
                                                     "53" = "Distrito Federal"))

# Sexo
# Renomeia C006 para "sexo" e recodifica os valores (1 = homem, 2 = mulher)
pns2019.1 <- pns2019.1 %>% rename(sexo = C006)
pns2019.1 <- pns2019.1 %>% mutate(sexo = recode_factor(sexo, "1" = "homem", "2" = "mulher"))

# Cria uma variável dummy: homem = 1 se sexo for "homem", 0 caso contrário
pns2019.1 <- pns2019.1 %>% mutate(homem = ifelse(sexo == "homem", 1, 0))

# Idade
# Renomeia C008 para "idade"
pns2019.1 <- pns2019.1 %>% rename(idade = C008)

# Cria faixas etárias a partir da idade
pns2019.1 <- pns2019.1 %>% mutate(fx_idade = cut(idade,
                                                 breaks = c(18, 30, 45, 60, 75, Inf),
                                                 labels = c("18 a 29 anos", "30 a 44 anos", "45 a 59 anos", "60 a 74 anos", "75 anos ou mais"),
                                                 ordered_result = TRUE, right = FALSE))

# Cor/Raça
# Renomeia C009 para "cor" e recodifica os valores
pns2019.1 <- pns2019.1 %>% rename(cor = C009)
pns2019.1 <- pns2019.1 %>% mutate(cor = recode_factor(cor,
                                                      "1" = "Branco",
                                                      "2" = "Negro",
                                                      "3" = "Amarelo",
                                                      "4" = "Pardo",
                                                      "5" = "Indígena",
                                                      "9" = "Ignorado"))

# Cria uma variável dummy: branco = 1 se cor for "Branco", 0 caso contrário
pns2019.1 <- pns2019.1 %>% mutate(branco = ifelse(cor == "Branco", 1, 0))

# Estado Civil
# Renomeia C011 para "est_civil" e recodifica os valores
pns2019.1 <- pns2019.1 %>% mutate(est_civil = C011)
pns2019.1 <- pns2019.1 %>% mutate(est_civil = recode_factor(est_civil,
                                                            "1" = "Casado(a)",
                                                            "2" = "Separado(a)*",
                                                            "3" = "Viúvo(a)",
                                                            "4" = "Solteiro(a)",
                                                            "9" = "Ignorado"))

# Cria uma variável dummy: solteiro = 1 se est_civil for "Solteiro(a)", 0 caso contrário
pns2019.1 <- pns2019.1 %>% mutate(solteiro = ifelse(est_civil == "Solteiro(a)", 1, 0))

# Total de Moradores
# Renomeia V0022 para "total_morador"
pns2019.1 <- pns2019.1 %>% rename(total_morador = V0022)

# Situação Censitária (Urbano/Rural)
# Renomeia V0026 para "sit_cens" e recodifica como fator (1 = urbano, 2 = rural)
pns2019.1 <- pns2019.1 %>% rename(sit_cens = V0026)
pns2019.1$sit_cens <- factor(pns2019.1$sit_cens, levels = c(1, 2), labels = c("urbano", "rural"))

# Cria uma variável dummy: urbano = 1 se sit_cens for "urbano", 0 se "rural"
pns2019.1 <- pns2019.1 %>% mutate(urbano = recode_factor(sit_cens, "urbano" = 1, "rural" = 0))

# Acesso à Internet
# Renomeia A01901 para "internet" e recodifica (1 = sem acesso, 0 = com acesso)
pns2019.1 <- pns2019.1 %>% rename(internet = A01901)
pns2019.1 <- pns2019.1 %>% mutate(internet = ifelse(internet == 1, 0, 1))

# Atividade Física
# Renomeia P034 para "atv_fis" e recodifica (1 = sem atividade, 0 = com atividade)
pns2019.1 <- pns2019.1 %>% rename(atv_fis = P034)
pns2019.1 <- pns2019.1 %>% mutate(atv_fis = ifelse(atv_fis == 1, 0, 1))

# Renomeia P03702 para "min_atv_fis" (minutos de atividade física)
pns2019.1 <- pns2019.1 %>% rename(min_atv_fis = P03702)

# Fumo
# Renomeia P050 para "fumo" e recodifica (1 ou 2 = fuma, 0 = não fuma)
pns2019.1 <- pns2019.1 %>% rename(fumo = P050)
pns2019.1 <- pns2019.1 %>% mutate(fumo = ifelse(fumo == 1 | fumo == 2, 1, 0))

# Plano de Saúde
# Cria "p_saude" com base em I00102 (1 = com plano, 0 = sem plano)
pns2019.1 <- pns2019.1 %>% mutate(p_saude = ifelse(I00102 == 1, 1, 0))

# --- Excesso de Peso e IMC ---
# Calcula o IMC (peso em kg / altura em metros ao quadrado)
# W00203 está em centímetros, então converte para metros ao quadrado (/10000)
pns2019.1 <- pns2019.1 %>% mutate(IMC = W00103 / ((W00203 * W00203) / 10000))

# Cria a variável de excesso de peso: exc_peso = 1 se IMC >= 25, 0 caso contrário
pns2019.1 <- pns2019.1 %>% mutate(exc_peso = ifelse(IMC >= 25, 1, 0))

# Cria faixas de IMC (abaixo do peso, normal, sobrepeso, obesidade)
pns2019.1 <- pns2019.1 %>% mutate(fx_imc = cut(pns2019.1$IMC,
                                               breaks = c(0, 18.5, 24.9, 29.9, Inf),
                                               labels = c("abaixo do peso", "normal", "sobrepeso", "obesidade")))

# --- Condições de Saúde Autorreferidas ---

# Hipertensão Arterial (Q002P)
# Cria Q002P com base em Q00201 (1 = sim, 2 = não)
pns2019.1 <- pns2019.1 %>% mutate(Q002P = Q00201)
pns2019.1$Q002P <- factor(pns2019.1$Q002P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2019.1$Q002P)

# Cria uma variável dummy: h_arterial = 1 se "Sim", 0 se "Não"
pns2019.1 <- pns2019.1 %>% mutate(h_arterial = recode_factor(Q002P, "Sim" = 1, "Não" = 0))

# Diabetes (Q004P)
# Cria Q004P com base em Q03001 (1 = sim, 2 = não)
pns2019.1 <- pns2019.1 %>% mutate(Q004P = Q03001)
pns2019.1$Q004P <- factor(pns2019.1$Q004P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2019.1$Q004P)

# Cria uma variável dummy: diabetes = 1 se "Sim", 0 se "Não"
pns2019.1 <- pns2019.1 %>% mutate(diabetes = recode_factor(Q004P, "Sim" = 1, "Não" = 0))

# Colesterol Alto (Q006P)
# Cria Q006P com base em Q060 (1 = sim, 2 = não)
pns2019.1 <- pns2019.1 %>% mutate(Q006P = Q060)
pns2019.1$Q006P <- factor(pns2019.1$Q006P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2019.1$Q006P)

# Cria uma variável dummy: col_alto = 1 se "Sim", 0 se "Não"
pns2019.1 <- pns2019.1 %>% mutate(col_alto = recode_factor(Q006P, "Sim" = 1, "Não" = 0))

# Acidente Vascular Cerebral (AVC) (Q008P)
# Cria Q008P com base em Q068 (1 = sim, 2 = não)
pns2019.1 <- pns2019.1 %>% mutate(Q008P = ifelse(Q068 == 1, 1, 2))
pns2019.1$Q008P <- factor(pns2019.1$Q008P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2019.1$Q008P)

# Cria uma variável dummy: AVC = 1 se "Sim", 0 se "Não"
pns2019.1 <- pns2019.1 %>% mutate(AVC = recode_factor(Q008P, "Sim" = 1, "Não" = 0))

# Depressão (Q014P)
# Cria Q014P com base em Q092 (1 = sim, 2 = não)
pns2019.1 <- pns2019.1 %>% mutate(Q014P = ifelse(Q092 == 1, 1, 2))
pns2019.1$Q014P <- factor(pns2019.1$Q014P, levels = c(1, 2), labels = c("Sim", "Não"))

# Verifica a distribuição
summary(pns2019.1$Q014P)

# Cria uma variável dummy: depr_ = 1 se "Sim", 0 se "Não"
pns2019.1 <- pns2019.1 %>% mutate(depr_ = recode_factor(Q014P, "Sim" = 1, "Não" = 0))

# Rendimento Domiciliar per Capita
# Cria faixas de renda com base em VDF004 (1-2: até 1/2 SM, 3: 1/2 a 1 SM, etc.)
pns2019.1 <- pns2019.1 %>% mutate(rend_per_capita = ifelse(VDF004 %in% 1:2, 1,
                                                           ifelse(VDF004 %in% 3, 2,
                                                                  ifelse(VDF004 %in% 4, 3,
                                                                         ifelse(VDF004 %in% 5, 4, 5)))))

# Recodifica rend_per_capita como fator com rótulos
pns2019.1$rend_per_capita <- factor(pns2019.1$rend_per_capita,
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Até 1/2 SM", "1/2 até 1 SM", "1 até 2 SM", "2 até 3 SM", "Mais de 3 SM"))

# Verifica a distribuição das faixas de renda
summary(pns2019.1$rend_per_capita)

# Renomeia VDF002 para "rend_dom" e cria uma variável logarítmica do rendimento
pns2019.1 <- pns2019.1 %>% rename(rend_dom = VDF002)
pns2019.1 <- pns2019.1 %>% mutate(log_rendom = log(rend_dom))

# Escolaridade
# Renomeia VDD004A para "fx_esc" e recodifica os níveis de escolaridade
pns2019.1 <- pns2019.1 %>% rename(fx_esc = VDD004A)
pns2019.1 <- pns2019.1 %>% mutate(fx_esc = recode_factor(fx_esc,
                                                         "1" = "Sem instrução",
                                                         "2" = "Fundamental incompleto",
                                                         "3" = "Fundamental completo",
                                                         "4" = "Médio incompleto",
                                                         "5" = "Médio completo",
                                                         "6" = "Superior incompleto",
                                                         "7" = "Superior completo"))

# --- Seleção Final das Variáveis ---
# Seleciona apenas as variáveis relevantes para a análise
pns2019.2 <- pns2019.1 %>% select(UPA_PNS, V0024, peso_morador_selec, UF, regiao,
                                  ano, sexo, W00203, W00103, homem, sit_cens, urbano, idade,
                                  fx_idade, IMC, fx_imc, cor, branco, fx_esc, est_civil, solteiro,
                                  rend_per_capita, rend_dom, log_rendom,
                                  total_morador, internet, Q002P, Q004P, Q006P,
                                  Q008P, Q014P, diabetes, h_arterial, col_alto, AVC,
                                  depr_, atv_fis, min_atv_fis, p_saude, fumo)

# Salva o conjunto de dados final como um arquivo RData no diretório data/
# Usa o mesmo caminho absoluto para salvar o arquivo
save(pns2019.2, file = "C:/Users/Sóstenes/OneDrive - Insper - Instituto de Ensino e Pesquisa/Documentos/Projeto diss/inequalities-bmi-brazil/data/pns2019.2.RData")

# Libera memória
gc()