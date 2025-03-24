library(dplyr)
library(ggplot2)
library(tidyverse)
library(nnet)
library(survey)


load("C:\\Users\\Sóstenes\\Downloads\\pns2013.rdata") #mudar pasta do arquivo

#Selecionando registros válidos para o módulo P e calculando peso amostral - summary de verificação
pns2013.1<- pns2013%>%filter(M001==1)
pns2013.1<-pns2013.1 %>% mutate(peso_morador_selec=((V00291*(60202/145572211))))
pns2013.1<-pns2013.1 %>% filter(!is.na(peso_morador_selec))
pns2013.1 <- pns2013.1%>%filter(C008>=18)
summary(pns2013.1$peso_morador_selec)

#Ano
pns2013.1 <- pns2013.1%>%rename(ano = V0020)

#Sexo
pns2013.1 <- pns2013.1%>%rename(sexo = C006)
pns2013.1 <- pns2013.1%>%mutate(sexo = recode_factor(sexo, "1" = "homem", "2" = "mulher"))
pns2013.1 <- pns2013.1%>%mutate(homem = ifelse(sexo == "homem",1,0))

#Situação censitária
pns2013.1 <- pns2013.1%>%rename(sit_cens = V0026)
pns2013.1$sit_cens <- factor(pns2013.1$sit_cens, levels = c(1,2), 
                             labels = c("urbano","rural"))
pns2013.1 <- pns2013.1%>%mutate(urbano = recode_factor
                                (sit_cens, "urbano" = 1, "rural" = 0 ))

#Estados - UFs
pns2013.1 <- pns2013.1 %>% rename(UF=V0001)

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


pns2013.1$UF<-factor(pns2013.1$UF, 
                                        levels=c(11,12,13,14,15,16,17,21,22,
                                                 23,24,25,26,27,28,29,31,32,
                                                 33,35,41,42,43,50,51,52,53),
                                        label=c("Rondonia","Acre","Amazonas",
                                                "Roraima","Para","Amapa","Tocantins",
                                                "Maranhao","Piaui","Ceara",
                                                "Rio Grande do Norte","Paraiba",
                                                "Pernambuco","Alagoas","Sergipe",
                                                "Bahia","Minas Gerais","Espirito Santo",
                                                "Rio de Janeiro","Sao Paulo",
                                                "Parana","Santa Catarina",
                                                "Rio Grande do Sul",
                                                "Mato Grosso do Sul","Mato Grosso","Goias","Distrito Federal"))
summary(pns2013.1$UF)

#Faixas Etárias
pns2013.1 <- pns2013.1%>%rename(idade = C008)
pns2013.1 <- pns2013.1 %>% mutate(fx_idade=cut(idade,
                                                   breaks = c(18,30, 45, 60, 75,Inf),
                                                   labels = c("18 a 29 anos","30 a 44 anos","45 a 59 anos","60 a 74 anos","75 anos ou mais"), 
                                                   ordered_result = TRUE, right = FALSE))
summary(pns2013.1$fx_idade)

#Cor
pns2013.1 <- pns2013.1%>%rename(cor = C009)
pns2013.1 <- pns2013.1%>%mutate(cor = recode_factor(cor,
                                                "1" = "Branco",
                                                "2" = "Negro",
                                                "3" = "Amarelo",
                                                "4" = "Pardo",
                                                "5" = "Indígena",
                                                "9" = "Ignorado"
                                                ))
pns2013.1 <- pns2013.1%>%mutate(branco = ifelse(cor == "Branco",1,0))

# Escolaridade
pns2013.1 <- pns2013.1%>%rename(fx_esc = VDD004A)
pns2013.1 <- pns2013.1%>%mutate(fx_esc = recode_factor(fx_esc, 
                                                   "1" =	"Sem instrução",
                                                   "2" =	"Fundamental incompleto",
                                                   "3" =	"Fundamental completo",
                                                   "4" = 	"Médio incompleto",
                                                   "5" =  "Médio completo",
                                                   "6" =	"Superior incompleto",
                                                   "7" = 	"Superior completo" ))
summary(pns2013.1$fx_esc)

#Rendimento domiciliar per capita

pns2013.1 <-  pns2013.1 %>% drop_na(VDF003) %>% mutate(rend_per_capita=cut(VDF003,
  breaks = c(-Inf,339, 678, 1356, 2034,Inf),
  labels=c("Até 1/2 SM","1/2 até 1 SM","1 até 2 SM","2 até 3 SM","Mais de 3 SM"), 
  ordered_result = TRUE, right = TRUE, na.exclude= TRUE))

summary(pns2013.1$rend_per_capita)

pns2013.1 <- pns2013.1%>%rename(rend_dom = VDF003)
pns2013.1 <- pns2013.1%>%mutate(log_rendom = log(rend_dom))

#C011-estado civil
pns2013.1 <- pns2013.1%>%mutate(est_civil = C011)
pns2013.1 <- pns2013.1%>%mutate(est_civil = recode_factor(est_civil,
                                                      "1" =	"Casado(a)",
                                                      "2" = "Separado(a)*",
                                                      "4" = "Separado(a)*",
                                                      "3" = "Viúvo(a)",
                                                      "5"	= "Solteiro(a)",
                                                      "9" = "Ignorado"
                                                      ))
pns2013.1 <- pns2013.1%>%mutate(solteiro = ifelse(est_civil == "Solteiro(a)",1,0))


#I001-plano de saúde
pns2013.1 <- pns2013.1%>%rename(p_saude = I001)
pns2013.1 <- pns2013.1%>%mutate(p_saude = ifelse(p_saude == 1,0,1))

#A019-acesso à internet
pns2013.1 <- pns2013.1%>%rename(internet = A019)
pns2013.1 <- pns2013.1%>%mutate(internet = ifelse(internet == 1,0,1))

#V0022-total de moradores
pns2013.1 <- pns2013.1%>%rename(total_morador = V0022)

#Diagnóstico médico autorreferido de hipertensão arterial  - Q002P 
pns2013.1 <- pns2013.1 %>% mutate(Q002P = if_else(Q002==1,1,2,missing=2))                                         
pns2013.1$Q002P<-factor(pns2013.1$Q002P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2013.1$Q002P)
pns2013.1 <- pns2013.1 %>% mutate(h_arterial = recode_factor(Q002P, "Sim" = 1, "Não" = 0))

#Diagnóstico médico autorreferido de diabetes - Q004P
pns2013.1 <- pns2013.1 %>% mutate(Q004P = if_else(Q030==1,1,2,missing=2))                                         
pns2013.1$Q004P<-factor(pns2013.1$Q004P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2013.1$Q004P)
pns2013.1 <- pns2013.1 %>% mutate(diabetes = recode_factor(Q004P, "Sim" = 1, "Não" = 0))

#Diagnóstico médico autorreferido de colesterol alto  - Q006P
pns2013.1 <- pns2013.1 %>% mutate(Q006P = if_else(Q060 == 1,1,2,missing=2))
pns2013.1$Q006P<-factor(pns2013.1$Q006P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2013.1$Q006P)
pns2013.1 <- pns2013.1 %>% mutate(col_alto = recode_factor(Q006P, "Sim" = 1, "Não" = 0))

#Diagnóstico médico autorreferido de AVC (Acidente Vascular Cerebral) - Q008P
pns2013.1 <- pns2013.1 %>% mutate(Q008P = ifelse(Q068 == 1,1,2))
pns2013.1$Q008P<-factor(pns2013.1$Q008P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2013.1$Q008P)
pns2013.1 <- pns2013.1 %>% mutate(AVC = recode_factor(Q008P, "Sim" = 1, "Não" = 0))

#Diagnóstico autorreferido de depressão por profissional de saúde mental - Q014P
pns2013.1 <- pns2013.1 %>% mutate(Q014P = ifelse(Q092==1,1,2))
pns2013.1$Q014P<-factor(pns2013.1$Q014P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2013.1$Q014P)
pns2013.1 <- pns2013.1 %>% mutate(depr_ = recode_factor(Q014P, "Sim" = 1, "Não" = 0))

# Excesso de peso - W001P
pns2013.1 <- pns2013.1 %>% filter(W00203>=1)
pns2013.1 <- pns2013.1 %>% mutate(altura_metro = W00203 / 100)
pns2013.1 <- pns2013.1 %>% mutate(IMC = W00103 / (altura_metro * altura_metro))
pns2013.1 <- pns2013.1 %>% mutate(W001P = ifelse(IMC >=24.999999999999,1,2))
pns2013.1$W001P<-factor(pns2013.1$W001P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2013.1$W001P)

pns2013.1 <- pns2013.1%>%mutate(exc_peso = recode_factor(W001P, "Sim" = 1, "Não" = 0))

pns2013.1 <- pns2013.1%>%mutate(fx_imc = cut(pns2013.1$IMC, 
                                             breaks = c(0, 18.5, 24.9, 29.9, Inf), 
                                             labels = c("abaixo do peso", "normal",
                                                        "sobrepeso", "obesidade")))

#P034 Atividade Física
pns2013.1 <- pns2013.1%>%rename(atv_fis = P034)
pns2013.1 <- pns2013.1%>%mutate(atv_fis = ifelse(atv_fis == 1,0,1))
pns2013.1 <- pns2013.1%>%rename(min_atv_fis = P03702)

#P050 Fumo
pns2013.1 <- pns2013.1%>%rename(fumo = P050)
pns2013.1 <- pns2013.1%>%mutate(fumo = ifelse(fumo == 1 | fumo == 2, 1,0))

pns2013.2 <- pns2013.1%>%select(UPA_PNS, V0024, peso_morador_selec, UF, regiao,
                                ano, sexo, W00203, W00103, homem, sit_cens, urbano, idade,
                                fx_idade, IMC, fx_imc, cor, branco, fx_esc, est_civil, solteiro,
                                rend_per_capita,rend_dom, log_rendom, 
                                total_morador, internet, Q002P, Q004P, Q006P,
                                Q008P,Q014P, diabetes,h_arterial,col_alto, AVC, 
                                depr_, atv_fis, min_atv_fis, p_saude, fumo)

save(pns2013.2, file = "pns2013.2.RData")
gc()