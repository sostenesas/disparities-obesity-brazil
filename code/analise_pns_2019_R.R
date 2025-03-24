# setwd ("C:/Users/Sóstenes/OneDrive - Insper - Institudo de Ensino e Pesquisa/Documentos/Projeto diss")
load("C:\\Users\\Sóstenes\\OneDrive - Insper - Instituto de Ensino e Pesquisa\\Documentos\\Projeto diss\\pns_2019.rdata")
library(esquisse)
library(tidyr)
library(survey)
library(dplyr)
library(ggplot2)

#filtrando por morador selecionado para antropometria e maiores de 18 anos
pns2019.1<- PNS_2019%>%filter(V0025A==1)
pns2019.1<-pns2019.1 %>% mutate(peso_morador_selec=((V00291*(94114/168426190))))
pns2019.1<-pns2019.1 %>% filter(!is.na(peso_morador_selec))
summary(pns2019.1$peso_morador_selec)
pns2019.1 <- pns2019.1 %>%filter(C008>=18)

#Ano
pns2019.1 <- pns2019.1%>%rename(ano = V0020)

#UF
pns2019.1 <- pns2019.1%>%rename(UF = V0001)

pns2019.1 <- pns2019.1 %>%
  mutate(regiao = case_when(
    UF %/% 10 == 1 ~ "Norte",
    UF %/% 10 == 2 ~ "Nordeste",
    UF %/% 10 == 3 ~ "Sudeste",
    UF %/% 10 == 4 ~ "Sul",
    UF %/% 10 == 5 ~ "Centro-Oeste",
    TRUE ~ NA_character_  # Para garantir que valores inesperados sejam NA
  ))

pns2019.1 <- pns2019.1%>%mutate(UF= recode_factor(UF,
                                              "11" =	"Rondônia",
                                              "12" = "Acre",
                                              "13" = 	"Amazonas",
                                              "14" =	"Roraima",
                                              "15"	= "Pará",
                                              "16" = 	"Amapá",
                                              "17"	= "Tocantins",
                                              "21"	= "Maranhão",
                                              "22" = 	"Piauí",
                                              "23" = 	"Ceará",
                                              "24" = 	"Rio Grande do Norte",
                                              "25" = 	"Paraíba",
                                              "26"	= "Pernambuco",
                                              "27" = 	"Alagoas",
                                              "28" = 	"Sergipe",
                                              "29" = 	"Bahia",
                                              "31"	= "Minas Gerais",
                                              "32" =	"Espírito Santo",
                                              "33" =	"Rio de Janeiro",
                                              "35" =	"São Paulo",
                                              "41"	= "Paraná",
                                              "42"	= "Santa Catarina",
                                              "43"	= "Rio Grande do Sul",
                                              "50" =	"Mato Grosso do Sul",
                                              "51"	= "Mato Grosso",
                                              "52" =	"Goiás",
                                              "53"	= "Distrito Federal"))

#Sexo
pns2019.1 <- pns2019.1%>%rename(sexo = C006)
pns2019.1 <- pns2019.1%>%mutate(sexo = recode_factor(sexo, "1" = "homem", "2" = "mulher"))
pns2019.1 <- pns2019.1%>%mutate(homem = ifelse(sexo == "homem",1,0))

#Idade
pns2019.1 <- pns2019.1%>%rename(idade = C008)
pns2019.1 <- pns2019.1 %>% mutate(fx_idade=cut(idade,
                                               breaks = c(18,30, 45, 60, 75,Inf),
                                               labels = c("18 a 29 anos","30 a 44 anos","45 a 59 anos","60 a 74 anos","75 anos ou mais"), 
                                               ordered_result = TRUE, right = FALSE))

#Cor
pns2019.1 <- pns2019.1%>%rename(cor = C009)
pns2019.1 <- pns2019.1%>%mutate(cor = recode_factor(cor,
                                                "1" = "Branco",
                                                "2" = "Negro",
                                                "3" = "Amarelo",
                                                "4" = "Pardo",
                                                "5" = "indígena",
                                                "9" = "Ignorado"))
pns2019.1 <- pns2019.1%>%mutate(branco = ifelse(cor == "Branco",1,0))

#Estado civil
pns2019.1 <- pns2019.1%>%mutate(est_civil = C011)
pns2019.1 <- pns2019.1%>%mutate(est_civil = recode_factor(est_civil,
                                                      "1" =	"Casado(a)",
                                                      "2" = "Divorciado(a) ou desquitado(a) ou separado(a) judicialmente",
                                                      "3" = "Viúvo(a)",
                                                      "4"	= "Solteiro(a)",
                                                      "9" = "Ignorado"))
pns2019.1 <- pns2019.1%>%mutate(solteiro = ifelse(est_civil == "Solteiro(a)",1,0))

#Total de moradores
pns2019.1 <- pns2019.1%>%rename(total_morador = V0022)

#Urbano
pns2019.1 <- pns2019.1%>%rename(sit_cens = V0026)
pns2019.1$sit_cens <- factor(pns2019.1$sit_cens, levels = c(1,2), labels = c("urbano","rural"))
pns2019.1 <- pns2019.1%>%mutate(urbano = recode_factor(sit_cens, "urbano" = 1, "rural" = 0 ))


#pns2019 <- pns2019%>%rename(d_cronic = J007)
#pns2019 <- pns2019%>%mutate(d_cronic = ifelse(d_cronic == 1,1,0))
#pns2019 <- pns2019%>%mutate(comorbidade = ifelse(Q00201 ==1 & Q03001 ==1 |Q060 ==1|Q06306==1|Q068==1|Q074==1|
                                                   #Q079==1|Q088==1|Q092==1|Q11006==1| Q11604 ==1| Q120 ==1| Q124 == 1,1,0))

#Internet
pns2019.1 <- pns2019.1%>%rename(internet = A01901)
pns2019.1 <- pns2019.1%>%mutate(internet = ifelse(internet == 1,0,1))

#Atividade Física
pns2019.1 <- pns2019.1%>%rename(atv_fis = P034)
pns2019.1 <- pns2019.1%>%mutate(atv_fis = ifelse(atv_fis == 1,0,1))
pns2019.1 <- pns2019.1%>%rename(min_atv_fis = P03702)

#Fumo
pns2019.1 <- pns2019.1%>%rename(fumo = P050)
pns2019.1 <- pns2019.1%>%mutate(fumo = ifelse(fumo == 1 | fumo == 2, 1,0))

#Plano de saúde
pns2019.1 <- pns2019.1%>%mutate(p_saude = ifelse(I00102 == 1, 1, 0))


#pns2019.1 <- pns2019.1%>%mutate(cons_alc = ifelse(P027== 1, 1, 0))


#Excesso de peso
pns2019.1 <- pns2019.1%>%mutate(IMC= W00103/((W00203*W00203)/10000))
pns2019.1 <- pns2019.1%>%mutate(exc_peso = ifelse(IMC>= 25, 1, 0))
pns2019.1 <- pns2019.1%>%mutate(fx_imc = cut(pns2019.1$IMC, 
                                           breaks = c(0, 18.5, 24.9, 29.9, Inf), 
                                           labels = c("abaixo do peso", "normal", "sobrepeso", "obesidade")))

#Diagnóstico médico autorreferido de hipertensão arterial  - Q002P 
pns2019.1 <- pns2019.1 %>% mutate(Q002P = Q00201)                                         
pns2019.1$Q002P<-factor(pns2019.1$Q002P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2019.1$Q002P)
pns2019.1 <- pns2019.1 %>% mutate(h_arterial = recode_factor(Q002P, "Sim" = 1, "Não" = 0))

#Diagnóstico médico autorreferido de diabetes - Q004P
pns2019.1 <- pns2019.1 %>% mutate(Q004P = Q03001)                                         
pns2019.1$Q004P<-factor(pns2019.1$Q004P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2019.1$Q004P)
pns2019.1 <- pns2019.1 %>% mutate(diabetes = recode_factor(Q004P, "Sim" = 1, "Não" = 0))

#Diagnóstico médico autorreferido de colesterol alto  - Q006P
pns2019.1 <- pns2019.1 %>% mutate(Q006P = Q060)
pns2019.1$Q006P<-factor(pns2019.1$Q006P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2019.1$Q006P)
pns2019.1 <- pns2019.1 %>% mutate(col_alto = recode_factor(Q006P, "Sim" = 1, "Não" = 0))

#Diagnóstico médico autorreferido de AVC (Acidente Vascular Cerebral) - Q008P
pns2019.1 <- pns2019.1 %>% mutate(Q008P = ifelse(Q068 == 1,1,2))
pns2019.1$Q008P<-factor(pns2019.1$Q008P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2019.1$Q008P)
pns2019.1 <- pns2019.1 %>% mutate(AVC = recode_factor(Q008P, "Sim" = 1, "Não" = 0))

#Diagnóstico autorreferido de depressão por profissional de saúde mental - Q014P
pns2019.1 <- pns2019.1 %>% mutate(Q014P = ifelse(Q092==1,1,2))
pns2019.1$Q014P<-factor(pns2019.1$Q014P, levels=c(1,2), labels=c("Sim","Não"))
summary(pns2019.1$Q014P)
pns2019.1 <- pns2019.1 %>% mutate(depr_ = recode_factor(Q014P, "Sim" = 1, "Não" = 0))

#Rendimento domiciliar per capita
pns2019.1 <- pns2019.1 %>% mutate(rend_per_capita = ifelse(VDF004 %in% 1:2, 1, 
                                                           ifelse(VDF004%in% 3, 2, 
                                                                  ifelse(VDF004%in% 4, 3,
                                                                         ifelse(VDF004%in% 5, 4,5)))))

pns2019.1$rend_per_capita<-factor(pns2019.1$rend_per_capita, levels=c(1,2,3,4,5), labels=c("Até 1/2 SM","1/2 até 1 SM","1 até 2 SM",
                                                                                           "2 até 3 SM","Mais de 3 SM"))
summary(pns2019.1$rend_per_capita)

pns2019.1 <- pns2019.1%>%rename(rend_dom = VDF002)
pns2019.1 <- pns2019.1%>%mutate(log_rendom = log(rend_dom))

#Escolaridade

pns2019.1 <- pns2019.1%>%rename(fx_esc = VDD004A)
pns2019.1 <- pns2019.1%>%mutate(fx_esc = recode_factor(fx_esc, 
                                                   "1" =	"Sem instrução",
                                                   "2" =	"Fundamental incompleto ou equivalente",
                                                   "3" =	"Fundamental completo ou equivalente",
                                                   "4" = 	"Médio incompleto ou equivalente",
                                                   "5" =  "Médio completo ou equivalente",
                                                   "6" =	"Superior incompleto ou equivalente",
                                                   "7" = 	"Superior completo" ))


pns2019.2 <- pns2019.1%>%select(ano, sexo, W00203, W00103, homem, sit_cens, regiao, urbano, idade,
                                fx_idade, IMC, fx_imc, cor, branco, fx_esc, est_civil, solteiro,
                                rend_per_capita,rend_dom, log_rendom, Q002P, Q004P, Q006P,
                                Q008P,Q014P, diabetes,h_arterial,col_alto, AVC, depr_, atv_fis, min_atv_fis, p_saude, fumo)

save(pns2019.2, file = "pns2019.2.RData")

gc()