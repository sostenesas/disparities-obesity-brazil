#Analise PNS 2013 e 2019

#Carregando bases selecionadas
#load("pns2013.2.RData")
#load("pns2019.2.RData")

#Juntando as bases
#df <- rbind(pns2013.2,pns2019.2)

#Salvar a base formada
#save(df, file = "pns_2013_2019.RData")

load("pns_2013_2019.RData")

#write.csv(df, file = "pns_2013_2019.csv")

# Objeto de desenho:
desPNSW13_19 = svydesign(id = ~UPA_PNS, strat = ~V0024, 
                         weight = ~peso_morador_selec, nest = TRUE, data = df)

df_weighted <- as.data.frame(desPNSW13_19$variables)

#Calculando a média do IMC para as faixas de idade e ano
media_imc1 <- aggregate(IMC ~ fx_idade + sexo + ano, data = df_weighted, FUN = mean)
media_imc2 <- aggregate(IMC ~ fx_idade + sit_cens + ano, data = df_weighted, FUN = mean)
media_imc3 <- aggregate(IMC ~ fx_idade + solteiro + ano , data = df_weighted, FUN = mean)
media_imc4 <- aggregate(IMC ~ fx_idade + branco + ano , data = df_weighted, FUN = mean)

#Plotando gráficos
ggplot(media_imc1, aes(x = fx_idade, y = IMC, 
                      colour = sexo, 
                      group = interaction(ano, sexo),
                      linetype = factor(ano))) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.5) +
  scale_color_hue(direction = 1) +
  scale_linetype_manual(values = c("2013" = "dashed", "2019" = "solid")) +
  theme_bw()+
  labs(x = "",
       y = "IMC médio",
       color = "Sexo",
       linetype = "Ano")+
  theme(legend.position = "bottom")

ggplot(media_imc2, aes(x = fx_idade, y = IMC, 
                      colour = sit_cens, 
                      group = interaction(ano, sit_cens),
                      linetype = factor(ano))) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.5) +
  scale_color_hue(direction = 1) +
  scale_linetype_manual(values = c("2013" = "dashed", "2019" = "solid")) +
  theme_bw()+
  labs(x = "",
       y = "IMC médio",
       color = "Situação censitária",
       linetype = "Ano")+
  theme(legend.position = "bottom")

ggplot(media_imc3, aes(x = fx_idade, y = IMC, 
                       colour = as.factor(solteiro), 
                       group = interaction(ano, solteiro),
                       linetype = factor(ano))) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.5) +
  scale_color_hue(direction = 1) +
  scale_linetype_manual(values = c("2013" = "dashed", "2019" = "solid")) +
  theme_bw()+
  labs(x = "",
       y = "IMC médio",
       color = "Solteiro",
       linetype = "Ano")+
  theme(legend.position = "bottom")

ggplot(media_imc4, aes(x = fx_idade, y = IMC, 
                       colour = as.factor(branco), 
                       group = interaction(ano, branco),
                       linetype = factor(ano))) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.5) +
  scale_color_hue(direction = 1) +
  scale_linetype_manual(values = c("2013" = "dashed", "2019" = "solid")) +
  theme_bw()+
  labs(x = "",
       y = "IMC médio",
       color = "Branco",
       linetype = "Ano")+
  theme(legend.position = "bottom")


ggplot(intervalos) +
  aes(x = fx_idade, y = media_imc, group = sexo, colour = sexo) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
  labs(title = "Média de IMC por idade e diagnóstico de doença crônica",
       x = "Idade",
       y = "Média de IMC") +
  theme_light()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

intervalos <- pns2019.1 %>%
  filter(!is.na(fx_idade))%>%
  group_by(sexo,fx_idade) %>%
  summarise(media_imc = mean(IMC),
            lower = mean(IMC) - qt(0.975, length(IMC) - 1) * sd(IMC) / sqrt(length(IMC)),
            upper = mean(IMC) + qt(0.975, length(IMC) - 1) * sd(IMC) / sqrt(length(IMC)))

ggplot(intervalos) +
  aes(x = fx_idade, y = media_imc, group = sexo, colour = sexo) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
  labs(title = "Média de IMC por idade e diagnóstico de doença crônica",
       x = "Idade",
       y = "Média de IMC") +
  theme_light()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 

intervalos |> esquisser()

t.test(dataset$imc, conf.level = 0.95)

intervalos <- df_weighted %>%
  filter(!is.na(IMC))%>%
  group_by(sexo,fx_idade, sit_cens, solteiro, branco) %>%
  summarise(media_imc = mean(IMC),
            lower = mean(IMC) - qt(0.975, length(IMC) - 1) * sd(IMC) / sqrt(length(IMC)),
            upper = mean(IMC) + qt(0.975, length(IMC) - 1) * sd(IMC) / sqrt(length(IMC)))



# Plotar o gráfico de linhas
ggplot(media_imc, aes(x = fx_idade, y = imc, group = solteiro1, color = solteiro1)) +
  geom_line() +
  geom_point() +
  labs(title = "Média de IMC por idade e status marital",
       x = "Idade",
       y = "Média de IMC") +
  theme_light()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 