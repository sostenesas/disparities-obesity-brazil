** Script: analise_pns_2013_2019.do
** Descrição: Este script realiza a análise dos dados combinados da PNS 2013 e 2019.
** Ele importa os dados, configura o desenho amostral, categoriza variáveis, realiza análises
** descritivas (proporções, correlações, gráficos de densidade), estima modelos de regressão,
** executa decomposição Oaxaca-Blinder e Propensity Score Matching (PSM), e salva os resultados
** em diretórios específicos (figures/ para gráficos e tables/ para tabelas).

** Limpeza do ambiente de trabalho
clear 
clear all
set more off  // Desativa a pausa automática entre os resultados

** Define os diretórios para salvar figuras e tabelas
* Cria os diretórios figures/ e tables/ se não existirem
cap mkdir "figures"
cap mkdir "tables"

** Importação dos dados
* Importa o arquivo CSV gerado anteriormente em R
import delimited "C:\Users\Sóstenes\OneDrive - Insper - Instituto de Ensino e Pesquisa\Documentos\Projeto diss\inequalities-bmi-brazil\data\pns_2013_2019.csv", encoding(utf8) 

** Configuração do survey (PNS 2013 e 2019)
* Define o desenho amostral com pesos (peso_morador_selec), estratos (v0024) e opções para variância
svyset [pweight=peso_morador_selec], strata(v0024) vce(linearized) singleunit(centered)

** Conversão de variáveis para numéricas e tratamento de valores ausentes
* Converte variáveis que podem estar como string para numéricas, forçando a conversão e tratando valores ausentes
destring imc, force replace
destring rend_dom, force replace
destring log_rendom, force replace
destring solteiro, force replace
destring branco, force replace
destring diabetes, force replace
destring h_arterial, force replace
destring col_alto, force replace
destring avc, force replace
destring depr_, force replace

** Criação de variáveis dummy para o ano
* Cria uma dummy para o ano base (1 para 2013, 0 para 2019)
gen ano_base = 1 if ano == 2013
replace ano_base = 0 if ano == 2019

** Criação de variáveis para categorias de IMC
* Cria uma dummy para excesso de peso (1 se fx_imc for "obesidade" ou "sobrepeso", 0 se "normal")
gen exc_peso = 1 if fx_imc == "obesidade" | fx_imc == "sobrepeso"
replace exc_peso = 0 if fx_imc == "normal"

* Cria uma dummy para obesidade (1 se fx_imc for "obesidade", 0 caso contrário)
gen obesity = 1 if fx_imc == "obesidade"
replace obesity = 0 if obesity == .

** Categorização da região
* Converte a variável regiao (string) em uma variável numérica (regiao_)
gen regiao_ = 1 if regiao == "Norte"
replace regiao_ = 2 if regiao == "Nordeste"
replace regiao_ = 3 if regiao == "Sul"
replace regiao_ = 4 if regiao == "Sudeste"
replace regiao_ = 5 if regiao == "Centro-Oeste"

** Categorização da faixa etária
* Converte a variável idade em faixas etárias numéricas (fx_idade_)
gen fx_idade_ = 1 if idade >= 18 & idade <= 29
replace fx_idade_ = 2 if idade >= 30 & idade <= 44
replace fx_idade_ = 3 if idade >= 45 & idade <= 59
replace fx_idade_ = 4 if idade >= 60 & idade <= 74
replace fx_idade_ = 5 if idade >= 75

** Categorização da escolaridade
* Converte a variável fx_esc (string) em uma variável numérica (fx_esc_)
gen fx_esc_ = 1 if fx_esc == "Sem instrução"
replace fx_esc_ = 2 if fx_esc == "Fundamental incompleto"
replace fx_esc_ = 3 if fx_esc == "Fundamental completo"
replace fx_esc_ = 4 if fx_esc == "Médio incompleto"
replace fx_esc_ = 5 if fx_esc == "Médio completo"
replace fx_esc_ = 6 if fx_esc == "Superior incompleto"
replace fx_esc_ = 7 if fx_esc == "Superior completo"

** Categorização da renda per capita
* Converte a variável rend_per_capita (string) em uma variável numérica (rend_per_capita_)
gen rend_per_capita_ = 1 if rend_per_capita == "Até 1/2 SM"
replace rend_per_capita_ = 2 if rend_per_capita == "1/2 até 1 SM"
replace rend_per_capita_ = 3 if rend_per_capita == "1 até 2 SM"
replace rend_per_capita_ = 4 if rend_per_capita == "2 até 3 SM"
replace rend_per_capita_ = 5 if rend_per_capita == "Mais de 3 SM"

** Categorização de cor
* Converte a variável cor (string) em uma variável numérica (cor_)
gen cor_ = 1 if cor == "Branco"
replace cor_ = 2 if cor == "Negro"
replace cor_ = 3 if cor == "Amarelo"
replace cor_ = 4 if cor == "Pardo"
replace cor_ = 5 if cor == "Indígena"

** Categorização de estado civil
* Converte a variável est_civil (string) em uma variável numérica (est_civil_)
gen est_civil_ = 1 if est_civil == "Solteiro(a)"
replace est_civil_ = 2 if est_civil == "Casado(a)"
replace est_civil_ = 3 if est_civil == "Separado(a)*"
replace est_civil_ = 4 if est_civil == "Viúvo(a)"

** Categorização do IMC
* Converte a variável fx_imc (string) em uma variável numérica (fx_imc_)
gen fx_imc_ = 1 if fx_imc == "abaixo do peso"
replace fx_imc_ = 2 if fx_imc == "normal"
replace fx_imc_ = 3 if fx_imc == "sobrepeso"
replace fx_imc_ = 4 if fx_imc == "obesidade"

** Categorização do IMC para obesidade (níveis de obesidade)
* Cria h_BMI para níveis de sobrepeso/obesidade com base no IMC
gen h_BMI = 1 if imc >= 25 & imc < 29.999  // Sobrepeso
replace h_BMI = 2 if imc >= 30 & imc < 34.999  // Obesidade grau I
replace h_BMI = 3 if imc >= 35 & imc < 39.999  // Obesidade grau II
replace h_BMI = 4 if imc >= 40  // Obesidade grau III

* Cria h_BMI_ como dummy (1 para obesidade, 0 para sobrepeso)
gen h_BMI_ = 0 if imc >= 25 & imc < 29.999  // Sobrepeso
replace h_BMI_ = 1 if imc >= 30  // Obesidade

** Estimar proporções
* Calcula proporções ponderadas de variáveis categóricas usando o desenho amostral
svy: prop homem regiao_ fx_esc_ cor_ est_civil_ rend_per_capita_

** Exportação dos resultados para Excel
* Salva as proporções em um arquivo Excel no diretório tables/
estimates store prop1

outreg2 [prop1] using "tables/prop1.xls", excel replace

** Análise de correlação entre variáveis
* Calcula a matriz de correlação entre variáveis de interesse
corr exc_peso regiao_ homem solteiro branco idade urbano fx_esc_ rend_dom ///
log_rendom rend_per_capita_ total_morador internet diabetes h_arterial /// 
col_alto avc depr_ atv_fis fumo p_saude

* Salva a matriz de correlação
matrix C = r(C)

* Opcional: Visualiza a matriz de correlação
matrix list C

** Gráfico de calor das correlações
* Gera um mapa de calor da matriz de correlação usando a paleta viridis
heatplot C, color(viridis) title("Mapa de Calor - Correlação das Variáveis")
* Salva o gráfico no diretório figures/
graph export "figures/heatmap_correlation.png", replace

** Gráficos de densidade do IMC por diferentes categorias
* Gráfico 1: Densidade do IMC por sexo (homens vs. mulheres)
twoway (kdensity imc if homem == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if homem == 0, color(red%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Homens" 2 "Mulheres")) ///
title("Distribuição do IMC por Sexo") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/
graph export "figures/density_imc_sex.png", replace

* Gráfico 2: Densidade do IMC por cor/raça (brancos vs. não-brancos)
twoway (kdensity imc if branco == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if branco == 0, color(red%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Brancos" 2 "Não-brancos")) ///
title("Distribuição do IMC por Cor/Raça") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/
graph export "figures/density_imc_race.png", replace

* Gráfico 3: Densidade do IMC por estado civil (solteiros vs. não-solteiros)
twoway (kdensity imc if solteiro == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if solteiro == 0, color(red%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Solteiros" 2 "Não-solteiros")) ///
title("Distribuição do IMC por Estado Civil") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/
graph export "figures/density_imc_marital_status.png", replace

* Gráfico 4: Densidade do IMC por situação censitária (urbano vs. rural)
twoway (kdensity imc if urbano == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if urbano == 0, color(red%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Urbano" 2 "Rural")) ///
title("Distribuição do IMC por Situação Censitária") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/
graph export "figures/density_imc_urban_rural.png", replace

* Gráfico 5: Densidade do IMC com múltiplas categorias (sexo, cor/raça, estado civil, situação censitária)
twoway (kdensity imc if homem == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if homem == 0, color(blue%40) lwidth(medium) lpattern(dash)) ///
       (kdensity imc if branco == 1, color(green%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if branco == 0, color(green%40) lwidth(medium) lpattern(dash)) ///
       (kdensity imc if solteiro == 1, color(red%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if solteiro == 0, color(red%40) lwidth(medium) lpattern(dash)) ///
       (kdensity imc if urbano == 1, color(gray%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if urbano == 0, color(gray%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Homens" 2 "Mulheres" 3 "Branco" 4 "Não-branco" 5 "Solteiro" 6 "Não-solteiro" 7 "Urbano" 8 "Rural")) ///
title("Distribuição do IMC por Múltiplas Categorias") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/
graph export "figures/density_imc_multiple_categories.png", replace

** Modelos de regressão
* Modelo 1: Regressão linear para IMC (variável contínua)
svy: reg imc i.regiao_ homem solteiro urbano branco idade i.fx_idade_ ///
i.fx_esc_ rend_dom log_rendom i.rend_per_capita_ total_morador internet diabetes ///
h_arterial col_alto avc depr_ atv_fis fumo p_saude ano_base
estimates store model1

* Modelo 2: Regressão logística multinomial para faixas de IMC (fx_imc_)
svy: mlogit fx_imc_ i.regiao_ homem solteiro urbano branco idade i.fx_idade_ ///
i.fx_esc_ rend_dom log_rendom i.rend_per_capita_ total_morador internet diabetes ///
h_arterial col_alto avc depr_ atv_fis fumo p_saude ano_base, baseoutcome(2) rrr
estimates store model2

* Modelo 3: Regressão logística para excesso de peso (exc_peso)
svy: logistic exc_peso i.regiao_ homem solteiro urbano branco idade i.fx_idade_ ///
i.fx_esc_ rend_dom log_rendom i.rend_per_capita_ total_morador internet diabetes ///
h_arterial col_alto avc depr_ atv_fis fumo p_saude ano_base
estimates store model3

* Modelo 4: Regressão logística multinomial para níveis de sobrepeso/obesidade (h_BMI)
svy: mlogit h_BMI i.regiao_ homem solteiro urbano branco idade i.fx_idade_ ///
i.fx_esc_ rend_dom log_rendom i.rend_per_capita_ total_morador internet diabetes ///
h_arterial col_alto avc depr_ atv_fis fumo p_saude ano_base, baseoutcome(1) rrr
estimates store model4

** Exportação dos resultados para Excel
* Salva os resultados dos modelos de regressão em um arquivo Excel no diretório tables/
outreg2 [model1 model2 model3 model4] using "tables/resultados.xls", excel replace

** Análise de decomposição (Oaxaca)
* Decomposição 1: Diferenças em excesso de peso entre homens e mulheres
oaxaca exc_peso regiao_ branco solteiro idade urbano fx_esc_ rend_dom log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(homem) logit pooled svy eform

* Decomposição 2: Diferenças em excesso de peso entre brancos e não-brancos
oaxaca exc_peso regiao_ homem solteiro idade urbano fx_esc_ rend_dom log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(branco) logit pooled svy eform swap

* Decomposição 3: Diferenças em excesso de peso entre solteiros e não-solteiros
oaxaca exc_peso regiao_ homem branco idade urbano fx_esc_ rend_dom log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(solteiro) logit pooled svy eform

* Decomposição 4: Diferenças em obesidade (h_BMI_) entre homens e mulheres
oaxaca h_BMI_ ano_base regiao_ solteiro branco idade urbano fx_esc_ log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(homem) logit pooled svy eform
estimates store oaxaca1

* Decomposição 5: Diferenças em obesidade (h_BMI_) entre solteiros e não-solteiros
oaxaca h_BMI_ ano_base regiao_ homem branco idade urbano fx_esc_ log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(solteiro) logit pooled svy eform
estimates store oaxaca2

* Decomposição 6: Diferenças em obesidade (h_BMI_) entre brancos e não-brancos
oaxaca h_BMI_ ano_base regiao_ homem solteiro idade urbano fx_esc_ log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(branco) logit pooled svy eform
estimates store oaxaca3

** Exportação dos resultados da decomposição para Excel
* Salva os resultados da decomposição Oaxaca-Blinder em um arquivo Excel no diretório tables/
outreg2 [oaxaca1 oaxaca2 oaxaca3] using "tables/oaxaca.xls", excel replace

** Script: code/analysis_pns_2013_2019.do
** Descrição: Este script realiza a análise dos dados combinados da PNS 2013 e 2019.
** Ele importa os dados, configura o desenho amostral, categoriza variáveis, realiza análises
** descritivas (proporções, correlações, gráficos de densidade), estima modelos de regressão,
** executa decomposição Oaxaca-Blinder e Propensity Score Matching (PSM), e salva os resultados
** em diretórios específicos (figures/ para gráficos e tables/ para tabelas).

** Limpeza do ambiente de trabalho
clear 
clear all
set more off  // Desativa a pausa automática entre os resultados

** Define os diretórios para salvar figuras e tabelas
* Cria os diretórios figures/ e tables/ na raiz do projeto (um nível acima de code/)
cap mkdir "../figures"
cap mkdir "../tables"

** Importação dos dados
* Importa o arquivo CSV da pasta data/, que está na raiz do projeto
import delimited "C:\Users\Sóstenes\OneDrive - Insper - Instituto de Ensino e Pesquisa\Documentos\Projeto diss\inequalities-bmi-brazil\data\pns_2013_2019.csv", encoding(utf8) 

** Configuração do survey (PNS 2013 e 2019)
* Define o desenho amostral com pesos (peso_morador_selec), estratos (v0024) e opções para variância
svyset [pweight=peso_morador_selec], strata(v0024) vce(linearized) singleunit(centered)

** Conversão de variáveis para numéricas e tratamento de valores ausentes
* Converte variáveis que podem estar como string para numéricas, forçando a conversão e tratando valores ausentes
destring imc, force replace
destring rend_dom, force replace
destring log_rendom, force replace
destring solteiro, force replace
destring branco, force replace
destring diabetes, force replace
destring h_arterial, force replace
destring col_alto, force replace
destring avc, force replace
destring depr_, force replace

** Criação de variáveis dummy para o ano
* Cria uma dummy para o ano base (1 para 2013, 0 para 2019)
gen ano_base = 1 if ano == 2013
replace ano_base = 0 if ano == 2019

** Criação de variáveis para categorias de IMC
* Cria uma dummy para excesso de peso (1 se fx_imc for "obesidade" ou "sobrepeso", 0 se "normal")
gen exc_peso = 1 if fx_imc == "obesidade" | fx_imc == "sobrepeso"
replace exc_peso = 0 if fx_imc == "normal"

* Cria uma dummy para obesidade (1 se fx_imc for "obesidade", 0 caso contrário)
gen obesity = 1 if fx_imc == "obesidade"
replace obesity = 0 if obesity == .

** Categorização da região
* Converte a variável regiao (string) em uma variável numérica (regiao_)
gen regiao_ = 1 if regiao == "Norte"
replace regiao_ = 2 if regiao == "Nordeste"
replace regiao_ = 3 if regiao == "Sul"
replace regiao_ = 4 if regiao == "Sudeste"
replace regiao_ = 5 if regiao == "Centro-Oeste"

** Categorização da faixa etária
* Converte a variável idade em faixas etárias numéricas (fx_idade_)
gen fx_idade_ = 1 if idade >= 18 & idade <= 29
replace fx_idade_ = 2 if idade >= 30 & idade <= 44
replace fx_idade_ = 3 if idade >= 45 & idade <= 59
replace fx_idade_ = 4 if idade >= 60 & idade <= 74
replace fx_idade_ = 5 if idade >= 75

** Categorização da escolaridade
* Converte a variável fx_esc (string) em uma variável numérica (fx_esc_)
gen fx_esc_ = 1 if fx_esc == "Sem instrução"
replace fx_esc_ = 2 if fx_esc == "Fundamental incompleto"
replace fx_esc_ = 3 if fx_esc == "Fundamental completo"
replace fx_esc_ = 4 if fx_esc == "Médio incompleto"
replace fx_esc_ = 5 if fx_esc == "Médio completo"
replace fx_esc_ = 6 if fx_esc == "Superior incompleto"
replace fx_esc_ = 7 if fx_esc == "Superior completo"

** Categorização da renda per capita
* Converte a variável rend_per_capita (string) em uma variável numérica (rend_per_capita_)
gen rend_per_capita_ = 1 if rend_per_capita == "Até 1/2 SM"
replace rend_per_capita_ = 2 if rend_per_capita == "1/2 até 1 SM"
replace rend_per_capita_ = 3 if rend_per_capita == "1 até 2 SM"
replace rend_per_capita_ = 4 if rend_per_capita == "2 até 3 SM"
replace rend_per_capita_ = 5 if rend_per_capita == "Mais de 3 SM"

** Categorização de cor
* Converte a variável cor (string) em uma variável numérica (cor_)
gen cor_ = 1 if cor == "Branco"
replace cor_ = 2 if cor == "Negro"
replace cor_ = 3 if cor == "Amarelo"
replace cor_ = 4 if cor == "Pardo"
replace cor_ = 5 if cor == "Indígena"

** Categorização de estado civil
* Converte a variável est_civil (string) em uma variável numérica (est_civil_)
gen est_civil_ = 1 if est_civil == "Solteiro(a)"
replace est_civil_ = 2 if est_civil == "Casado(a)"
replace est_civil_ = 3 if est_civil == "Separado(a)*"
replace est_civil_ = 4 if est_civil == "Viúvo(a)"

** Categorização do IMC
* Converte a variável fx_imc (string) em uma variável numérica (fx_imc_)
gen fx_imc_ = 1 if fx_imc == "abaixo do peso"
replace fx_imc_ = 2 if fx_imc == "normal"
replace fx_imc_ = 3 if fx_imc == "sobrepeso"
replace fx_imc_ = 4 if fx_imc == "obesidade"

** Categorização do IMC para obesidade (níveis de obesidade)
* Cria h_BMI para níveis de sobrepeso/obesidade com base no IMC
gen h_BMI = 1 if imc >= 25 & imc < 29.999  // Sobrepeso
replace h_BMI = 2 if imc >= 30 & imc < 34.999  // Obesidade grau I
replace h_BMI = 3 if imc >= 35 & imc < 39.999  // Obesidade grau II
replace h_BMI = 4 if imc >= 40  // Obesidade grau III

* Cria h_BMI_ como dummy (1 para obesidade, 0 para sobrepeso)
gen h_BMI_ = 0 if imc >= 25 & imc < 29.999  // Sobrepeso
replace h_BMI_ = 1 if imc >= 30  // Obesidade

** Estimar proporções
* Calcula proporções ponderadas de variáveis categóricas usando o desenho amostral
svy: prop homem regiao_ fx_esc_ cor_ est_civil_ rend_per_capita_

** Exportação dos resultados para Excel
* Salva as proporções em um arquivo Excel no diretório tables/ (na raiz do projeto)
estimates store prop1

outreg2 [prop1] using "../tables/prop1.xls", excel replace

** Análise de correlação entre variáveis
* Calcula a matriz de correlação entre variáveis de interesse
corr exc_peso regiao_ homem solteiro branco idade urbano fx_esc_ rend_dom ///
log_rendom rend_per_capita_ total_morador internet diabetes h_arterial /// 
col_alto avc depr_ atv_fis fumo p_saude

* Salva a matriz de correlação
matrix C = r(C)

* Opcional: Visualiza a matriz de correlação
matrix list C

** Gráfico de calor das correlações
* Gera um mapa de calor da matriz de correlação usando a paleta viridis
heatplot C, color(viridis) title("Mapa de Calor - Correlação das Variáveis")
* Salva o gráfico no diretório figures/ (na raiz do projeto)
graph export "../figures/heatmap_correlation.png", replace

** Gráficos de densidade do IMC por diferentes categorias
* Gráfico 1: Densidade do IMC por sexo (homens vs. mulheres)
twoway (kdensity imc if homem == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if homem == 0, color(red%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Homens" 2 "Mulheres")) ///
title("Distribuição do IMC por Sexo") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/ (na raiz do projeto)
graph export "../figures/density_imc_sex.png", replace

* Gráfico 2: Densidade do IMC por cor/raça (brancos vs. não-brancos)
twoway (kdensity imc if branco == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if branco == 0, color(red%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Brancos" 2 "Não-brancos")) ///
title("Distribuição do IMC por Cor/Raça") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/ (na raiz do projeto)
graph export "../figures/density_imc_race.png", replace

* Gráfico 3: Densidade do IMC por estado civil (solteiros vs. não-solteiros)
twoway (kdensity imc if solteiro == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if solteiro == 0, color(red%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Solteiros" 2 "Não-solteiros")) ///
title("Distribuição do IMC por Estado Civil") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/ (na raiz do projeto)
graph export "../figures/density_imc_marital_status.png", replace

* Gráfico 4: Densidade do IMC por situação censitária (urbano vs. rural)
twoway (kdensity imc if urbano == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if urbano == 0, color(red%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Urbano" 2 "Rural")) ///
title("Distribuição do IMC por Situação Censitária") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/ (na raiz do projeto)
graph export "../figures/density_imc_urban_rural.png", replace

* Gráfico 5: Densidade do IMC com múltiplas categorias (sexo, cor/raça, estado civil, situação censitária)
twoway (kdensity imc if homem == 1, color(blue%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if homem == 0, color(blue%40) lwidth(medium) lpattern(dash)) ///
       (kdensity imc if branco == 1, color(green%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if branco == 0, color(green%40) lwidth(medium) lpattern(dash)) ///
       (kdensity imc if solteiro == 1, color(red%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if solteiro == 0, color(red%40) lwidth(medium) lpattern(dash)) ///
       (kdensity imc if urbano == 1, color(gray%40) lwidth(medium) lpattern(solid)) ///
       (kdensity imc if urbano == 0, color(gray%40) lwidth(medium) lpattern(dash)), ///
legend(order(1 "Homens" 2 "Mulheres" 3 "Branco" 4 "Não-branco" 5 "Solteiro" 6 "Não-solteiro" 7 "Urbano" 8 "Rural")) ///
title("Distribuição do IMC por Múltiplas Categorias") xlabel(10(5)50) ylabel(,grid)
* Salva o gráfico no diretório figures/ (na raiz do projeto)
graph export "../figures/density_imc_multiple_categories.png", replace

** Modelos de regressão
* Modelo 1: Regressão linear para IMC (variável contínua)
svy: reg imc i.regiao_ homem solteiro urbano branco idade i.fx_idade_ ///
i.fx_esc_ rend_dom log_rendom i.rend_per_capita_ total_morador internet diabetes ///
h_arterial col_alto avc depr_ atv_fis fumo p_saude ano_base
estimates store model1

* Modelo 2: Regressão logística multinomial para faixas de IMC (fx_imc_)
svy: mlogit fx_imc_ i.regiao_ homem solteiro urbano branco idade i.fx_idade_ ///
i.fx_esc_ rend_dom log_rendom i.rend_per_capita_ total_morador internet diabetes ///
h_arterial col_alto avc depr_ atv_fis fumo p_saude ano_base, baseoutcome(2) rrr
estimates store model2

* Modelo 3: Regressão logística para excesso de peso (exc_peso)
svy: logistic exc_peso i.regiao_ homem solteiro urbano branco idade i.fx_idade_ ///
i.fx_esc_ rend_dom log_rendom i.rend_per_capita_ total_morador internet diabetes ///
h_arterial col_alto avc depr_ atv_fis fumo p_saude ano_base
estimates store model3

* Modelo 4: Regressão logística multinomial para níveis de sobrepeso/obesidade (h_BMI)
svy: mlogit h_BMI i.regiao_ homem solteiro urbano branco idade i.fx_idade_ ///
i.fx_esc_ rend_dom log_rendom i.rend_per_capita_ total_morador internet diabetes ///
h_arterial col_alto avc depr_ atv_fis fumo p_saude ano_base, baseoutcome(1) rrr
estimates store model4

** Exportação dos resultados para Excel
* Salva os resultados dos modelos de regressão em um arquivo Excel no diretório tables/ (na raiz do projeto)
outreg2 [model1 model2 model3 model4] using "../tables/resultados.xls", excel replace

** Análise de decomposição (Oaxaca)
* Decomposição 1: Diferenças em excesso de peso entre homens e mulheres
oaxaca exc_peso regiao_ branco solteiro idade urbano fx_esc_ rend_dom log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(homem) logit pooled svy eform

* Decomposição 2: Diferenças em excesso de peso entre brancos e não-brancos
oaxaca exc_peso regiao_ homem solteiro idade urbano fx_esc_ rend_dom log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(branco) logit pooled svy eform swap

* Decomposição 3: Diferenças em excesso de peso entre solteiros e não-solteiros
oaxaca exc_peso regiao_ homem branco idade urbano fx_esc_ rend_dom log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(solteiro) logit pooled svy eform

* Decomposição 4: Diferenças em obesidade (h_BMI_) entre homens e mulheres
oaxaca h_BMI_ ano_base regiao_ solteiro branco idade urbano fx_esc_ log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(homem) logit pooled svy eform
estimates store oaxaca1

* Decomposição 5: Diferenças em obesidade (h_BMI_) entre solteiros e não-solteiros
oaxaca h_BMI_ ano_base regiao_ homem branco idade urbano fx_esc_ log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(solteiro) logit pooled svy eform
estimates store oaxaca2

* Decomposição 6: Diferenças em obesidade (h_BMI_) entre brancos e não-brancos
oaxaca h_BMI_ ano_base regiao_ homem solteiro idade urbano fx_esc_ log_rendom ///
rend_per_capita_ total_morador internet diabetes h_arterial col_alto avc depr_ ///
atv_fis fumo p_saude, by(branco) logit pooled svy eform
estimates store oaxaca3

** Exportação dos resultados da decomposição para Excel
* Salva os resultados da decomposição Oaxaca-Blinder em um arquivo Excel no diretório tables/ (na raiz do projeto)
outreg2 [oaxaca1 oaxaca2 oaxaca3] using "../tables/oaxaca.xls", excel replace

** Análise de Propensity Score Matching (PSM) para diferença entre anos - educ_bin

** Definir variáveis binárias
* Cria variável binária de escolaridade (educ_bin): 1 se fx_esc_ >= 3 (Fundamental completo ou mais), 0 caso contrário
gen educ_bin = (fx_esc_ >= 3) if !missing(fx_esc_)
* Cria variável binária de renda (renda_bin): 1 se log_rendom > mediana, 0 caso contrário
gen renda_bin = (log_rendom > r(p50)) if !missing(log_rendom)

** Filtrar observações sem valores ausentes
* Remove observações com valores ausentes em variáveis-chave para garantir integridade da análise
keep if !missing(exc_peso, educ_bin, log_rendom, idade, homem, regiao_, solteiro, branco, total_morador, internet, urbano, diabetes, h_arterial, col_alto, atv_fis, avc, fumo, p_saude, ano_base)

** Salva base completa temporariamente
* Salva a base filtrada para uso em análises separadas (geral, homens, mulheres)
* Usa caminho relativo para acessar o diretório data/ na raiz do projeto
save "../data/base_completa.dta", replace

** PSM para educ_bin - Controle Inter-ano (Amostra Geral) **
* Carrega a base salva, usando caminho relativo
use "../data/base_completa.dta", clear

** Modelo logit com ano_base como covariável
* Estima propensity score com logit ajustado para survey, incluindo todas as covariáveis e ano_base
svy: logit educ_bin log_rendom idade homem regiao_ solteiro branco total_morador internet urbano diabetes h_arterial col_alto atv_fis avc fumo p_saude ano_base
* Prevê o propensity score para cada observação
predict pscore_educ_inter, pr

** Aplica PSM
* Executa PSM com 2 vizinhos mais próximos, caliper de 0.07, usando pscore calculado
psmatch2 educ_bin, out(exc_peso) pscore(pscore_educ_inter) neighbor(2) caliper(0.07)
* Cria indicador para amostra pareada (1 se observação foi pareada, 0 caso contrário)
gen matched_sample_inter = (_weight != . & _weight != 0)
* Mantém apenas as observações pareadas
keep if matched_sample_inter
* Conta o tamanho da amostra pareada conjunta
count  

** Teste de balanceamento pós-pareamento (Amostra Geral)
* Avalia se as covariáveis estão balanceadas entre tratados (educ_bin = 1) e controles (educ_bin = 0)
pstest log_rendom idade homem regiao_ solteiro branco total_morador internet urbano diabetes h_arterial col_alto atv_fis avc fumo p_saude ano_base, treated(educ_bin)

** Ajuste do efeito com survey para diferença entre anos
* Calcula médias de exc_peso ajustadas por survey, estratificadas por educ_bin e ano_base
svy, subpop(matched_sample_inter): mean exc_peso, over(educ_bin ano_base)

** Calcula efeitos usando os rótulos corretos (_subpop_X)
* Efeito da escolaridade em 2013: alta escolaridade (subpop_4) - baixa escolaridade (subpop_2)
lincom [exc_peso]_subpop_4 - [exc_peso]_subpop_2  
* Efeito da escolaridade em 2019: alta escolaridade (subpop_3) - baixa escolaridade (subpop_1)
lincom [exc_peso]_subpop_3 - [exc_peso]_subpop_1  
* Diferença inter-ano: (efeito 2013) - (efeito 2019)
lincom ([exc_peso]_subpop_4 - [exc_peso]_subpop_2) - ([exc_peso]_subpop_3 - [exc_peso]_subpop_1)  

** PSM para Homens **
* Carrega a base salva, usando caminho relativo
use "../data/base_completa.dta", clear
* Filtra apenas homens (homem = 1)
keep if homem == 1  

** Modelo logit para homens
* Estima propensity score com logit ajustado para survey, apenas para homens
svy: logit educ_bin log_rendom idade regiao_ solteiro branco total_morador internet urbano diabetes h_arterial col_alto atv_fis avc fumo p_saude ano_base
* Prevê o propensity score para homens
predict pscore_men, pr

** Aplica PSM para homens
* Executa PSM com 2 vizinhos mais próximos, caliper de 0.07
psmatch2 educ_bin, out(exc_peso) pscore(pscore_men) neighbor(2) caliper(0.07)
* Cria indicador para amostra pareada de homens
gen matched_men = (_weight != . & _weight != 0)
* Mantém apenas as observações pareadas
keep if matched_men

** Teste de balanceamento pós-pareamento (Homens)
* Avalia balanceamento das covariáveis entre tratados e controles para homens
pstest log_rendom idade regiao_ solteiro branco total_morador internet urbano diabetes h_arterial col_alto atv_fis avc fumo p_saude ano_base, treated(educ_bin)

** Ajuste do efeito para homens
* Calcula médias de exc_peso ajustadas por survey para homens
svy, subpop(matched_men): mean exc_peso, over(educ_bin ano_base)
* Efeito em 2013 para homens
lincom [exc_peso]_subpop_4 - [exc_peso]_subpop_2  
* Efeito em 2019 para homens
lincom [exc_peso]_subpop_3 - [exc_peso]_subpop_1  
* Diferença inter-ano para homens
lincom ([exc_peso]_subpop_4 - [exc_peso]_subpop_2) - ([exc_peso]_subpop_3 - [exc_peso]_subpop_1)

** PSM para Mulheres **
* Carrega a base salva, usando caminho relativo
use "../data/base_completa.dta", clear
* Filtra apenas mulheres (homem = 0)
keep if homem == 0  

** Modelo logit para mulheres
* Estima propensity score com logit ajustado para survey, apenas para mulheres
svy: logit educ_bin log_rendom idade regiao_ solteiro branco total_morador internet urbano diabetes h_arterial col_alto atv_fis avc fumo p_saude ano_base
* Prevê o propensity score para mulheres
predict pscore_women, pr

** Aplica PSM para mulheres
* Executa PSM com 2 vizinhos mais próximos, caliper de 0.07
psmatch2 educ_bin, out(exc_peso) pscore(pscore_women) neighbor(2) caliper(0.07)
* Cria indicador para amostra pareada de mulheres
gen matched_women = (_weight != . & _weight != 0)
* Mantém apenas as observações pareadas
keep if matched_women

** Teste de balanceamento pós-pareamento (Mulheres)
* Avalia balanceamento das covariáveis entre tratados e controles para mulheres
pstest log_rendom idade regiao_ solteiro branco total_morador internet urbano diabetes h_arterial col_alto atv_fis avc fumo p_saude ano_base, treated(educ_bin)

** Ajuste do efeito para mulheres
* Calcula médias de exc_peso ajustadas por survey para mulheres
svy, subpop(matched_women): mean exc_peso, over(educ_bin ano_base)
* Efeito em 2013 para mulheres
lincom [exc_peso]_subpop_4 - [exc_peso]_subpop_2  
* Efeito em 2019 para mulheres
lincom [exc_peso]_subpop_3 - [exc_peso]_subpop_1  
* Diferença inter-ano para mulheres
lincom ([exc_peso]_subpop_4 - [exc_peso]_subpop_2) - ([exc_peso]_subpop_3 - [exc_peso]_subpop_1)
