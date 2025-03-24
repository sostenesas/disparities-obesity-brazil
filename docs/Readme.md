# Inequalities in BMI, Overweight and Obesity in Brazil

Este repositório contém os materiais do artigo "Inequalities in BMI, Overweight and Obesity in Brazil: A Multimodel Analysis of Regional, Demographic and Health Factors".

## Estrutura do Repositório
- **manuscript/**: Manuscrito do artigo (PDF e fonte LaTeX/Word).
- **data/**: Dados utilizados (ou instruções para acesso, se não públicos).
- **code/**: Scripts para análise (regressão, decomposição Oaxaca-Blinder, PSM).
- **figures/**: Gráficos gerados para o artigo.
- **tables/**: Tabelas com resultados.
- **docs/**: Documentação adicional.
- **references/**: Arquivos de referências (BibTeX).

## Como Reproduzir os Resultados
1. Clone este repositório: `git clone https://github.com/sostenesas/inequalities-bmi-brazil.git`
2. Instale as dependências (e.g., R, pacotes como `tidyverse`, `oaxaca`, `MatchIt`).
3. Execute os scripts em `code/` na ordem: 'analise_pns_2013.R', 'analise_pns2019_R.R', 'analise_pns_2013_2019_mean.R' e 'analise_pns_2013_2019_expl.R' .
4. Os resultados serão gerados em `figures/` e `tables/`.

## Dados
Os dados são provenientes da Pesquisa Nacional de Saúde (PNS) 2013 e 2019. Devido a restrições de privacidade, os dados brutos não estão incluídos. Consulte `data/data_description.md` para instruções de acesso via IBGE.

## Licença
Este projeto está licenciado sob [MIT License](LICENSE).

## Contato
Para dúvidas, entre em contato com [sostenes.soeiro@gmail.com].
