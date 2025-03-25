# Disparities in Obesity Across Brazil

Este repositório contém os materiais do artigo "Disparities in Obesity Across Brazil: A Multimodel Analysis of Regional, Demographic, and Health Drivers".

## Estrutura do Repositório
- **manuscript/**: Manuscrito do artigo (word).
- **data/**: Dados utilizados.
- **code/**: Scripts para análise (regressão, decomposição Oaxaca-Blinder, PSM).
- **figures/**: Gráficos gerados para o artigo.
- **tables/**: Tabelas com resultados.
- **docs/**: Documentação adicional.
- **references/**: Arquivos de referências (BibTeX).

## Como Reproduzir os Resultados
1. Clone este repositório: `git clone https://github.com/sostenesas/inequalities-bmi-brazil.git`
2. Instale as dependências (e.g., R, pacotes como `tidyverse`, `oaxaca`, `MatchIt`).
3. Execute os scripts em `code/` na ordem: '1.analise_pns_2013.R', '2.analise_pns2019_R.R', '3.analise_pns_2013_2019_mean.R' e assim por diante.
4. Os resultados serão gerados em `figures/` e `tables/`.

## Dados
Os dados são provenientes da Pesquisa Nacional de Saúde (PNS) 2013 e 2019. Consulte `data/data_description.md` para instruções de acesso via IBGE.

## Licença
Este projeto está licenciado sob [MIT License](LICENSE).

## Contato
Para dúvidas, entre em contato com [sostenes.soeiro@gmail.com].
