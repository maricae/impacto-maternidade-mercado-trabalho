# Relação entre Maternidade e Penalidade Salarial: Análise Econométrica com Dados em Painel (RAIS)

Este projeto foi desenvolvido como minha monografia no curso de Ciências Econômicas da Universidade Federal de Juiz de Fora (UFJF).

O objetivo é investigar como a maternidade e o tempo de afastamento por licença impactam a remuneração das mulheres no setor privado de Minas Gerais, no período de 2007 a 2009.

Para isso, utilizo modelos econométricos para dados em painel, buscando estimar a penalidade salarial associada à maternidade e reduzir possíveis vieses decorrentes de características individuais não observáveis

Acesso ao trabalho completo: 
https://repositorio.ufjf.br/jspui/bitstream/ufjf/14455/1/marianacaetanovidal.pdf

## ➡️ Pergunta de Pesquisa

A maternidade impacta negativamente a remuneração das mulheres no mercado formal de trabalho?

Caso sim:

 - Esse efeito permanece após controlar características individuais?

- Existe persistência do impacto ao longo do tempo?

## ➡️ Base de Dados

Os dados são derivados da **RAIS identificada**, contendo informações sobre vínculos formais de trabalho.

Características da amostra:

- Painel balanceado (2007–2009)
- Mulheres do setor privado
- Remuneração média mensal (`rem_med_rs`)
- Indicadores de licença maternidade (`licenca`, `afastamento_licenca`)
- Escolaridade (`grau_instr`)
- Idade
- Tempo de emprego
- Identificador individual (`chave`)

A base foi tratada para garantir consistência longitudinal e permitir a construção do painel.

## ➡️ Metodologia

Para estimar o efeito da maternidade sobre os salários, foram utilizados modelos para dados em painel com o pacote `plm` no R.

## ➡️ Modelos Estimados

Modelos do pacote *plm*:

- Pooled OLS  
- Efeitos Fixos (Fixed Effects)  
- Efeitos Aleatórios (Random Effects)

### ➡️ Testes realizados:

- F-test para efeitos individuais  
- Breusch-Pagan LM  
- Teste de Hausman (para escolha entre FE e RE)

A variável dependente utilizada foi o logaritmo da remuneração (`ln_salario`).

Também foi incluída uma variável defasada de licença (`licenca_t-1`) para avaliar possíveis efeitos persistentes da maternidade sobre o salário.

## ➡️ Justificativa do Uso de Painel

Modelos de dados em painel permitem controlar heterogeneidade individual não observável — como habilidade, motivação ou preferências de carreira — que podem enviesar estimativas em análises puramente transversais.

O modelo de Efeitos Fixos, em particular, explora a variação dentro do próprio indivíduo ao longo do tempo, aproximando a análise de uma interpretação causal mais robusta.

## ➡️ Principais Resultados

- Evidência de penalidade salarial associada à maternidade.
- O efeito negativo permanece mesmo após controle por efeitos fixos individuais.
- A análise com defasagem sugere que o impacto pode se estender além do período imediato da licença.

Os resultados dialogam com a literatura sobre a “motherhood penalty” na economia do trabalho.

## ➡️ Limitações

- Horizonte temporal reduzido (3 anos)
- Possíveis limitações na variável de licença
- Ausência de controles explícitos para heterogeneidade ao nível da firma

Extensões futuras poderiam incluir períodos mais longos ou abordagens como difference-in-differences.

## ➡️ Tecnologias Utilizadas

- R  
- plm  
- dplyr  

```r
install.packages(c("plm", "dplyr"))
