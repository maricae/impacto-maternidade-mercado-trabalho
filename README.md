# Relação entre Maternidade e Remuneração das Mulheres no Mercado de Trabalho de Minas Gerais

Meu trabalho de monografia apresentado ao curso de Ciências Econômicas da Universidade Federal de Juiz de Fora, que analisa como a maternidade e o tempo de afastamento por licença impactam a remuneração das mulheres no setor privado de Minas Gerais (2007–2009). O estudo utiliza modelos para dados em painel e evidencia a penalidade salarial associada à maternidade.

Acesso ao trabalho completo: https://repositorio.ufjf.br/jspui/bitstream/ufjf/14455/1/marianacaetanovidal.pdf

## Dados Utilizados

Os dados são derivados da **RAIS identificada**, sem qualquer informação
pessoal.\
Principais variáveis utilizadas:

-   `rem_med_rs` --- remuneração média mensal
-   `sexo`
-   `licenca` e `afastamento_licenca`
-   `grau_instr`
-   `idade`, `temp_empr`
-   `chave`
-   anos: 2007, 2008 e 2009

A amostra final é um painel balanceado, contendo apenas indivíduos presentes nos três anos.

## Etapas do Projeto

1.  Carregamento e limpeza da base
2.  Tratamento de variáveis
3.  Filtragem da amostra
4.  Construção do painel
5.  Estatísticas descritivas
6.  Estimativa dos modelos
7.  Análise com defasagem (`licenca_t-1`)

## Modelos Estimados

Modelos do pacote *plm*:

-   Pooled OLS
-   Efeitos Fixos
-   Efeitos Aleatórios
-   Testes: F-test, Breusch-Pagan, Hausman

Variável dependente: `ln_salario`

## Requisitos

``` r
install.packages(c("plm", "dplyr"))
```

