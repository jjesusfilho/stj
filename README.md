
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stj

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jjesusfilho/stj?branch=master&svg=true)](https://ci.appveyor.com/project/jjesusfilho/stj)
[![Travis build
status](https://travis-ci.org/jjesusfilho/stj.svg?branch=master)](https://travis-ci.org/jjesusfilho/stj)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

O objetivo do pacote stj é baixar e organizar decisões do Superior
Tribunal de Justiça

## Instalação

Você pode instalar a versão em desenvolvimento com:

``` r
devtools::install_github("stj")
```

## Como usar

Para extrair dados de acórdãos do STJ, basta chamar a seguinte função:

``` r
df <- extrair_metadados_stj(
livre = "homicídio",
data_inicial = "01/01/2018",
data_final = "31/12/2018"
)
```

Uma vez obtido o dataframe com os metadados, você pode baixar os dados
dos processos individualmente considerados:

``` r
baixar_processo_stj(df$registro_stj, diretorio = ".")
```

Depois de baixados, você pode ler os detalhes:

``` r
detalhes <- ler_detalhes_stj(diretorio = ".")
```

Em seguida, ler o andamento processual:

``` r
fases <- ler_fases_stj(diretorio = ".")
```

Depois disso, você pode visualizar uma tabela com os títulos das
decisões e respectivos códigos:

``` r
metadocs <- ler_metadocs_stj(diretorio = ".")
```

Por fim, pode baixar essas decisões:

``` r
dir.create("pdfs")
baixar_pdfs_stj(metadocs$sequencial,diretorio = "pdfs")
```

## Código de conduta

Please note that the ‘stj’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.
