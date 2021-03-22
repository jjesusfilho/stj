
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

Recomenda-se iniciar pela função `stj_baixar_cjsg`, pela qual você pode
baixar tanto os metadados das decisões colegiadas, quanto os metadados e
o próprio inteiro teor da decisão monocrática.

### Para baixar acórdãos:

``` r
dir.create("jurisprudencia")

stj_baixar_cjsg(
livre = "homicídio",
data_inicial = "01/01/2018",
data_final = "31/12/2018"
base = "ACOR"
diretorio = "jurisprudencia"
)
```

### Para baixar decisões monocráticas:

``` r
dir.create("monocraticas")

stj_baixar_cjsg(
livre = "homicídio",
data_inicial = "01/01/2018",
data_final = "31/12/2018"
base = "MONO"
diretorio = "monocraticas"
)
```

### Leitura das decisões

Depois disso, basta ler as decisões em um dataframe:

``` r
jurisprudencia <- stj_ler_cjsg_acor(diretorio = "urisprudencia")

monocraticas <- stj_ler_cjsg_mono(diretorio = "monocraticas")
```

Uma vez obtido o dataframe com os metadados, você pode baixar os dados
dos processos individualmente considerados:

``` r
dir.create("processos")
baixar_processo_stj(jurisprudencia$registro_stj, diretorio = "processos")
```

Você pode usar usar tanto o número do registro, quanto o número do CNJ,
este conforme resolução 65/2008. A função dá conta de distinguir as
situação e realizar a busca corretamente.

Você pode optar por baixar também os documentos relativos ao processo,
tais como ementa, inteiro teor etc. Eles serão baixados em html. Ainda
não avaliei se vale a pena incluir a opção de pdf. Os nomes dos
documentos terão a palavra documento, seguido do código sequencial:

``` r
baixar_processo_stj(df$registro_stj, diretorio = "processos", documentos = TRUE)
```

Depois de baixados, você pode ler os detalhes:

``` r
detalhes <- ler_detalhes_stj(diretorio = "processos")
```

Em seguida, ler o andamento processual:

``` r
fases <- ler_fases_stj(diretorio = "processos")
```

Caso você queira saber ao que corresponde cada um dos documentos, use a
seguinte função:

``` r
metadocs <- ler_metadocs_stj(diretorio = "processos")
```

Caso queira baixar os documentos em um segundo momento, aí sim você pode
optar por html ou pdf:

``` r
dir.create("documentos")
baixar_documento_stj(metadocs$sequencial,diretorio = "documentos", formato="pdf")
```

Em seguida, você pode ler tais documentos

``` r
docs <- ler_documento_stj(diretorio = "documentos")
```

## Código de conduta

Please note that the ‘stj’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.
