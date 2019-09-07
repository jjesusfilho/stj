
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

Você pode usar usar tanto o número do registro, quanto o número do CNJ,
este conforme resulução 65/2008. A função dá conta de distinguir as
situação e realizar a busca corretamente.

Você pode optar por baixar também os documentos relativos ao processo,
tais como ementa, inteiro teor etc. Eles serão baixados em html. Ainda
não avaliei se vale a pena incluir a opção de pdf. Os nomes dos
documentos terão a palavra documento, seguido do código sequencial:

``` r
baixar_processo_stj(df$registro_stj, diretorio = ".", documentos = TRUE)
```

Depois de baixados, você pode ler os detalhes:

``` r
detalhes <- ler_detalhes_stj(diretorio = ".")
```

Em seguida, ler o andamento processual:

``` r
fases <- ler_fases_stj(diretorio = ".")
```

Caso você queira saber ao que corresponde cada um dos documentos, use a
seguinte função:

``` r
metadocs <- ler_metadocs_stj(diretorio = ".")
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

## Pesquisa jurisprudencial

Para a pesquisa jurisprudencial, além da pesquisa livre, você pode
informar o repositório, a data inicial, a data final e o operador: “e”
ou “adj”. O padrão para o repositório (“repo”), é “ACOR”, para acórdãos.
Mas você pode informar també “SUMU” para súmulas,“DTXT” para
monocráticas e “INFJ” para informativo jurisprudencia.

``` r
dir.create("julgados")

baixar_julgados_stj(livre="dissolução regular",
operador = "adj", repo = "ACOR",
data_inicial = "01/01/2018", 
data_final = "01/09/2019",diretorio="julgados")
```

Em seguida, basta ler os julgados:

``` r
df <- ler_julgados_stj(diretorio = "julgados")
```

## Código de conduta

Please note that the ‘stj’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.
