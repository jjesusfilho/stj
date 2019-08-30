## code to prepare `DATASET` dataset goes here

usethis::use_data("DATASET")

library(JurisMiner)
list.files("data-raw","html",full.names = T) %>%
  purrr::map(file.remove)
stj<-cnj_sequencial(342000,342100,2018,segmento = 3,uf = 00,distribuidor = 0000)
baixar_processo_stj(stj,"data-raw")
