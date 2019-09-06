#' Organiza dados do stj a partir das bases julgados e detalhes
#'
#' @param detalhes dados obtidos com ler_datalhes_stj
#' @param julgados dados obtidos com ler_julgados_stj
#'
#' @return dados organizadps
#' @export
#'
#' @examples
#' \dontrun{
#' df <- organizar_stj(detalhes, julgados)
#' }
organizar_stj <- function(detalhes,julgados){

  detalhes<-dplyr::mutate_all(detalhes,list(~stringr::str_squish(.)))
  julgados<-dplyr::mutate_all(julgados,list(~stringr::str_squish(.)))

  suppressMessages({
  suppressWarnings({



  df <- detalhes %>%
    dplyr::mutate(v1 = NULL,
                  data_autuacao = lubridate::dmy(autuacao),
                  autuacao = NULL,
                  localizacao = NULL) %>%
                # numero = ifelse(nchar(numero)==13,sub(numero,5,4,"/"),numero),
                # numero = ifelse(nchar(numero)==13,sub(registro,13,12,"-"),numero),
                #  assunto_s = str_squish(assunto_s)) %>%
    dplyr::rename(classe = "processo") %>%
    tidyr::separate(assunto_s,c("assunto","subassunto1","subassunto2","subassunto3","subassunto4","subassunto5"),sep = ",",extra="merge") %>%
    tidyr::separate(relator_a,c("relator","orgao_julgador"),sep = "-", extra="merge") %>%
    tidyr::separate(advogado, c("advogado","oab"),sep = " - ", extra="merge") %>%
    dplyr::mutate(relator = stringr::str_remove(relator,"(?i)^min\\.\\s?"),
                  ultima_fase= NULL) %>%
    dplyr::select(registro,tidyselect::everything()) %>%
    dplyr::left_join(julgados, by="registro") %>%
    dplyr::mutate(classe.x = NULL,
                  relator.x = NULL,
                  orgao_julgador.x = NULL,) %>%
    dplyr::rename(classe = "classe.y",
           relator = "relator.y",
           orgao_julgador = "orgao_julgador.y") %>%
    dplyr::mutate(numero=NULL)

  nomes <- names(df)

  nomes2<-c(c("registro","numero_unico","classe","assunto","ramo_do_direito","relator","orgao_julgador","tribunal_de_origem","data_julgamento","data_autuacao","data_publicacao",paste0("subassunto",1:5)),nomes) %>% unique()

  df <- df %>%
    dplyr::select(!!nomes2)

  df <- df %>%
    classificar_recurso(dispositivo,decisao) %>%
    votacao(dispositivo)

  })
  })
}

