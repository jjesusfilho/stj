#' Baixa acórdãos do STJ a partir das urls lidas do stj_ler_cjsg
#'
#' @param url Vetor de urls
#' @param diretorio Onde armazenar os arquivos
#'
#' @return pdfs
#' @export
#'
stj_baixar_acordao_cjsg <- function(url,diretorio = "."){
  
  purrr::walk(url, purrr::possibly(~{
    
   r1 <- .x |> 
          httr2::request() |> 
          httr2::req_perform()
   
   mime <- r1 |> 
           httr2::resp_content_type()
   
   basename2 <- .x |> 
     stringr::str_extract("(?<=\\?).+") |>
     stringr::str_replace_all("\\W","_") |> stringr::str_remove("\\D+$") |>
     stringr::str_c("bn2_",...=_,".pdf")
   
   if (mime == "application/pdf"){
     
     arquivo <- file.path(diretorio,basename2)
     
     writeBin(r1$body, arquivo)
     
   } else if(mime == 'text/html'){
     
     urls <- r1 |> 
            httr2::resp_body_html() |> 
            xml2::xml_find_all("//a[contains(@id, 'acd')]") |> 
            xml2::xml_attr("href") |> 
            unique()
     
     
     
     purrr::walk(urls,~{
       
       basename1 <- .x |> 
         stringr::str_extract("(?<=\\?).+") |>
         stringr::str_replace_all("\\W","_") |> stringr::str_remove("\\D+$") |>
         stringr::str_c("bn1_",...=_)
       
       arquivo <- file.path(diretorio,paste0(basename1, "_", basename2))
       
       .x |> 
          httr2::request() |> 
          httr2::req_perform(path = arquivo)
         
     })
     
     
   } else 
     
     NULL
     
  }, NULL), .progress = TRUE)
  
  
}