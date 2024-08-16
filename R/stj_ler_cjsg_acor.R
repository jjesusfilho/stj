#' Lê metadados de decisões colegiadas do STJ
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se não informar arquivos, informar diretório
#'
#' @return Tibble
#' @export
#'
stj_ler_cjsg_acor <- function(arquivos = NULL, diretorio =  "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern = "acor", full.names = TRUE)
    
  }
  
  purrr::map_dfr(arquivos, purrr::possibly(~{
    
    arquivo <- .x
    
    pagina <- stringr::str_extract(arquivo, "(?<=pagina_)\\d+")
    
    documentos <- arquivo |> 
      xml2::read_html() |> 
      xml2::xml_find_all("//div[@class='documento']")
    
    da <- purrr::map_dfr(documentos, ~{
      
      documento <- .x
      
      processo <- documento |>
        xml2::xml_find_all(".//div[@class='paragrafoBRS'][div[text()='Processo']]//div[@class='docTexto']/text()[following-sibling::br][1]") |>
        xml2::xml_text(trim = TRUE)
      
      origem <- processo |>
        stringr::str_extract("\\w{2}$")
      
      classe <- documento|>
        xml2::xml_find_all(".//div[@class='paragrafoBRS'][div[text()='Processo']]//div[@class='docTexto']/text()[following-sibling::br][2]") |>
        xml2::xml_text(trim = TRUE)
      
      registro_stj <- documento |>
        xml2::xml_find_all(".//div[@class='paragrafoBRS'][div[text()='Processo']]//div[@class='docTexto']/text()[preceding-sibling::br][2]") |>
        xml2::xml_text(trim = TRUE)
      
      relator <- documento |>
        xml2::xml_find_all(".//div[@class='docTitulo'][text()='Relator']/following-sibling::div[@class='docTexto']|.//div[@class='docTitulo'][text()='Relatora']/following-sibling::div[@class='docTexto']") |>
        xml2::xml_text() |>
        stringr::str_extract("(?<=Ministr[ao]\\s).*(?=\\s\\()")
      
      orgao_julgador <- documento |>
        xml2::xml_find_all(".//div[text()='\u00D3rg\u00E3o Julgador']/following-sibling::div[@class='docTexto']") |>
        xml2::xml_text()
      
      data_julgamento <- documento |>
        xml2::xml_find_all(".//div[text()='Data do Julgamento']/following-sibling::div[@class='docTexto']") |>
        xml2::xml_text() |>
        lubridate::dmy()
      
      publicacao <- documento |>
        xml2::xml_find_all(".//div[text()='Data da Publica\u00E7\u00E3o/Fonte']/following-sibling::div[@class='docTexto']") |>
        xml2::xml_text()
      
      fonte <- publicacao |>  
        stringr::str_extract("\\w+")
      
      data_publicacao <- stj:::pt_time_extract(publicacao)
      
      ementa <- documento |>
        xml2::xml_find_all(".//div[text()='Ementa']/following-sibling::div[@class='docTexto']") |>
        xml2::xml_text(trim = TRUE)
      
      dispositivo <- documento |>
        xml2::xml_find_all(".//div[text()='Ac\u00F3rd\u00E3o']/following-sibling::div[@class='docTexto']") |>
        xml2::xml_text()
      
      url_inteiro_teor <- documento |>
        xml2::xml_find_all(".//div[@class='col-auto clsIconesAcoes']/a[@title='Exibir o inteiro teor do ac\u00F3rd\u00E3o.']") |> 
        xml2::xml_attr("href") |> 
        stringr::str_extract("SCON.+") |> 
        xml2::url_absolute("https://scon.stj.jus.br/")
      
      if(length(url_inteiro_teor) == 0) {url_inteiro_teor = NA_character_}
      
      tibble::tibble(
        pagina,
        processo,
        origem,
        classe,
        registro_stj,
        relator,
        orgao_julgador,
        data_julgamento,
        fonte,
        data_publicacao,
        ementa,
        dispositivo,
        url_inteiro_teor
      )
      
    })
    
  },NULL), .progress = TRUE)
  
}
