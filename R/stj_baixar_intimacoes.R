#' Baixa intimações do usuário especificado
#'
#' @param usuario CPF do usuário. Se não for informado, irá procurar
#'     pela variável de ambiente STJUSUARIO. Se não encontrar,
#'     irá abrir uma caixa de diálogo para preenchimento.
#' @param senha Senha. Se não for informado, irá procurar
#'     pela variável de ambiente STJSENHA. Se não encontrar,
#'     irá abrir uma caixa de diálogo para preenchimento.
#' @param diretorio Diretório onde baixar o arquivo xls.
#'
#' @return Arquivo xls
#' @export
#'
stj_baixar_intimacoes <- function(usuario = NULL, senha = NULL, diretorio = "."){

  if (is.null(usuario) || is.null(senha)) {

    usuario <- Sys.getenv("STJUSUARIO")
    senha <- Sys.getenv("STJSENHA")

    if (usuario == "" || senha == "") {
      usuario <- as.character(getPass::getPass(msg = "Usu\u00E1rio: "))
      senha <- as.character(getPass::getPass(msg = "Senha: "))
    }
  }

  url1 <- "https://www.stj.jus.br/portalIntimacao/public/login.xhtml"

  viewid <- url1 |>
    httr::GET() |>
    httr::content("text") |>
    xml2::read_html() |>
    xml2::xml_find_first("//input[@name='javax.faces.ViewState']") |>
    xml2::xml_attr("value")


  url2 <- "https://www.stj.jus.br/portalIntimacao/public/login.xhtml"


  body1 <- list(j_idt17 = "j_idt17", `j_idt17:j_idt22` = usuario,
                `j_idt17:j_idt26` = "", javax.faces.ViewState = viewid,
                javax.faces.source = "j_idt17:j_idt22", javax.faces.partial.event = "change",
                javax.faces.partial.execute = "j_idt17:j_idt22 j_idt17:j_idt22",
                javax.faces.partial.render = "j_idt17:optnEnteLogin", javax.faces.behavior.event = "change",
                javax.faces.partial.ajax = "true")


  r2 <- httr::POST(url2, encode = "form",
                   body = body1)


  body2 <- list(j_idt17 = "j_idt17",
                `j_idt17:j_idt22` = usuario,
                `j_idt17:j_idt26` = senha,
                `j_idt17:optnEnteLogin` = "281",
                `j_idt17:j_idt32` = "",
                javax.faces.ViewState = viewid)

  r3 <- httr::POST(url2, encode = "form",
                   body = body2)


  url3 <- "https://www.stj.jus.br/portalIntimacao/protected/user/listarIntimacoesPendentes.xhtml"

  body3 <- list(
    formDadosIntimacao = "formDadosIntimacao",
    `formDadosIntimacao:pendentesTable:j_idt106` = "",
    `formDadosIntimacao:pendentesTable:j_idt109:filter` = "",
    `formDadosIntimacao:pendentesTable:j_idt111:filter` = "",
    `formDadosIntimacao:pendentesTable:j_idt113:filter` = "",
    `formDadosIntimacao:pendentesTable:j_idt115:filter` = "",
    `formDadosIntimacao:pendentesTable:j_idt117:filter` = "",
    `formDadosIntimacao:pendentesTable:j_idt119:filter` = "",
    `formDadosIntimacao:pendentesTable_selection` = "",
    javax.faces.ViewState = viewid
  )


  arquivo <- file.path(diretorio, paste0("stj_intimacoes_baixadas_em_",
                                         stringr::str_replace_all(Sys.time(),"\\D","_"),
                                         ".xls"))

  httr::POST(
    url3,
    body = body3,
    encode = "form",
    httr::accept("application/vnd.ms-excel"),
    httr::write_disk(arquivo, overwrite = TRUE)
  )

}
