#' Baixa comunicações do STJ via api do CNJ
#'
#' @param processo Número único do processo
#' @param dt_inicio Data do início das comunicações
#' @param dt_fim data do fim das comunicações
#' @param diretorio Data final
#'
#' @details
#'    Tente restringir a data_inicio e data_fim para
#'    o mesmo dia ou alguns poucos dias.
#'
#' @returns json
#' @export
#'
cnj_consultar_comunicacao1 <- function(processo = NULL,
                                       dt_inicio = NULL,
                                       dt_fim = NULL,
                                       diretorio = ".") {

  # Verificação de argumentos nulos
  if (is.null(processo)) {
    stop("O número do processo não pode ser nulo")
  }

  if (is.null(dt_inicio) || is.null(dt_fim)) {
    stop("As datas de início e fim não podem ser nulas")
  }

  # Tratamento do número do processo
  processo_original <- processo
  processo <- tryCatch({
    stringr::str_remove_all(processo, "\\D+")
  }, error = function(e) {
    stop("Erro ao formatar o número do processo: ", e$message)
  })

  if (nchar(processo) != 20) {
    stop("O número do processo deve ter exatamente 20 dígitos. Fornecido: ",
         processo_original, " (", nchar(processo), " dígitos após remoção de caracteres não numéricos)")
  }

  # Tratamento das datas
  tryCatch({
    dt_inicio <- lubridate::dmy(dt_inicio)
    if (is.na(dt_inicio)) {
      stop("Data de início inválida. Use o formato dd/mm/aaaa")
    }
  }, error = function(e) {
    stop("Erro ao processar a data de início: ", e$message)
  })

  tryCatch({
    dt_fim <- lubridate::dmy(dt_fim)
    if (is.na(dt_fim)) {
      stop("Data de fim inválida. Use o formato dd/mm/aaaa")
    }
  }, error = function(e) {
    stop("Erro ao processar a data de fim: ", e$message)
  })

  # Verificação de ordem cronológica
  if (dt_inicio > dt_fim) {
    stop("A data de início não pode ser posterior à data de fim")
  }

  # Verificação do diretório
  if (!dir.exists(diretorio)) {
    stop("O diretório especificado não existe: ", diretorio)
  }

  # Construção da URI
  uri_parseada <- tryCatch({
    structure(
      list(
        scheme = "https",
        hostname = "comunicaapi.pje.jus.br",
        username = NULL,
        password = NULL,
        port = NULL,
        path = "/api/v1/comunicacao",
        query = list(
          pagina = "1",
          itensPorPagina = "1000",
          siglaTribunal = "STJ",
          dataDisponibilizacaoInicio = dt_inicio,
          dataDisponibilizacaoFim = dt_fim,
          numeroProcesso = processo
        ),
        fragment = NULL
      ),
      class = "httr2_url"
    )
  }, error = function(e) {
    stop("Erro ao construir a URL: ", e$message)
  })

  uri <- tryCatch({
    httr2::url_build(uri_parseada)
  }, error = function(e) {
    stop("Erro ao construir a URL: ", e$message)
  })

  # Construção do nome do arquivo
  basename <- tryCatch({
    glue::glue("cnj_processo_{processo}_dt_inicio_{dt_inicio}_dt_fim_{dt_fim}.json") |>
      stringr::str_replace_all("-", "_")
  }, error = function(e) {
    stop("Erro ao gerar o nome do arquivo: ", e$message)
  })

  arquivo <- file.path(diretorio, basename)

  # Realização da requisição HTTP
  tryCatch({
    resposta <- uri |>
      httr2::request() |>
      httr2::req_perform(path = arquivo)

    # Verificar se a requisição foi bem-sucedida
    status <- httr2::resp_status(resposta)
    if (status < 200 || status >= 300) {
      warning("A requisição retornou status ", status)
    }

    return(resposta)
  }, error = function(e) {
    stop("Erro na requisição HTTP: ", e$message)
  })
}


#' Parsea coluna texto
#'
#' @param x coluna texto em html
#'
#' @returns texto
#'
cnj_parsear_texto <- function(x) {
  # Verifica se x é nulo ou vazio
  if (is.null(x) || length(x) == 0 || all(nchar(x) == 0)) {
    warning("Texto vazio ou nulo fornecido para parsing")
    return("")
  }

  tryCatch({
    # Tenta fazer o parsing do HTML
    html <- xml2::read_html(x, encoding = "UTF-8")

    # Busca os elementos <p>
    paragrafos <- xml2::xml_find_all(html, "//p")

    # Se não encontrar parágrafos, pode ser um formato diferente
    if (length(paragrafos) == 0) {
      # Tenta extrair todo o texto do corpo
      texto <- xml2::xml_text(xml2::xml_find_first(html, "//body"))
      if (is.na(texto) || nchar(texto) == 0) {
        warning("Não foi possível encontrar texto no conteúdo HTML")
        return(x) # Retorna o texto original se não conseguir parsear
      }
      return(texto)
    }

    # Extrai o texto dos parágrafos
    texto <- xml2::xml_text(paragrafos)

    # Junta os parágrafos com quebras de linha
    resultado <- stringr::str_flatten(texto, "\n\n")

    return(resultado)
  }, error = function(e) {
    warning("Erro ao parsear o texto HTML: ", e$message, "\nRetornando texto original.")
    return(x) # Retorna o texto original em caso de erro
  })
}

#' Lê comunicação do CNJ
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se arquivos não foi fornecido, tenta encontrar arquivos no diretório
#'
#' @returns tibble
#' @export
#'
cnj_ler_comunicacao <- function(arquivos = NULL, diretorio = ".") {
  # Verifica se o diretório existe
  if (!dir.exists(diretorio)) {
    stop("O diretório especificado não existe: ", diretorio)
  }

  # Se arquivos não foi fornecido, tenta encontrar arquivos no diretório
  if (is.null(arquivos)) {
    arquivos <- tryCatch({
      arquivos_encontrados <- list.files(diretorio, full.names = TRUE, pattern = "cnj_")

      if (length(arquivos_encontrados) == 0) {
        stop("Nenhum arquivo com padrão 'cnj_' encontrado no diretório: ", diretorio)
      }

      arquivos_encontrados
    }, error = function(e) {
      stop("Erro ao listar arquivos no diretório: ", e$message)
    })
  } else if (length(arquivos) == 0) {
    stop("Lista de arquivos vazia")
  }

  # Verifica se os arquivos existem
  arquivos_inexistentes <- arquivos[!file.exists(arquivos)]
  if (length(arquivos_inexistentes) > 0) {
    warning("Os seguintes arquivos não existem: ",
            paste(arquivos_inexistentes, collapse = ", "))
    arquivos <- arquivos[file.exists(arquivos)]

    if (length(arquivos) == 0) {
      stop("Nenhum dos arquivos especificados existe")
    }
  }

  # Função para processar um único arquivo com tratamento de erros
  processar_arquivo <- function(arquivo) {
    tryCatch({
      # Extrai o número do processo do nome do arquivo
      processo <- stringr::str_extract(arquivo, "\\d{20}")
      if (is.na(processo) || nchar(processo) != 20) {
        warning("Não foi possível extrair um número de processo válido do arquivo: ", arquivo)
        processo <- NA_character_
      }

      # Extrai as datas do nome do arquivo
      dt_inicio_str <- stringr::str_extract(arquivo, "(?<=inicio_).{10}")
      dt_fim_str <- stringr::str_extract(arquivo, "(?<=fim_).{10}")

      dt_inicio <- tryCatch({
        lubridate::ymd(dt_inicio_str)
      }, error = function(e) {
        warning("Data de início inválida no arquivo: ", arquivo)
        return(NA)
      })

      dt_fim <- tryCatch({
        lubridate::ymd(dt_fim_str)
      }, error = function(e) {
        warning("Data de fim inválida no arquivo: ", arquivo)
        return(NA)
      })

      # Lê e processa o JSON
      json <- tryCatch({
        conteudo <- jsonlite::fromJSON(arquivo)

        # Verifica se o JSON contém a chave "items"
        if (!"items" %in% names(conteudo)) {
          warning("O arquivo JSON não contém a chave 'items': ", arquivo)
          return(NULL)
        }

        items <- purrr::pluck(conteudo, "items")

        # Verifica se items é vazio
        if (is.null(items) || length(items) == 0 || nrow(items) == 0) {
          warning("Nenhum item encontrado no arquivo: ", arquivo)
          return(NULL)
        }

        # Tenta executar o unnest_wider, que pode falhar se as colunas esperadas não existirem
        dados_unnest <- tryCatch({
          tidyr::unnest_wider(items, c(destinatarios, destinatarioadvogados), names_sep = "_")
        }, error = function(e) {
          warning("Erro ao expandir colunas no arquivo ", arquivo, ": ", e$message)
          # Tenta uma abordagem mais robusta
          if ("destinatarios" %in% names(items)) {
            items$destinatarios_id <- NA
            items$destinatarios_nome <- NA
            items$destinatarios_contato <- NA
          }
          if ("destinatarioadvogados" %in% names(items)) {
            items$destinatarioadvogados_id <- NA
            items$destinatarioadvogados_nome <- NA
            items$destinatarioadvogados_contato <- NA
          }
          return(items)
        })

        # Adiciona a coluna de conteúdo parseado se existir a coluna 'texto'
        if ("texto" %in% names(dados_unnest)) {
          dados_unnest$conteudo <- sapply(dados_unnest$texto, function(txt) {
            tryCatch({
              cnj_parsear_texto(txt)
            }, error = function(e) {
              warning("Erro ao parsear texto: ", e$message)
              return(txt)
            })
          })
          # Reordenar para colocar 'conteudo' após 'texto'
          idx <- which(names(dados_unnest) == "texto")
          if (idx < ncol(dados_unnest)) {
            ordem_colunas <- c(1:idx, ncol(dados_unnest), (idx+1):(ncol(dados_unnest)-1))
            dados_unnest <- dados_unnest[, ordem_colunas]
          }
        } else {
          warning("Coluna 'texto' não encontrada no arquivo: ", arquivo)
        }

        # Limpa os nomes das colunas
        dados_limpos <- janitor::clean_names(dados_unnest)

        # Adiciona as colunas de metadados
        dados_limpos <- tibble::add_column(
          dados_limpos,
          numero_unico = processo,
          dt_inicio = dt_inicio,
          dt_fim = dt_fim,
          arquivo_origem = arquivo,
          .before = 1
        )

        return(dados_limpos)
      }, error = function(e) {
        warning("Erro ao processar o arquivo JSON ", arquivo, ": ", e$message)
        return(NULL)
      })

      return(json)
    }, error = function(e) {
      warning("Erro geral ao processar o arquivo ", arquivo, ": ", e$message)
      return(NULL)
    })
  }

  # Processa todos os arquivos com barra de progresso
  resultados <- tryCatch({
    purrr::map_dfr(arquivos, purrr::possibly(processar_arquivo, NULL), .progress = TRUE)
  }, error = function(e) {
    stop("Erro ao combinar os resultados dos arquivos: ", e$message)
  })

  # Verifica se o resultado final é vazio
  if (is.null(resultados) || nrow(resultados) == 0) {
    warning("Nenhum dado foi extraído dos arquivos processados")
    return(tibble::tibble())
  }

  return(resultados)
}

