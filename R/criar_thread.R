#' Cria theread com perguntas para os arquivos.
#'
#' @param perguntas Vetor de perguntas
#' @param colunas Vetor com os nomes das colunas
#' @param ids_arquivos Vetor de arquivos
#' @param api_key OPENAI_API_KEY. Coloque como variável de ambiente.
#'
#' @return Retorna o id da thread.
#' @export
#'
criar_thread <- function(perguntas, colunas, ids_arquivos, api_key = NULL){


  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }

  corpo2 <- list(messages = jus_prompt(perguntas, colunas))

  corpo2$messages[[1]]$file_ids <- as.list(ids_arquivos)


  headers <- c(`Authorization`  = glue::glue("Bearer {api_key}"),
               `Content-Type` = "application/json",
               `OpenAI-Beta` = "assistants=v1")


  r2 <-  "https://api.openai.com/v1/threads" |>
    httr2::request() |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_json(corpo2) |>
    httr2::req_perform()

  r2 |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    purrr::pluck("id")

}




#' Monta mensagens para thread
#'
#' @param perguntas Vetor de perguntas
#' @param colunas Vetor com os nomes das colunas
#'
#' @return Lista de mensagens
#'
jus_prompt <- function(perguntas, colunas){


  p <- perguntas |>
    purrr::map(~{
      list("role" = "user",
           "content" = .x)
    })

  chaves <- stringr::str_c(colunas, collapse = ", ")


  mensagens <- list(
    list(
      "role" = "user",
      "content" = glue::glue("Retorne os resultados em formato json o file_ids seguidos das seguintes chaves: {chaves}")
    )

  )

  append(p, mensagens)


}
