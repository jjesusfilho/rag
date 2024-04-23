#' Cria o assistente com as instruções para os arquivos.
#'
#' @param ids_arquivos Vetor os ids dos arquivos.
#' @param nome Nome do assistente
#' @param instrucoes Orientações para o assistente.
#' @param modelo Padrão "gpt-4-turbo-preview"
#' @param api_key Chave da OPENAI. Se não informar, buscará em OPENAI_API_KEY
#'
#' @return id do assistente
#' @export
#'
criar_assistente <- function(ids_arquivos, nome, instrucoes, modelo = "gpt-4-turbo-preview", api_key = NULL ){

  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }


  headers <- c(`Authorization`  = glue::glue("Bearer {api_key}"),
               `Content-Type` = "application/json",
               `OpenAI-Beta` = "assistants=v1")


  corpo <- list(name = nome,
                description=instrucoes,
                model = modelo,
                tools = list(list(`type` =  "retrieval")),
                file_ids = as.list(ids_arquivos))


  r2 <-  "https://api.openai.com/v1/assistants" |>
    httr2::request() |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_json(corpo) |>
    httr2::req_perform()

  r2 |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    purrr::pluck("id")


}
