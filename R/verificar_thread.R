#' Consulta se a thread já foi rodada.
#'
#' @param url Url obtida com openai_roda_thread.
#' @param api_key OPENAI_API_KEY. Melhor se estiver como variável de ambiente.
#'
#' @return Resposta em json.
#' @export
#'
verificar_thread <- function(url, api_key=NULL){

  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }

  headers <- c(`Authorization`  = glue::glue("Bearer {api_key}"),
               `Content-Type` = "application/json",
               `OpenAI-Beta` = "assistants=v1")

  url |>
    httr2::request() |>
    httr2::req_headers(!!!headers) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

}
