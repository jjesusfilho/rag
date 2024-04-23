#' Obter resposta da thread rodada
#'
#' @param id_thread Id da thread
#' @param api_key OPENAI_API_KEY
#'
#' @return Resposta em json
#' @export
#'
obter_resposta <- function(id_thread, api_key = NULL){

  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }

  url_mensagem <- glue::glue("https://api.openai.com/v1/threads/{id_thread}/messages")

  headers <- c(`Authorization`  = glue::glue("Bearer {api_key}"),
               `Content-Type` = "application/json",
               `OpenAI-Beta` = "assistants=v1")


  url_mensagem |>
    httr2::request() |>
    httr2::req_headers(!!!headers) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    purrr::pluck("data", "content", 1, "text","value")

}
