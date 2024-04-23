#' Roda thread no assistente.
#'
#' @param id_thread Id da thread
#' @param id_assistente Id do assistente
#' @param api_key OPENAI_API_KEY
#'
#' @return url para checagem se já foi finalizado.
#' @export
#'
rodar_thread <- function(id_thread, id_assistente, api_key = NULL){


  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }

  url_run <- glue::glue("https://api.openai.com/v1/threads/{id_thread}/runs")

  headers <- c(`Authorization`  = glue::glue("Bearer {api_key}"),
               `Content-Type` = "application/json",
               `OpenAI-Beta` = "assistants=v1")

  r5 <- url_run |>
    httr2::request() |>
    httr2::req_body_json(list(`assistant_id`= id_assistente)) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_perform()

  r5$url
}
