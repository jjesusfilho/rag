#' Lista arquivos na OPENAI
#'
#' @param api_key OPENAI_API_KEY
#'
#' @return tibble
#' @export
#'
listar_arquivos <- function(api_key=NULL){


  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }

  headers <- c(`Authorization`  = glue::glue("Bearer {api_key}"))

  u <- "https://api.openai.com/v1/files"

  u |>
    httr2::request() |>
    httr2::req_headers(!!!headers) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("data") |>
    dplyr::bind_rows()

}
