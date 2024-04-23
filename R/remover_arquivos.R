#' Remover arquivos
#'
#' @param ids_arquivos Ids dos arquivos
#' @param api_key OPENAI_API_KEY
#'
#' @return tibble com informação sobre remoção do arquivo.
#' @export
#'
remover_arquivos <- function(ids_arquivos, api_key = NULL){


  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }

  headers <- c(`Authorization`  = glue::glue("Bearer {api_key}"))

  purrr::map_dfr(ids_arquivos, purrr::possibly(~{

    glue::glue("https://api.openai.com/v1/files/{.x}") |>
      httr2::request() |>
      httr2::req_method("DELETE") |>
      httr2::req_headers(`Authorization`  = glue::glue("Bearer {api_key}")) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON() |>
      tibble::as_tibble()

  }, NULL), .progress = TRUE)

}
