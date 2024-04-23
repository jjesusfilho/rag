#' Obtêm arquivo
#'
#' @param api_key Chave da OPENAI. Se não informar, buscará em OPENAI_API_KEY
#' @param ids_arquivos Vetor os ids dos arquivos.
#'
#' @return tibble com informaçoes dos arquivos
#' @export
#'
obter_arquivo <- function(api_key, ids_arquivos){


  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }

  headers <- c(`Authorization`  = glue::glue("Bearer {api_key}"))

  uri <- "https://api.openai.com/v1/files/"


  purrr::map_dfr(ids_arquivos, purrr::possibly(~{

    uri |>
      paste0(.x) |>
      httr2::request() |>
      httr2::req_headers(!!!headers) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      purrr::compact() |>
      tibble::as_tibble()

  }, NULL))

}
