#' Sobe arquivos para assistente
#'
#' @param arquivos Vetor com caminho para arquivos.
#' @param api_key Chave. Se não informar, buscará em OPENAI_API_KEY.
#'
#' @return Tibble com ids
#' @export
#'
subir_arquivos <- function(arquivos = NULL, api_key = NULL){

  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }


  purrr::map_dfr(arquivos, purrr::possibly(~{

    r1 <- "https://api.openai.com/v1/files" |>
      httr2::request() |>
      httr2::req_headers(`Authorization`  = glue::glue("Bearer {api_key}")) |>
      httr2::req_body_multipart(purpose = 'assistants',
                                file = curl::form_file(.x)) |>
      httr2::req_perform()


    id <-   r1 |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON() |>
      purrr::pluck("id")

    tibble::tibble(arquivo = .x, id = id)

  }, NULL), .progress = TRUE)

}
