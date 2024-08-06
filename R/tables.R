#' Create table with variable overview
#'
#' @param data Data frame
#' @param html Logical, should the output be in HTML format?
#'
#' @return A tibble with columns `Column`, `Type`, `Completeness`
#' @export
tbl_var_overview <- function(data, html = FALSE) {
  tbl <- tibble::tibble(Column = names(data),
                        Type = purrr::map(data, class),
                        Completeness =
                          purrr::map(data, is.na) |>
                          purrr::map(mean) |>
                          purrr::map(\(x){1 - as.numeric(x)}) |>
                          purrr::map(scales::label_percent())) |>
    tidyr::unnest(c(Type, Completeness))

  if (html) {
    kableExtra::kable(tbl, align = "llr")
  } else {
    tbl
  }
}
