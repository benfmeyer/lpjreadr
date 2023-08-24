#' read_lpj_meta returns the names of all variables contained in an LPJ-GUESS .nc output file
#'
#' @param nc_path
#'
#' @return a tibble
#' @export
#'
read_lpj_meta <- function(nc_path){
  nc <- ncdf4::nc_open(nc_path)

  var_info <- nc$var

  var_names <- names(var_info) |>
    tibble::as_tibble() |>
    tidyr::separate(value, c("group", "name"), "/")

  var_size <- lapply(var_info, purrr::pluck, "size") |>
    as.character() |>
    tibble::as_tibble()

  lpj_meta <- dplyr::bind_cols(var_names, var_size) |>
    dplyr::rename("dimensions" = "value")

  return(lpj_meta)

}
