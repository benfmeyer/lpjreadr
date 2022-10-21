#' read_lpj_pft
#'
#' @param nc_path  path to netcdf file containing lpj output
#' @param var_name variable(s) to extract from nc file ("var1" or c("var1", "var2", ...))

#' @return a tibble
#'
#' @export
#' @examples
#' \dontrun{
#' path <- "path/to/lpj/nc/file.nc"
#' tidy_output <- read_lpj_pft(path, c("Pft-Out/npp", "Pft-Out/gpp"))
#' }
#'


read_lpj_pft <- function(nc_path, var_name) {

  .add_attributes <- function(x, var_name, pfts, lat, lon, dates) {
    x |>
      t() |>
      tibble::as_tibble(.name_repair = "minimal") |>
      purrr::set_names(pfts) |>
      dplyr::mutate(date = rep(dates, length(lat)), lat = rep(lat, each = length(dates)),
                    lon = rep(lon, each = length(dates)), var = var_name)
  }

  nc   <- ncdf4::nc_open(nc_path)
  lat  <- ncdf4::ncvar_get(nc, "Base/Latitude")
  lon  <- ncdf4::ncvar_get(nc, "Base/Longitude")
  pfts <- ncdf4::ncvar_get(nc, "Base/Pfts")

  start <- stringr::str_extract(nc$var$`Base/Time`$units, "\\d{2}-\\d{2}-\\d{4}") |>
    strsplit("-") |>
    purrr::pluck(1) |>
    rev() |>
    paste(collapse = "-")

  res <- stringr::word(nc$var$`Base/Time`$units) |>
    stringr::str_remove("s")

  time <- nc$dim$time$vals
  dates <- seq_date_lpj(time, res, start)

  if (length(lat) <= 0) {
    error <- glue::glue("Number of gridcells is: {length(lat)}. Must be at least 1")
    ncdf4::nc_close(nc)
    return(print(error))
  }

  if (length(lat) != 1) {
    if (length(var_name) != 1) {

      out_tibble <- lapply(var_name, ncdf4::ncvar_get, nc = nc) |>
        lapply(as.data.frame) |>
        purrr::map2(var_name, .add_attributes, pfts, lat, lon, dates) |>
        dplyr::bind_rows()

      ncdf4::nc_close(nc)
      return(out_tibble)
    }

    out_tibble <- ncdf4::ncvar_get(nc, var_name) |>
      as.data.frame() |>
      .add_attributes(var_name, pfts, lat, lon, dates)

    ncdf4::nc_close(nc)
    return(out_tibble)
  }

  if (length(var_name) != 1) {

    out_tibble <- lapply(var_name, ncdf4::ncvar_get, nc = nc) |>
      purrr::map2(var_name, .add_attributes, pfts, lat, lon, dates) |>
      dplyr::bind_rows()

    ncdf4::nc_close(nc)
    return(out_tibble)
  }


  out_tibble <- ncdf4::ncvar_get(nc, var_name) |>
    .add_attributes(var_name, pfts, lat, lon, dates)

  ncdf4::nc_close(nc)
  return(out_tibble)
}
