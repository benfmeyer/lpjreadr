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

  var_name_internal <- paste0("Pft-Out/",var_name)

  .add_attributes <- function(x, var_name, unit_val, pfts, lat, lon, dates) {
    x |>
      t() |>
      tibble::as_tibble(.name_repair = "minimal") |>
      purrr::set_names(pfts) |>
      dplyr::mutate(date = rep(dates, length(lat)), lat = rep(lat, each = length(dates)),
                    lon = rep(lon, each = length(dates)), var = var_name, unit = unit_val)
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

      unit_val <- lapply(var_name_internal, ncdf4::ncatt_get, nc = nc) |>
        lapply(`[[`, "unit")
      raw_var <- lapply(var_name_internal, ncdf4::ncvar_get, nc = nc) |>
        lapply(as.data.frame)
      out_tibble <- purrr::pmap(list(raw_var, var_name, unit_val), .add_attributes,
                                pfts, lat, lon, dates) |>
        dplyr::bind_rows()

      ncdf4::nc_close(nc)
      return(out_tibble)
    }

    unit_val <- ncdf4::ncatt_get(nc, var_name_internal)[["unit"]]
    out_tibble <- ncdf4::ncvar_get(nc, var_name_internal) |>
      as.data.frame() |>
      .add_attributes(var_name, unit_val, pfts, lat, lon, dates)

    ncdf4::nc_close(nc)
    return(out_tibble)
  }

  if (length(var_name) != 1) {

    unit_val <- lapply(var_name_internal, ncdf4::ncatt_get, nc = nc) |>
      lapply(`[[`, "unit")
    raw_var <- lapply(var_name_internal, ncdf4::ncvar_get, nc = nc)
    out_tibble <- purrr::pmap(list(raw_var, var_name, unit_val), .add_attributes, pfts, lat, lon, dates) |>
      dplyr::bind_rows()

    ncdf4::nc_close(nc)
    return(out_tibble)
  }

  unit_val <- ncdf4::ncatt_get(nc, var_name_internal)[["unit"]]
  out_tibble <- ncdf4::ncvar_get(nc, var_name_internal) |>
    .add_attributes(var_name, unit_val, pfts, lat, lon, dates)

  ncdf4::nc_close(nc)
  return(out_tibble)
}
