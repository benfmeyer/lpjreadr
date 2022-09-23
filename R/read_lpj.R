#' read_lpj
#'
#' @param nc_path  path to netcdf file containing lpj output
#' @param var_name variable(s) to extract from nc file ("var1" or c("var1", "var2", ...))
#' @param res      temporal resolution of nc file ("year", "month", or "day")
#' @param start    gets passed to seq_date_lpj to determine beginning of time series
#'
#' @return returns a nested tibble grouped by pfts or pfts and vars for one or more
#'     var inputs, respectively
#'
#' @export
#' @examples
#' \dontrun{
#' path <- "path/to/lpj/nc/file.nc"
#' tidy_output <- read_lpj(path, c("Pft-Out/npp", "Pft-Out/gpp"), "year")
#' }
#'
read_lpj <- function(nc_path, var_name, res, start = "1901-01-01") {
  date <- lat <- lon <- pft <- var <- NULL
  . <- "I'm a tree, leave me alone"

  nc <- ncdf4::nc_open(nc_path)

  lat <- ncdf4::ncvar_get(nc, "Base/Latitude")
  lon <- ncdf4::ncvar_get(nc, "Base/Longitude")
  pfts <- ncdf4::ncvar_get(nc, "Base/Pfts")

  time <- nc$dim$time$vals
  dates <- seq_date_lpj(time, res, start)

  if (length(var_name) == 1) {
    var1 <- ncdf4::ncvar_get(nc, var_name)

    out_list <- vector("list")
    for (i in seq_along(t(var1[1, 1, ]))) {
      out_list[[i]] <- t(var1[, , i]) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        purrr::set_names(pfts) %>%
        dplyr::mutate(
          lat = rep(lat[[i]], nrow(.)),
          lon = rep(lon[[i]], nrow(.)),
          date = dates
        )

      out_tibble <- dplyr::bind_rows(out_list) %>%
        tidyr::pivot_longer(-c(date, lat, lon),
          names_to = "pft",
          values_to = "value"
        ) %>%
        dplyr::group_nest(pft)
    }
  }

  if (length(var_name) > 1) {
    var_list <- vector("list")
    for (i in seq_along(var_name)) {
      var_list[[i]] <- ncdf4::ncvar_get(nc, var_name[[i]])
    }
  }




  if (length(var_name) > 1) {
    joined_arrays <- abind::abind(var_list, along = 4)

    inner_list <- outer_list <- vector("list")
    for (j in seq_along(joined_arrays[1, 1, 1, ])) {
      outer_list[[j]] <- joined_arrays[, , , j]
    }
    for (h in seq_along(outer_list)) {
      for (i in seq_along(t(outer_list[[h]][1, 1, ]))) {
        inner_list[[i]] <- t(outer_list[[h]][, , i]) %>%
          tibble::as_tibble(.name_repair = "minimal") %>%
          purrr::set_names(pfts) %>%
          dplyr::mutate(
            lat = rep(lat[[i]], nrow(.)),
            lon = rep(lon[[i]], nrow(.)),
            date = dates,
            var = rep(var_name[[h]], nrow(.))
          )
      }
      outer_list[[h]] <- inner_list
    }

    out_tibble <- dplyr::bind_rows(outer_list) %>%
      tidyr::pivot_longer(-c(date, lat, lon, var),
                   names_to = "pft",
                   values_to = "value") %>%
      dplyr::group_nest(pft, var)
  }
  ncdf4::nc_close(nc)
  return(out_tibble)
}
