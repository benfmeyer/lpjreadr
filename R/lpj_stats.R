#' Run summary statistics (mean, min, range, sd, etc.) on all variables in an output file.
#' Choose whether to aggregate by year, gridcell, or both.
#'
#' @param nc_path string. file path to LPJG netCDF output
#' @param lvl string. one of: "year", "gridcell", "both". defaults to "both".
#' @return a tibble
#' @export
lpj_stats <- function(nc_path, lvl = "both") {

  .make_stats <- function(var) {

    mn <- switch (lvl,
                  "both" = quote(mean),
                  "year" = quote(rowMeans),
                  "gridcell" = quote(colMeans)
    )

    mnm <- switch(lvl,
                  "both" = quote(min),
                  "year" = quote(matrixStats::rowMins),
                  "gridcell" = quote(matrixStats::colMins)
    )

    mxm <- switch(lvl,
                  "both" = quote(max),
                  "year" = quote(matrixStats::rowMaxs),
                  "gridcell" = quote(matrixStats::colMaxs)
    )

    mdn <- switch(lvl,
                  "both" = quote(median),
                  "year" = quote(matrixStats::rowMedians),
                  "gridcell" = quote(matrixStats::colMedians)
    )

    x <- ncdf4::ncvar_get(nc, var)

    if (grepl("Pft", var)) {
      x_mn <- apply(x, 1, eval(mn))
      x_min <- apply(x, 1, eval(mnm))
      x_max <- apply(x, 1, eval(mxm))
      x_mid <- apply(x, 1, eval(mdn))
      len <- nrow(x_mn)
    } else if (grepl("Patch", var)) {
      x_mn <- do.call(eval(mn), list(x)) |> as.matrix()
      x_min <- do.call(eval(mnm), list(x)) |> as.matrix()
      x_max <- do.call(eval(mxm), list(x)) |> as.matrix()
      x_mid <- do.call(eval(mdn), list(x)) |>  as.matrix()
      len <- nrow(x_mn)
    }

    stt <- rbind(x_mn, x_min, x_max, x_mid) |>
      tibble::as_tibble(.name_repair = ~vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))|>
      dplyr::mutate(stat = rep(c("mean", "min", "max", "median"), each = len),
                    var = var)

    if (lvl == "both")


      return(stt)

    if (lvl == "gridcell") {
      stt <- stt |>
        dplyr::mutate(lon = rep(lon, 4),
                      lat = rep(lat, 4))


      return(stt)
    }

    stt <- stt |>
      dplyr::mutate(date = rep(dates, 4))


    return(stt)
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

  pft_vn <- nc$var |>
    names() |>
    stringr::str_subset("Pft-Out")

  patch_vn <- nc$var |>
    names() |>
    stringr::str_subset("Patch-Out")

  dynnms <- switch(lvl,
                      "both" = c("stat", "var"),
                      "year" = c("stat", "var", "date"),
                      "gridcell" = c("stat", "var", "lon", "lat")
                      )

  nms <- c(pfts, dynnms)

  pft_vv <- lapply(pft_vn, .make_stats) |>
    dplyr::bind_rows() |>
    purrr::set_names(nms)

  patch_vv <- lapply(patch_vn, .make_stats) |>
    dplyr::bind_rows() |>
    dplyr::rename(val = `...1`)

  ncdf4::nc_close(nc)
  return(list(pft_vv, patch_vv))
}
