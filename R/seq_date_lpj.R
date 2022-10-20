#' seq_date_lpj
#' primarily for internal use in read_lpj but can also be used on its own
#' @param time time variable from lpj output file
#' @param res  temporal resolution of lpj output file
#' @param start when to start date vector
#'
#' @return returns a date vector without leap years
#' @export


seq_date_lpj <- function(time, res, start = "1901-01-01") {

  if (!(res %in% c("day", "month", "year"))) {
    error <- glue::glue("Time unit is: {res}. Must be one of: day, month, year")
    return(print(error))
  }

  date_vec <- seq.Date(from = as.Date(start), length.out = length(time), by = res)

  if (res == "day") {
    no_leap <- date_vec[format(date_vec, "%m-%d") != "02-29"]
    num_mis_date <- length(date_vec) - length(no_leap)
    missing_dates <- seq.Date(from = dplyr::last(date_vec),
                              length.out = num_mis_date + 1, by = res)
    date_vec <- c(no_leap, missing_dates[-1])
    return(date_vec)
  }

  return(date_vec)
}
