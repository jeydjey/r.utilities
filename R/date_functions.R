
#' Splits a date range into monthly factors
#'
#' @param from date from
#' @param to date to
#' @param basis.360 360 days in a year
#' @export
period_split <- function(from, to, basis.360 = FALSE) {

  #from <- lubridate::as_date("2022-10-01")
  #to <- lubridate::as_date("2023-01-01")

  yr <- lubridate::year(to) - lubridate::year(from)

  month <- yr * 12 + (lubridate::month(to) - lubridate::month(from)) + 1

  mtx <- matrix(c(rep(lubridate::year(from):lubridate::year(to), each = 12), rep(1:12, yr+1), rep(1, (yr+1)*12), rep(0, (yr+1)*12)), nrow = (yr+1)*12, ncol = 4, byrow = FALSE)

  mtx <- mtx[lubridate::month(from):dim(mtx)[1],][seq_len(month),]

  mtx[1,3] <- (lubridate::days_in_month(from)-lubridate::day(from)+1) / lubridate::days_in_month(from)
  mtx[dim(mtx)[1],3] <- (lubridate::day(to)-1) / lubridate::days_in_month(to)

  mtx[,4] <- mtx[,3]/sum(mtx[,3])

  res <- cbind(mtx[,1]*100+mtx[,2], mtx[,4])

  colnames(res) <- c("year_month", "factor")

  return(res[res[,2]>0,, drop = FALSE])

}
