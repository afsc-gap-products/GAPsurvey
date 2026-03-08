#' Median window filter
#'
#' Applies a median window filter to variables.
#'
#' @param x A numerical vector
#' @param window Window size (odd integer).
#' @noRd
#' @importFrom stats median
#' @author Sean Rohan

median_filter <- function(x, window = 5) {

  half_window <- (window-1)/2
  n_iter <- length(x)
  x_out <- numeric(length = n_iter)

  for(ii in (half_window+1):(n_iter-half_window)) {
    x_out[ii] <- median(x[max(c(1,ii-half_window)):min(c(n_iter, ii+half_window))], na.rm = TRUE)
  }

  return(x_out)

}


#' Low-pass filter
#'
#' Low-pass filter variables based on time using the filter from SBE data processing.
#'
#' @param x A numerical vector
#' @param time_constant Numeric vector of time constants for filters (in seconds).
#' @param precision Numeric vector indicating how many significant digits to use for each channel.
#' @param freq_n Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.
#' @noRd
#' @author Sean Rohan

lowpass_filter <- function(x, time_constant, freq_n, precision) {

  lp <- function(var, tc, freq_n, prec) {

    n_var <- length(var)
    aa <- 1 / (1 + 2 * tc * (1/freq_n))
    bb <- (1 - 2 * tc * (1/freq_n)) * aa
    new_var <- numeric(length = n_var)
    new_var[1] <- var[1]

    for(jj in 2:n_var) {
      new_var[jj] <- aa*(var[jj]+var[jj-1]) - bb * new_var[jj-1]
    }

    new_var <- round(new_var, digits = prec)
    return(new_var)
  }

  # Forwards and backwards
  pass_1 <- lp(var = x,
               tc = time_constant,
               freq_n = freq_n,
               prec = precision)
  pass_2 <- lp(var = rev(pass_1),
               time_constant,
               freq_n = freq_n,
               prec = precision)
  x_out <- rev(pass_2)

  return(x_out)

}
