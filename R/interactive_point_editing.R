#' Interactively remove points from a plot
#'
#' Iteratively remove points from a `data.frame` by clicking on them in a plot.
#'
#' @param x A `data.frame` containing at least two numeric columns.
#' @param x_col A string specifying the name of the column to use for the x-axis (e.g. "DATE_TIME").
#' @param y_col A string specifying the name of the column to use for the y-axis (e.g. "TEMPERATURE").
#' @param tol A numeric value specifying the tolerance for selecting a point. A click farther than `tol * max(range)` from the closest point is ignored. 0.5
#'
#' @return A `data.frame` with selected points removed.
#' @noRd
#' @import graphics
#' @examples
#' \dontrun{
#' df <- data.frame(x = rnorm(20), y = rnorm(20))
#' cleaned_df <- interactive_point_removal(df, "x", "y")
#' }

interactive_point_editing <- function(x, x_col, y_col, tol = 0.5) {

  par(mfrow = c(1,1))

  x_vals <- x[[x_col]]
  y_vals <- x[[y_col]]

  repeat{
    plot(x_vals, y_vals, main = "Click on a point to remove it (ESC or right-click to finish)",
         xlab = x_col, ylab = y_col, pch = 19)

    # clicked <- identify(x_vals, y_vals)
    # Selet the point
    clicked <- locator(1)

    # Exit on ESC or right-click
    if(is.null(clicked)) {
      message("Point removal finished.")
      break
    }

    # Calculate distances to click - datetimes are scaled to ensure time doesn't dominate distance calcs
    dist <- sqrt((as.numeric(x_vals)/1e10 - as.numeric(clicked$x)/1e10)^2 + (y_vals - clicked$y)^2)

    # Find closest point
    closest_index <- which.min(dist)

    # Check if the click is close enough
    if(dist[closest_index] > tol * max(diff(range(as.numeric(x_vals)/1e10)), diff(range(y_vals)))) {
      message("Click was too far from any point. Try again.")
      next
    }

    # Remove the point
    x_vals <- x_vals[-closest_index]
    y_vals <- y_vals[-closest_index]

    message("Removed ", paste(unlist(as.vector(x[closest_index, ])), collapse = ", "), "\n")

    x <- x[-closest_index, ]
  }

  return(x)
}
