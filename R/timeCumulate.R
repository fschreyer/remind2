#' timeCumulate
#'
#' calculate cumulated values over time for a magpie object
#'
#' @author Falk Benke
#' @param x a mapgie object
#' @param includeEndYear boolean indicating logic for summing up
#' when set to TRUE, the value for 2010 is the sum of the values from 2005 - 2010,
#' otherwise the value for 2010 is the sum of 2005 - 2009
#' @export
timeCumulate <- function(x, includeEndYear = FALSE) {
  if (any(is.na(x))) {
    warning("The magpie object contains NAs. This might lead to unexpected results.")
  }

  years <- getYears(x, as.integer = TRUE)

  xInt <- time_interpolate(x, seq(years[1], years[length(years)], 1))
  tmp <- x
  tmp[, , ] <- 0

  if (includeEndYear) {
    tmp[, 1, ] <- x[, 1, ]
    for (ts in 2:length(years)) {
      tmp[, ts, ] <- tmp[, ts - 1, ] + dimSums(xInt[, seq(years[ts - 1] + 1, years[ts], 1), ], dim = 2)
    }
  } else {
    for (ts in 2:length(years)) {
      tmp[, ts, ] <- tmp[, ts - 1, ] + dimSums(xInt[, seq(years[ts - 1], years[ts] - 1, 1), ], dim = 2)
    }
  }

  return(tmp)
}
