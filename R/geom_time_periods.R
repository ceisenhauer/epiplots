#' Label Sections of a Timeseries
#'
#' @description Adds vertical lines to a time series plot cutting the plot into different sections, 
#' typically different waves of an epidemic. A horizontally centered label is added to each section
#' at a user specified height. **Warning** : this is an opinionated function with specific column
#' name requirements in the `periods` argument.
#' 
#' @param y `dbl` Y axis value for section labels.
#' @param periods `df` Data frame contaning time period labels, start dates, and end dates. These 
#'   columns *must* be named "label", "start", and "end" respectively. 
#' @param color `str` Color to use for the vertical lines and labels. Default is grey (#c5c5c5). 
#' 
#' @return `list` of `ggplot2` objects
#' 
#' @importFrom ggplot2 geom_vline annotate
#' 
#' @export
geom_time_periods <- function(y, periods, color = '#c5c5c5') {
  mid_points <- periods$start + floor((periods$end - periods$start) / 2) 

  out <- list()
  
  for (day in periods$end[-nrow(periods)]) {
    out <- c(out,
             ggplot2::geom_vline(xintercept = day,
                                 color = color,
                                 linetype = 'longdash'))
  }

  for (i in 1:nrow(periods)) {
    out <- c(out,
             ggplot2::annotate(geom = 'text',
                               x = mid_points[[i]],
                               y = y,
                               label = periods$label[[i]],
                               color = color,
                               size = 5,
                               fontface = 2))
  }

  return(out)
}

