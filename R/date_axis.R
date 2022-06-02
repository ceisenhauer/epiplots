#' Date Axis
#' 
#' @description Adds an attractive date axis to a timeseries plot. `date_axis` automatically removes
#' the title elemnt from the x-axis.
#'
#' @param date_breaks `str` Breaks to be used specified with the format '\[#\] \[unit\]', for 
#'   example : '2 weeks'. Default is '3 months'.
#' @param limits `date vct` Axis limits to be used. Default is NULL, allowing
#'   `ggplot2::scale_x_date()` to set the limits automatically. 
#' @param labels `fct` Allows users to set their own labels if needed. Default is 
#'   `scales::label_date_short()`.
#' 
#' @return `list` of `ggplot2` objects
#'
#' @importFrom ggplot2 scale_x_date xlab
#' @importFrom scales label_date_short
#'
#' @export
date_axis <- function(date_breaks = '3 months', limits = NULL,
                      labels = scales::label_date_short()) {

  out <- list(ggplot2::scale_x_date(limits = limits,
                                    date_breaks = date_breaks,
                                    labels = labels),
              ggplot2::xlab(NULL))

  return(out)
}

