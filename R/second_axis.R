#' Second Axis
#'
#' @description Adds a secondary vertical axis on the right side of a ggplot based on a scale factor
#' of the main (left) vertical axis. *Note : * by  default the secondary axis will not have a line,
#' only ticks. If desired, an axis line must be added manually by the user.
#' 
#' @param scale_factor `dbl` Scale factor to convert main (left) axis into the new secondary (right)
#'   axis
#' @param percent `bool` Convenience argument generating a percent formatted axis. Default is
#'   `FALSE`.
#' @param digits `int` Number of decimal digits to use when `percent` is `TRUE`. Default is 0.
#' @param formatter `fct` Formatter to use for axis labels. This option is superceeded when 
#'   `percent` is `TRUE`. 
#' @param title `str` Axis title. Default is NULL.
#' @param color `str` Axis color (ticks, title, values). Default is grey (#c5c5c5). 
#' 
#' @return `list` of `ggplot2` objects
#'
#' @importFrom ggplot2 sec_axis scale_y_continuous theme element_text element_line
#' @importFrom scales percent_format
#' 
#' @export
second_axis <- function(scale_factor, percent = FALSE, digits = 0, formatter = NULL,
                        title = NULL, color = '#c5c5c5') {

  labels <- ifelse(percent,
                   scales::percent_format(accuracy = digits + 1),
                   formatter)

  axis_scale <- ggplot2::sec_axis(~ ./scale_factor,
                                  labels = labels,
                                  name = title)

  out <- list(ggplot2::scale_y_continuous(sec.axis = axis_scale),
              ggplot2::theme(axis.title.y.right = ggplot2::element_text(color = color),
                             axis.text.y.right = ggplot2::element_text(color = color),
                             axis.ticks.y.right = ggplot2::element_line(color = color)))

  return(out)
}

