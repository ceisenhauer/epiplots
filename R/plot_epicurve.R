#' Basic Epicurve
#' 
#' @description Basic epicurve with the option to automatically aggregate the data.
#' 
#' @param df `dataframe / tibble / datatable` Dataset.
#' @param x `chr` Column name of x axis data. Default is 'date'. **Warning**: this column *must* be
#'   datetime formatted.
#' @param y `chr` Character string of column to be plotted on the y axis. 
#' @param date_breaks `chr` Character string indicating the time betweek x-axis ticks. Must be 
#'   expressed in the format '(#) (time unit)', for example '1 week', or '2 months'. **Warning**: 
#'   this parameter must be all lowercase; '1 Week', for example, will return an error. Default is 
#'   '2 months'.
#' @param color `chr` Bar color to be used. Default is #2E4172 (dark blue).
#' @param date_limits `date vct` Axis limits to be used. Default is NULL, allowing
#'   `ggplot2::scale_x_date()` to set the limits automatically. 
#' @param y_limits `vct` Vector of two dbls indicating the lower and upper bounds of the y axis, 
#'   eg: `c(0, 2000)`. If NULL, limits are calculated automatically based on the data. Default is
#'   NULL. **NOTE**: providing `y_limits` will overide `y_breaks`. 
#' @param y_breaks `int` Number of desired breaks on the y-axis; to be passed to [base::pretty()]. 
#'   Default is 3. 
#' @param y_range `dbl series` Series to be used by [base::pretty()] when calculating y-axis breaks.
#' @param xlab `chr` X-axis label.
#' @param ylab `chr` Y-axis label.
#' @param title `chr` Title.
#' @param subtitle `chr` Subtitle.
#' @param caption `chr` Caption.
#' @param ... Parameters to be sent to theme_basic, eg `base_size`. 
#'  
#' @return `ggplot` of timeseries data.
#' 
#' @importFrom dplyr %>% group_by summarize
#' @importFrom ggplot2 ggplot aes_string geom_col scale_y_continuous labs ylim
#' @export
plot_epicurve <- function(df, y = 'cases', x = 'date', 
                          color ='#2E4172', 
                          date_breaks = '2 months', date_limits = NULL,
                          y_limits = NULL, y_breaks = 3, y_range = NULL,
                          xlab = NULL, ylab = NULL,
                          title = NULL, subtitle = NULL, caption = NULL, ...) {

  if (length(df[[x]]) != length(unique(df[[x]]))) {
    warning('multiple observations found for one or more dates, plot reflects their sum')
  }

  df <- df %>%
          group_by(.data[[x]]) %>%
          summarize(aggregated = sum(.data[[y]], na.rm = TRUE))

  df[[y]] <- df$aggregated

  # update y_range for pretty breaks
  if (is.null(y_range)) {
    y_range <- df[[y]]
  }

  # initialize plot with base aesthetics
  p <- df %>% 
        ggplot2::ggplot(ggplot2::aes_string(x = x,
                          y = y)) +
        ggplot2::geom_col(fill = color) +
        date_axis(date_breaks,
                  limits = date_limits) +
        theme_clean() +
        ggplot2::labs(title = title,
                      subtitle = subtitle,
                      caption = caption,
                      x = xlab,
                      y = ylab)

  if (is.null(y_limits)) {
    p <- p +  
         scale_y_continuous(breaks = pretty(y_range, y_breaks))
  } else {
    p <- p +
          ylim(y_limits[[1]], y_limits[[2]])
  }

  return(p)
}

