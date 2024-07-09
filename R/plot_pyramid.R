#' Pyramid Plot
#' @describeIn plot_pyramid Create pyramid plot
#' 
#' @description Pyramid plot of two variables with a common id variable; eg: incidence and 
#' positivity by department. Note, whereas traditional pyramid plots normally plot a single 
#' indicator between groups for a single variable (for example counts of men and women for various 
#' age bands), this pyramid is intended to be flexible enough to plot two unrelated indicators. As 
#' a result, `plot_pyramid()` requres the data for the left-hand and right-hand sides to be in *two 
#' separate columns*.
#' 
#' @param df `dataframe / tibble / datatable` Dataset.
#' @param id `chr` Column name of id data, to be shown in the middle of the pyramid.
#' @param val_left `chr` Column name of left-hand side data.
#' @param val_right `chr` Column name of right-hand side data.
#' @param text_size `int` Size of text labels in the middle. Default is 3.
#' @param center_width `flt` Relative width of the center section (with the text) compared to the
#'   plot areas on either side. Default is 0.5 (50% as big).
#' @param fill `chr` Column name of variable to be mapped to the bar fill. Default is NULL.
#' @param fills_left `chr/vct` Color(s) to use for the bar fill on the left-hand side. When `fill`
#'   is specified, a vector of colors should be supplied. Default is colors$blue.
#' @param color_text_left `chr` Color to use for the axis text and ticks on the left-hand axis. 
#'   Default is 'black'.
#' @param fills_right `chr/vct` Color(s) to use for the bar fill on the right-hand side. When `fill`
#'   is specified, a vector of colors should be supplied. Default is colors$blue.
#' @param color_text_right `chr` Color to use for the title and axis text / ticks on the right-hand axis. 
#'   Default is 'black'.
#' @param limits_left `vct` Limits to use for the left side of the pyramid. Default is to have
#'   limits chosen automatically (NULL).
#' @param limits_right `vct` Limits to use for the right side of the pyramid. Default is to have
#'   limits chosen automatically (NULL).
#' @param label_left `chr` Title to be shown for left-hand side (plotted on the top). Default is ''.
#' @param label_right `chr` Title to be shown for right-hand side (plotted on the top). Default is 
#'   ''.
#' @param ... Arguments to be passed to [theme_pyramid()], eg: 'base_size'.
#' 
#' @return `ggplot` Pyramid plot
#' 
#' @importFrom ggplot2 geom_col scale_y_continuous geom_text aes_string
#' @importFrom cowplot plot_grid
#' @export
plot_pyramid <- function(df, id, val_left, val_right, text_size = 5, center_width = 0.5,
                         fill = NULL,
                         fills_left = colors$blue, color_text_left = 'black',
                         fills_right = colors$blue, color_text_right = 'black',
                         limits_left = NULL, limits_right = limits_left,
                         #color_left = '#4472C4', color_right = 'darkgrey',
                         label_left = val_left, label_right = val_right, ...) {

  if (is.null(fill)) {
    geom_left <- geom_col(fill = fills_left)
    geom_right <- geom_col(fill = fills_right)
  } else {
    geom_left <- geom_col()
    geom_right <- geom_col()
  }


  p_left <- plot_pyramid_base(df = df, 
                              id = id,
                              val = val_left,
                              fill = fill,
                              fill_values = fills_left,
                              color_text = color_text_left,
                              axis_label = label_left,
                              ...) +
           geom_left +
           #scale_y_continuous(trans = 'reverse') +
           scale_y_continuous(trans = 'reverse',
                              limits = rev(limits_left)) +
           theme(plot.margin = unit(c(0.5, 0, 0.5, 0.5), "cm"))

  p_right <- plot_pyramid_base(df = df,
                               id = id,
                               val = val_right,
                               fill = fill,
                               fill_values = fills_right,
                               color_text = color_text_right,
                               axis_label = label_right,
                               ...) +
           geom_right +
           scale_y_continuous(limits = limits_right) +
           theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm"))

# TODO: remove axis everything from this plot
# or maybe NOT? because past cat was fucking clever and made it all white
# but actually probs better to remove anyways to avoid weird overlap effects ?
  p_id <- df %>%
    plot_pyramid_base(id = id,
                      val = 0,
                      color_text = 'transparent',
                      axis_label = '') +
    geom_text(aes_string(label = id),
                         size = text_size) +
    theme(plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"))
           

  p <- cowplot::plot_grid(p_left, p_id, p_right,
                 nrow = 1,
                 rel_widths = c(1, center_width, 1))

  return(p)
}

#' Pyramid Plot Base
#' 
#' @param df `dataframe / tibble / datatable` Dataset.
#' @param id `chr` Column of id values to be set on vertical axis.
#' @param val `chr` Column of values to be plotted.
#' @param fill `chr` Column name of variable to be mapped to the bar fill. Default is NULL.
#' @param fill_values `chr/vct` Color(s) to use for the bar fill. When `fill` is specified, a vector
#'   of colors should be supplied. Default is colors$blue.
#' @param color_text `chr` Color to use for axis text / ticks. Default is 'black'.
#' @param axis_label `chr` Optional title for axis values; to be displayed above the plot. Default 
#'   is ''.
#' @param ... Arguments to be passed to [theme_pyramid()], eg: 'base_size'.
#' 
#' @return `ggplot` of base pyramid plot component.
#' 
#' @importFrom ggplot2 ggplot aes_string coord_flip labs
plot_pyramid_base <- function(df, id, val, fill = NULL, fill_values = NULL, color_text = 'black',
                              axis_label = '', ...) {
  p <- ggplot(data = df,
              aes_string(x = id,
                         y = val,
                         fill = fill)) +
         scale_fill_manual(values = fill_values,
                           name = '') +
         #scale_y_continuous(limits = c(0, 30)) +
         coord_flip() + 
         labs(title = axis_label) +
         theme_pyramid(color = color_text,
                       ...)

  return(p)
}

