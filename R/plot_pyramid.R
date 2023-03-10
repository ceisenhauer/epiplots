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
#' @param color_left `chr` Optional color specification of left-hand side data. This argument will 
#'   also color the title, ticks, and axis values of the left-hand side. Default is '#4472C4' 
#'   (blue).
#' @param color_right `chr` Optional color specification of right-hand side data. This argument will 
#'   also color the title, ticks, and axis values of the right-hand side. Default is 'darkgrey'.
#' @param label_left `chr` Title to be shown for left-hand side (plotted on the top). Default is ''.
#' @param label_right `chr` Title to be shown for right-hand side (plotted on the top). Default is 
#'   ''.
#' @param ... Arguments to be passed to [theme_pyramid()], eg: 'base_size'.
#' 
#' @return `ggplot` Pyramid plot
#' 
#' @examples
#' library(dplyr)
#' 
#' region <- 84
#' deps <- geo_dictionary %>% filter(reg == region) %>% pull(dep)
#' 
#' tmp <- df_dep %>%
#'   filter(dep %in% deps &
#'          date == max(date)) %>%
#'   add_indicators(add_evolution = FALSE) %>%
#'   left_join(geo_dictionary[ , c('dep', 'nom_dep')]) %>%
#'   mutate(name = paste0(nom_dep, ' (', dep, ')') %>% factor() %>% forcats::fct_rev())
#' 
#' plot_pyramid(df = tmp,
#'              id = 'name',
#'              val_left = 'ti',
#'              val_right = 'p',
#'              label_left = "Taux d'Incidence",
#'              label_right = 'Nombre de Cas')
#' 
#' @importFrom ggplot2 geom_col scale_y_continuous geom_text aes_string
#' @importFrom cowplot plot_grid
#' @export
plot_pyramid <- function(df, id, val_left, val_right, text_size = 5, center_width = 0.5, fill,
                         colors_right, color_text_right, colors_left, color_text_left,
                         color_left = '#4472C4', color_right = 'darkgrey',
                         label_left = val_left, label_right = val_right, ...) {

  p_left <- plot_pyramid_base(df = df, 
                              id = id,
                              val = val_left,
                              fill = fill,
                              fill_colors = colors_left,
                              color = color_text_left,
                              axis_label = label_left) +
                              #...) +
           geom_col() +
           scale_y_continuous(trans = 'reverse')

  p_right <- plot_pyramid_base(df = df,
                               id = id,
                               val = val_right,
                               fill = fill,
                               fill_colors = colors_right,
                               color = color_text_right,
                               axis_label = label_right) +
                               #...) +
           geom_col()

  p_id <- plot_pyramid_base(df = df,
                            id = id,
                            val = 0,
                            color = 'white',
                            axis_label = '') +
            geom_text(aes_string(label = id),
                                 size = text_size)

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
#' @param color `chr` Color to use for bars, title, and axis text / ticks. Default is 'black'.
#' @param axis_label `chr` Optional title for axis values; to be displayed above the plot. Default 
#'   is ''.
#' @param ... Arguments to be passed to [theme_pyramid()], eg: 'base_size'.
#' 
#' @return `ggplot` of base pyramid plot component.
#' 
#' @importFrom ggplot2 ggplot aes_string coord_flip labs
plot_pyramid_base <- function(df, id, val, fill = NULL, fill_colors = NULL, color = 'black',
                              text_color = color, axis_label = '', ...) {
  p <- ggplot(data = df,
              aes_string(x = id,
                         y = val,
                         fill = fill)) +
         scale_fill_manual(values = fill_colors,
                           name = '') +
         #scale_y_continuous(limits = c(0, 30)) +
         coord_flip() + 
         labs(title = axis_label) +
         theme_pyramid(color = text_color,
                       ...)

  return(p)
}

