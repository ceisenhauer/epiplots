#' Clean ggplot Theme
#' 
#' @description Adds base theme to a `ggplot`; intended to be publication ready.
#' 
#' @param base_size `int` Base size of the theme (for text sizing). Default is 14.
#' @param legend_position `chr` `ggplot2` style position for plot legend. Default is 'top'.
#' @param legend_x `dbl` Horizontal legend location. Default is NULL.
#' @param legend_y `dbl` Vertical legend location. Default is NULL.
#' @param lines `bool` Whether to plot y-axis grid lines. Default is FALSE.
#' @param title_hjust `dbl` Alignment of the plot title. Default is left aligned.
#' @param subtitle_hjust `dbl` Alignment of the subtitle. Default is to match the alignment 
#'   of the title.
#'  
#' @return `ggplot` with base theme
#' 
#' @importFrom ggplot2 theme theme_classic element_text element_blank %+replace%
#' 
#' @export
theme_clean <- function(base_size = 20, legend_position = 'top', legend_x = NULL, legend_y = NULL,
                        lines = FALSE, 
                        title_hjust = 0, subtitle_hjust = title_hjust) {
 if (lines) {
    panel_grid <- ggplot2::element_line(color = 'grey',
                               linetype = 'dashed')
  } else {
    panel_grid <- ggplot2::element_blank()
  } 

  if (!is.null(legend_x) & !is.null(legend_y)) {
    legend_position = c(legend_x, legend_y)
  }

  out <- ggplot2::theme_classic(base_size = base_size) %+replace%

         ggplot2::theme(plot.title = ggplot2::element_text(size = base_size,
                                                           face = 'bold',
                                                           hjust = title_hjust),
                        plot.subtitle = ggplot2::element_text(size = base_size * 0.9,
                                                              color = 'darkgrey',
                                                              hjust = subtitle_hjust),
                        plot.caption = ggplot2::element_text(size = base_size * 0.75,
                                                             color = 'darkgrey',
                                                             face = 'italic',
                                                             hjust = 1),

                        axis.title = ggplot2::element_text(size = base_size * 0.9,
                                                           face = 'bold'),
                        axis.text = ggplot2::element_text(size = base_size * 0.9),
                        axis.line = ggplot2::element_blank(),

                        legend.position = legend_position,
                        legend.text = ggplot2::element_text(size = base_size * 0.75),
                        legend.title = ggplot2::element_text(size = base_size * 0.8,
                                                             face = 'bold'),

                        panel.grid.major.y = panel_grid,

                        plot.margin = ggplot2::margin(0.5, 1, 0.5, 0.5, 'cm')
         )

  return(out)
}



#' Add the basic map theme to a ggplot map
#' 
#' @description Adds base map theme to a `ggplot`; intended to be publication ready.
#' 
#' @param base_size `int` Base size of the theme (for text sizing).
#' @param title_hjust `num` Hjust.
#'  
#' @return `ggplot` with base map theme
#' 
#' @importFrom ggplot2 theme element_blank %+replace%
#' @export
theme_map <- function(base_size = 20, title_hjust = 0) {
  out <- theme_clean(base_size = base_size,
                     title_hjust = title_hjust) %+replace%

         theme(axis.line = element_blank(),
               axis.title = element_blank(),
               axis.text = element_blank(),
               axis.ticks = element_blank(),

               legend.position = 'bottom'
         )

  return(out)
}



#' Add the basic pyramid theme to a ggplot
#' 
#' @description Adds base pyramid theme to a `ggplot`; intended to be publication ready. Note, the 
#' final pyramid plot is actually a grid of three plots, so this theme is to be used for each.
#' 
#' @param base_size `int` Base size of the theme (for text sizing).
#' @param color `chr` Color to be used for the horizontal axis. Default is 'black'.
#'  
#' @return `ggplot` with base pyramid theme
#' 
#' @importFrom ggplot2 theme element_text element_line element_blank %+replace%
#' @importFrom grid unit
#' @export
theme_pyramid <- function(base_size = 20, color = 'black') {
  out <- theme_clean(base_size = base_size) %+replace%

         theme(plot.title = element_text(size = base_size * 1,
                                         family = 'arial',
                                         face = 'bold',
                                         color = color,
                                         hjust = .5),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.title.x = element_blank(),
               axis.text.x = element_text(color = color,
                                          family = 'arial'),
               axis.ticks.x = element_line(color = color),

               legend.position = 'none', 

               plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
         )

  return(out)
}
