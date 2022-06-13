#' Static Choropleth
#' 
#' @description Static choropleth map of France.
#' 
#' @param df `dataframe / tibble / datatable` Dataset.
#' @param x `chr` Column name of variable to be plotted in the choropleth.
#' @param x_range `vector` Min and max value to be used when setting the color scale of `x`. If NULL,
#'   the min and max observed in `x` will be used. Default is NULL.
#' @param date `date` Date to be plotted. If NULL, the most recent available date is used. Default 
#'   is NULL.
#' @param map `sf` An optional simple features dataset containing GIS data to join with df if
#'   not using one of the default maps. Default is NULL.
#' @param factor_palette `vector` Named list of colors (values) and label names (keys) to use for 
#'   the color scale. This option is intended for categorical variables and if present `plot_map()`
#'   will ignore `n_breaks` and `color_palette`.
#' @param n_breaks `int` Number of desired breaks in `x`; to be passed to [base::pretty()]. Default 
#'   is 5.
#' @param color_palette `palette` Color palette to be used. If none is provided, a reversed version 
#'   of 'YlOrRd' from [RColorBrewer::brewer.pal()] will be used.
#' @param border_color `chr` Color of the borders.
#' @param border_size `int` Size of the borders.
#' @param percent `bool` Whether to format the the legend labels as percents. Default is FALSE.
#' @param si_notation `bool` Whether to format the legend labels with a limited si notation (k for 
#'   thousands, M for millions). Default is FALSE. 
#' @param keywidth `int` Width of legend color bar. Note this is the length of the whole bar, not a
#'   single segment. Default is 70. 
#' @param label `bool` If TRUE, id labels will appear on the map. Default is FALSE.
#' @param label_column `chr` String indicating the column name to be used for labeling if `label` is
#'   TRUE. Default is either 'dep', 'reg', or 'epci' based on the `level` specified. 
#' @param label_size `int` Text size of labels; ignored if `label` is FALSE. Default is 3.
#' @param title `chr` Title.
#' @param subtitle `chr` Subtitle.
#' @param caption `chr` Caption.
#' @param legend_title `chr` Legend Title.
#' 
#' @return `ggplot` map.
#' 
#' @examples
#' library(dplyr)
#'  
#' df_dep %>%
#'   plot_map(x = 'p',
#'            title = 'SARS-COV-2 Cases in France',
#'            subtitle = 'Data updated 7 January 2020',
#'            caption = 'Map data: https://france-geojson.gregoiredavid.fr/')
#'  
#' @importFrom dplyr %>% filter pull mutate right_join
#' @importFrom grid unit
#' @importFrom ggplot2 ggplot aes_string geom_sf scale_fill_manual coord_quickmap labs guide_legend 
#' @importFrom ggrepel geom_text_repel
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang .data
#' @export
plot_map <- function(df, x, x_range = NULL, date = NULL, map = NULL, 
                     factor_palette = NULL,
                     color_palette = NULL, n_breaks = 5, 
                     border_color = "white", border_size = 0, 
                     percent = FALSE, si_notation = TRUE,
                     keywidth = 70,
                     label = FALSE, label_column = level, label_size = 3,
                     title = NULL, subtitle = NULL, caption = NULL, 
                     legend_title = NULL) {
  level <- match.arg(level)

  # generate joined map data -----
  # date filtering -----
  if ('date' %in% names(df)) {
    plot_date <- ifelse(is.null(date), max(df$date), date)

    df <- df %>% filter(.data$date == plot_date)   
  }

  map_data <- make_map_data(df,
                            level = level,
                            date = date,
                            custom_map = custom_map,
                            sp_output = FALSE)

  # customize color palette -----
  if (is.null(factor_palette)) {
    # CREATE PRETTY BREAKS AND LEGEND LABELS -----
    if (is.null(x_range)) {
      breaks <- pretty(df[[x]], n_breaks)
    } else {
      breaks <- pretty(min(x_range):max(x_range), n_breaks)
    }

    if (Inf %in% df[[x]]) {
      breaks <- c(breaks, Inf)
    }
    
    if (-Inf %in% df[[x]]) {
      breaks <- c(-Inf, breaks)
    }

    map_data <- map_data %>%
                  mutate('{x}' := cut(.data[[x]],
                                      breaks = breaks,
                                      include.lowest = TRUE))

    if (si_notation) {
      legend_labels <- breaks[-1] %>% lapply(si_notation)
    }

    if (percent) {
      legend_labels <- lapply(breaks[-1], function(x) french_format(x, percent = TRUE))
    }

    legend_labels[[1]] <- legend_labels[[1]] %>% paste('\u2264', .)

    if (is.null(color_palette)) {
      color_palette <- brewer.pal(length(legend_labels), 'YlOrRd')
      # color_palette <- centered_palette(legend_labels,
      #                                   n_breaks,
      #                                   show_palette = FALSE)
    }

    color_scale <- scale_fill_manual(breaks = rev(levels(map_data[[x]])),
                                     values = rev(color_palette),
                                     labels = rev(legend_labels),
                                     drop = FALSE,
                                     name = legend_title, 
                                     guide = guide_legend(direction = 'horizontal',
                                                          keyheight = unit(2, 
                                                                           units = 'mm'), 
                                                          keywidth = unit(keywidth / length(legend_labels),
                                                                          units = 'mm'),
                                                          nrow = 1,
                                                          byrow = TRUE,
                                                          reverse = TRUE,
                                                          title.hjust = 0.5,
                                                          title.position = 'top',
                                                          label.hjust = 1,
                                                          label.position = 'bottom')) 

  } else {
    color_scale <- ggplot2::scale_fill_manual(values = unname(factor_palette),
                                              labels = names(factor_palette),
                                              name = legend_title,
                                              drop = FALSE)
  }


  # build map -----
  p <- map_data %>%
         ggplot2::ggplot() +
         # geom_sf(fill = '#f8f8f8') +
         ggplot2::geom_sf(ggplot2::aes_string(fill = x),
                          color = border_color,
                          size = border_size) +
         color_scale +
         theme_map() +
         ggplot2::labs(title = title,
                       subtitle = subtitle,
                       caption = caption)

  if (label) {
    p <- p +
           ggrepel::geom_text_repel(ggplot2::aes_string(geometry = 'geometry',
                                      label = label_column),
                           stat = 'sf_coordinates',
                           size = label_size, 
                           point.padding = -15,
                           fontface = 'bold',
                           bg.color = 'white',
                           bg.r = 0.1,
                           seed = 9,
                           min.segment.length = 0,
                           max.overlaps = 100
                           )
  }

  return(p)
}


#' Prepare map data
#' 
#' @description Prepares choropleth map data for either [plot_map()] (static) or [plot_leaflet()] 
#' (interactive). This function will use GIS data internal to `pitoolbox` for maps at the epci, 
#' department, and regional level. These internal datasets contain two features: `id` and `name`. A 
#' custom map can be provided if desired. The function also filters data by date as needed, 
#' defaulting to the final observation of the provided data. **WARNING:** date data, if present, 
#' *must* be stored in a column called 'date' for the filtering to work.
#' 
#' @param df `dataframe / tibble / datatable` Data to be plotted on the choropleth.
#' @param date `date` Date to be plotted. If NULL, the most recent available date is used. Default 
#'   is NULL.
#' @param sp_output `bool` If True, a spatial object (sp class) will be returned, if False a simple 
#'   features object (sf class) will be returned. This choice will depend on the plotting library 
#'   used to render the map data; for example, leaflet will require sp and ggplot will require sf.
#'   Default is TRUE.
#' @param level `chr` One of 'epci', 'dep', or 'reg' indicating the resolution of gis data to use. 
#'   This argument is ignored if `custom_map` is present. Default is 'dep'.
#' @param custom_map `sf` An optional simple features dataset containing GIS data to join with df if
#'   not using one of the default maps. Default is NULL.
#' 
#' @import sf
#' @importFrom dplyr %>% filter left_join
#' @importFrom methods as
#' @export
make_map_data <- function(df, date = NULL, level = c('dep', 'epci', 'reg'), custom_map = NULL, 
                          sp_output = TRUE) {

  # date filtering -----
  if ('date' %in% names(df)) {
    plot_date <- ifelse(is.null(date), max(df$date), date)

    df <- df %>% filter(.data$date == plot_date)   
  }

  # get gis data -----
  if (is.null(custom_map)) {
    level <- match.arg(level)

    map_data <- switch(level,
                       epci = sf_epci,
                       dep = sf_department,
                       reg = sf_region)
  } else {
    if ('sf' %notin% class(custom_map)) {
      stop('custom_map must be of type sf (simple features object)')
    }

    map_data <- custom_map
  }

  # join df data and convert to spatial class if needed-----
  map_data <- map_data %>%
                left_join(df,
                          sort = FALSE)

  if (sp_output) {
    map_data <- map_data %>% as('Spatial')
  }

  return(map_data)
}

