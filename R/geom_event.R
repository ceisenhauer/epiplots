#' (Labeled) Event Line Geom
#' 
#' @description A geom to add a threshold line (with an optional label) to a ggplot.
#' 
#' @param line_position `dbl / date / chr` Location on x or y axis where the line should be 
#'   anchored.
#' @param orientation `chr` One of 'vertical' or 'horizontal' indicating the orientation of the 
#'   line. Default is 'vertical'.
#' @param label `chr` Optional label to add above (if horizontal) or to the left of (if vertical) 
#'   the line. Default is no label.
#' @param label_position `dbl / date / chr` Location on the x (if horizontal) or y (if vertical)  
#'   axis to center the label. **WARNING**, if a label is given to `geom_threshold` then `label_x` 
#'   *must* be specified. Default is NULL. 
#' @param color `chr` Color of line and text. Default is 'black'.
#' @param linetype `chr` Line type to be used; must be an appropriate `ggplot2` linetype. Default is
#'   'dashed'.
#' @param line_size `dbl` Line thickness. Default is 0.5.
#' 
#' @return `ggplot` geom
#' 
#' @importFrom ggplot2 aes geom_vline geom_hline annotate theme_get aes_string
#' @importFrom ggrepel geom_text_repel
#' @export
geom_event <- function(line_position, label = '', label_position = NULL, 
                       orientation = 'vertical', color = 'black', linetype = 'dashed',
                       line_size = 0.5) {
  out <- list()

  if (orientation == 'vertical') {
    out <- c(out, geom_vline(aes(xintercept = line_position),
                             color = color,
                             linetype = linetype,
                             size = line_size))
    angle <- 90
    annotate_x <- line_position
    annotate_y <- label_position
  } else {
    out <- c(out, geom_hline(aes(yintercept = line_position),
                                 color = color,
                                 linetype = linetype,
                                 size = line_size))
    angle <- 0
    annotate_x <- label_position
    annotate_y <- line_position
  }

  if (label != '') {
    if (is.null(label_position)) {
      stop('label_position is missing and must be provided if you want to include a label.')
    }

    out <- c(out,
             geom_text_repel(data = data.frame(label = paste0(label, '\n'),
                                               x = annotate_x,
                                               y = annotate_y),
                             aes_string(label = 'label',
                                        x = 'x',
                                        y = 'y'),
                             color = color,
                             inherit.aes = FALSE,
                             angle = angle,
                             bg.color = 'white',
                             bg.r = 0.1,
                             max.iter = 0)
             )
  }

  return(out)
}
