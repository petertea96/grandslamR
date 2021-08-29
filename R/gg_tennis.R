
#' This function draws a full tennis court.
#' @return A ggplot of a tennis court (dimensions in metres)
#' @export
#' @examples
#' ggplot() + gg_fulltenniscourt()

gg_fulltenniscourt <- function(fill_colour ='#3C638E',
                                outer_fill = "#ad5049",
                                line_colour = 'white'){
  list(
    # -- Two outer doubles lines
    geom_rect(mapping=aes(xmin=-11.89, xmax=11.89, ymin=-5.485, ymax=-4.115),
              color=line_colour, alpha=0.5, fill = fill_colour),
    geom_rect(mapping=aes(xmin=-11.89, xmax=11.89, ymin= 4.115, ymax= 5.485),
              color=line_colour, alpha=0.5, fill = fill_colour),
    # -- Service boxes
    geom_rect(mapping=aes(xmin=-6.4, xmax=0, ymin=-4.115, ymax=0),
              color=line_colour, alpha=0.5, fill = fill_colour),
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=-4.115),
              color=line_colour, alpha=0.5, fill = fill_colour),
    geom_rect(mapping=aes(xmin=-6.4, xmax=0, ymin=0, ymax= 4.115),
              color=line_colour, alpha=0.5, fill = fill_colour),
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=4.115),
              color=line_colour, alpha=0.5, fill = fill_colour),
    # -- Baseline
    geom_rect(mapping=aes(xmin=-11.89, xmax=-6.4, ymin=-4.115, ymax=4.115),
              color=line_colour, alpha=0.5, fill = fill_colour),
    geom_rect(mapping=aes(xmin=6.4, xmax=11.89, ymin=-4.115, ymax=4.115),
              color=line_colour, alpha=0.5, fill = fill_colour),

    # -- Emphasize the net
    geom_segment(aes(x = 0, xend = 0, y = -5.485, yend = 5.485),
                 size = 1),
    labs(x = '', y = ''),
    theme_classic(),
    theme(panel.background = element_rect(fill=outer_fill),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          #axis.text.y=element_blank(),
          #axis.ticks.y=element_blank(),
          axis.line=element_blank(),
          strip.text = element_text(colour = 'black',face = 'bold')))
}


#' This function draws half of a symmetric tennis court.
#' @return A ggplot of a tennis court (dimensions in metres)
#' @export
#' @examples
#' ggplot() + gg_halftenniscourt()

gg_halftenniscourt <- function(fill_colour ='#3C638E',
                                outer_fill = "#ad5049",
                                line_colour = 'white'){
  list(
    # -- Two outer doubles lines
    geom_rect(mapping=aes(xmin=-0.5, xmax=11.89, ymin=-5.485, ymax=-4.115),
              size = 0.75,
              color=line_colour, alpha=0.25, fill = fill_colour),
    geom_rect(mapping=aes(xmin=-0.5, xmax=11.89, ymin= 4.115, ymax= 5.485),
              size = 0.75,
              color=line_colour, alpha=0.25, fill = fill_colour),
    # -- Service boxes
    geom_rect(mapping=aes(xmin=-0.5, xmax=0, ymin=-4.115, ymax=0),
              color=line_colour, size = 1, alpha=0.25, fill = fill_colour),
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=-4.115),
              color=line_colour, size = 1, alpha=0.25, fill = fill_colour),
    geom_rect(mapping=aes(xmin=-0.5, xmax=0, ymin=0, ymax= 4.115),
              color=line_colour, size = 1, alpha=0.25, fill =fill_colour),
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=4.115),
              color=line_colour, size = 1, alpha=0.25, fill = fill_colour),
    # -- Baseline
    #geom_rect(mapping=aes(xmin=-11.89, xmax=-6.4, ymin=-4.115, ymax=4.115),
    #          color="black", alpha=0.5, fill = 'white'),
    geom_rect(mapping=aes(xmin=6.4, xmax=11.89, ymin=-4.115, ymax=4.115),
              color=line_colour, size = 1, alpha=0.25, fill = fill_colour),

    # -- Add serve center marker
    geom_rect(mapping=aes(xmin=11.5, xmax=11.89, ymin=0, ymax=0),
              color=line_colour, size = 1.25),
    # -- Emphasize the net
    geom_segment(aes(x = 0, xend = 0, y = -5.485, yend = 5.485),
                 size = 1, colour = 'black'),
    labs(x = '', y = ''),
    theme_classic(),
    theme(panel.background = element_rect(fill=outer_fill), #99e6b3,#aaf0d1 <-- Miami teal colours
          plot.title = element_text(hjust = 0.5, face = "bold"),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          #axis.text.y=element_blank(),
          #axis.ticks.y=element_blank(),
          axis.line=element_blank(),
          strip.text = element_text(colour = 'black',face = 'bold'))

  )
}
