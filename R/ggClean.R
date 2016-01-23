#' Clean visualizations
#'
#' @description Remove unnecessary gridlines and 'de-bold' the blacks.
#'
#' @param vis A ggvis object
#' @details Like ggplot2, ggvis by default will criss-cross a plot with gridlines, something I almost never need and is completely unnecessary if one has interactive hover over effects like ggvis can do. This function takes a ggvis object and removes the gridlines and adds opacity to the default black axes and labels, allowing the pattern of the visual to be expressed in unimpeded fashion. It simply uses the add_axis function in ggvis and so any part of it can be overridden by supplying your own add_axis after. I also supply an old ggplot theme I used to use that does the same.
#'
#' @examples
#' require(lazerhawk)
#' require(ggvis)
#' data(mtcars)
#' mtcars %>% ggvis(~wt, ~mpg) %>% ggClean()

#' @export
ggClean = function(vis){
  vis %>%
    ggvis::add_axis('x', grid=F, format='####', properties = ggvis::axis_props(
      labels = list(
        fill = "black",
        fillOpacity = .5,
        angle = 45,
        fontSize = 14,
        align = "left",
        baseline = "middle",
        dx = 3),
      title = list(fill = "black",
                   fillOpacity = .75,
                   dy=20),
      axis = list(stroke = "black",
                  strokeOpacity = .5)
    )) %>%
    ggvis::add_axis('y', grid=F, format='####', properties = ggvis::axis_props(
      labels = list(
        fill = "black",
        fillOpacity = .5,
        fontSize = 14,
        dx = -2),
      title = list(fill = "black",
                   fillOpacity = .75,
                   dy=-10),
      axis = list(stroke = "black",
                  strokeOpacity = .5)
    ))
}

#' @rdname ggClean
#' @export
theme_trueMinimal = function(){
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(colour='gray50'),
    axis.text.y = ggplot2::element_text(colour='gray50'),
    legend.key = ggplot2::element_rect(fill='white'),
    legend.background = ggplot2::element_rect(fill='white'),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )
}
