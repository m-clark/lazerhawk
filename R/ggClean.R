#' Clean visualizations
#'
#' @description Clean up plots from their defaults.
#'
#' @param vis A plotly, or ggvis
#' @details From a gray background, to unnecessary gridlines, to by-default reference lines, some of the more popular visualization packages come out 75% fantastic and 25% wtf?.  These functions remove unnecessary gridlines, 'de-bold' the blacks, etc.
#' **ggplot2**: This function takes a ggplot object and removes the gray background, gridlines and adds opacity to the default black axes and labels, allowing the pattern of the visual to be expressed in unimpeded fashion.
#'
#' While ggvis looks to be abandonware at this point, and as such I'm not really using it anymore, I leave this here just in case. Like ggplot2, ggvis by default will criss-cross a plot with gridlines, something I almost never need and is completely unnecessary if one has interactive hover over effects like ggvis can do.  It simply uses the add_axis function in ggvis and so any part of it can be overridden by supplying your own add_axis after.
#'
#' Plotly will put reference lines at zero, and some of its 'modebar' is unnecessary.  Otherwise little is changed at this point.
#'
#' @examples
#' require(lazerhawk)
#' require(ggplot2)
#' require(ggvis)
#' require(plotly)
#' data(mtcars)
#' mtcars %>% ggplot(aes(wt, mpg)) + theme_trueMinimal()
#' mtcars %>% ggvis(~wt, ~mpg) %>% theme_ggvis()
#' mtcars %>% plot_ly(x=~wt, y=~mpg, mode='markers') %>% theme_plotly()



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

#' @rdname theme_trueMinimal
#' @export
theme_ggvis = function(vis){
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



#' @rdname theme_trueMinimal
#' @export
theme_plotly = function(vis) {
  vis %>%
  plotly::layout(xaxis = list(zeroline=F,
                              showgrid=F),
                 yaxis = list(zeroline=F,
                              showgrid=F)) %>%
    plotly::config(showLink = F,
                   displaylogo = FALSE,
                   modeBarButtonsToRemove = list('pan2d'))
}
