#' D3 Heatmap widget for correlation matrices
#'
#' @description  Creates a D3.js-based heatmap widget specifically geared toward
#'   correlation matrices. All dendrogram related functionality related to the
#'   original \code{d3heatmap} function is removed, and replaced with an
#'   ordering based on a factor analysis.
#'
#' @param x A complete correlation matrix
#' @param psychOptions a \emph{named} list(!) of options to be passed to the psych package.
#'   Do not include the \code{nfactors} argument, as that is chosen
#'   automatically.
#' @param factanalOptions a \emph{named} list(!) of options to be passed to the factanal
#' function in base R. \code{psychOptions} will be checked first, and if present
#' the psych package will be used and override this argument. Do not include the
#' \code{factors} argument, as that is chosen automatically.
#' @param ordering Order cols/rows based on \code{psych::fa.sort} (default), max
#'   raw ("raw"), or absolute ("absolute") loadings across all factors, or based
#'   on the first
#'   factor's raw loadings from \code{psych::fa} or \code{factanal}.
#' @param labRow character vectors with row labels to use (from top to bottom);
#'   default to rownames(x).
#' @param labCol character vectors with column labels to use (from left to
#'   right); default to colnames(x).
#' @param cexRow positive numbers. If not missing, it will override
#'   \code{xaxis_font_size} and will give it a value cexRow*14
#' @param cexCol positive numbers. If not missing, it will override
#'   \code{yaxis_font_size} and will give it a value cexCol*14
#' @param theme A custom CSS theme to use. Currently the only valid values are
#'   \code{""} and \code{"dark"}. \code{"dark"} is primarily intended for
#'   standalone visualizations, not R Markdown or Shiny.
#' @param width Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @param xaxis_height Size of axes, in pixels.
#' @param yaxis_width Size of axes, in pixels.
#' @param xaxis_font_size Font size of axis labels, as a CSS size (e.g. "14px"
#'   or "12pt").
#' @param yaxis_font_size Font size of axis labels, as a CSS size (e.g. "14px"
#'   or "12pt").
#' @param brush_color The base color to be used for the brush. The brush will be
#'   filled with a low-opacity version of this color. \code{"#RRGGBB"} format
#'   expected.
#' @param show_grid \code{TRUE} to show gridlines, \code{FALSE} to hide them, or
#'   a numeric value to specify the gridline thickness in pixels (can be a
#'   non-integer).
#' @param anim_duration Number of milliseconds to animate zooming in and out.
#'   For large \code{x} it may help performance to set this value to \code{0}.
#' @param digits integer indicating the number of decimal places to be used by
#'   \link{round} for 'label'.
#' @param cellnote (optional) matrix of the same dimensions as \code{x} that has
#'   the human-readable version of each value, for displaying to the user on
#'   hover. If \code{NULL}, then \code{x} will be coerced using
#'   \code{\link{as.character}}. If missing, it will use \code{x}, after
#'   rounding it based on the \code{digits} parameter.
#' @param cellnote_scale logical (default is FALSE). IF cellnote is missing and
#'   x is used, should cellnote be scaled if x is also scaled?
#' @param ... currently ignored
#'
#'
#' @details d3heatmap is a great tool for matrix visualization and highly
#'   recommended. Correlation matrices are typically better visualized rather
#'   than parsed numerically, and while one can do so with d3heatmap, one often
#'   may not want the cluster based approach to ordering if dealing with a
#'   correlation matrix, which may be too small column-wise to be useful for a
#'   cluster analysis, or may be a specific type of data more amenable to a
#'   measurement error approach (e.g. items from a particular scale).
#'
#'   \code{corrheat} produces a color coded matrix in which Blue represents
#'   positive, and Red, negative correlations, and fades to white the smaller
#'   the values are.  The ordering is based on the results of a factor analysis from
#'   the \code{\link[psych]{fa}} package (which is required).  Though one can
#'   use factanal from base R, it's not recommended, and can actually be
#'   reproduced with an additional argument to the psych options (as such I will
#'   deprecate this option). This function does not currently allow choice of
#'   the number of factors. The number of factors is chosen to more likely 'just
#'   work' for visualization purposes (\code{nfact = 1} if \code{ncol <=4}, else
#'   \code{floor(sqrt(ncol(x)))}), which is all we are worried about here. If
#'   you want explore a factor analysis you will have to do that separately.
#'
#'
#' @source Base code comes from \link[d3heatmap]{d3heatmap} package's core
#'   function, which was mostly gutted, and all dendrogram functionality
#'   replaced with a factor analytic approach.
#'
#' @seealso \code{\link[psych]{fa}}, \code{\link[stats]{factanal}}
#'
#' @examples
#' library(lazerhawk)
#' corrheat(cor(mtcars), factanalOptions=list(rotation='varimax'), ordering='absolute')
#' corrheat(cor(mtcars), factanalOptions=list(rotation='varimax'), ordering='raw')
#' corrheat(Harman74.cor$cov, psychOptions=list(fm='ml'))
#' corrheat(cor(state.x77), psychOptions=list(fm='ml'), ordering='raw')
#'
#'
#' @import htmlwidgets
#' @importFrom scales rescale_mid
#' @importFrom stats factanal


#'
#' @export
corrheat <- function(x,

                     # fa related
                     psychOptions=NULL,
                     factanalOptions=NULL,
                     ordering= c('fa','raw','first', 'absolute'),

                     # labeling
                     labRow = rownames(x),
                     labCol = colnames(x),
                     cexRow,
                     cexCol,

                     ## value formatting
                     digits = 3L,
                     cellnote,
                     cellnote_scale = FALSE,

                     ## visual options
                     theme = NULL,
                     width = NULL, height = NULL,
                     xaxis_height = 80,
                     yaxis_width = 120,
                     xaxis_font_size = NULL,
                     yaxis_font_size = NULL,
                     brush_color = "#0000FF",
                     show_grid = TRUE,
                     anim_duration = 500,
                     ...
) {

  ## Check that x is a square, symmetric matrix
  ##====================
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if(!is.matrix(x)) stop("x must be a matrix")

  nr <- dim(x)[1]
  nc <- dim(x)[2]

  if(nr != nc) stop("x must be a square matrix")

  if(!all.equal(sort(x[lower.tri(x)]), sort(x[upper.tri(x)]))) stop('x must be a symmetric matrix')

  ## Labels for Row/Column
  ##======================
  if(is.null(labRow)) rownames(x) <-  paste0('var', 1:nr)
  if(is.null(labCol)) colnames(x) <-  paste0('var', 1:nc)

  if(!missing(cexRow)) {
    if(is.numeric(cexRow)) {
      yaxis_font_size <- cexRow * 14   # axes switched
    } else {
      warning("cexRow is not numeric. It is ignored")
    }
  }
  if(!missing(cexCol)) {
    if(is.numeric(cexCol)) {
      xaxis_font_size <- cexCol * 14
    } else {
      warning("cexCol is not numeric. It is ignored")
    }
  }

  ## Factor analysis
  ##====================
  # number of factors to use.
  if(nc <= 4) {
    nf = 1
  } else {
    nf = floor(sqrt(nc))
  }

  whichfunc = c(is.null(psychOptions), is.null(factanalOptions))
  if (all(whichfunc)) message('No FA options specified, using psych package defaults')

  if((all(whichfunc) | !is.null(psychOptions)) && requireNamespace('psych')) {
    if(!is.null(psychOptions)) {
      args = append(list(r=x, nfactors=nf), psychOptions)
    } else {
      args = list(r=x, nfactors=nf)
    }
    suppressWarnings({
      faResult = do.call(psych::fa, args)
    })
  } else {
    if(!is.null(factanalOptions) | (is.null(factanalOptions) & is.null(psychOptions))) {
      args = append(list(covmat=x, factors=nf), factanalOptions)
    } else {
      args = list(covmat=x, factors=nf)
    }
    tryCatch(
      faResult <- do.call(factanal, args),
      error = function(c) {
        c$message = paste('factanal failed with the following message: \n', c$message)
        stop(c)
      })
    }

  # extract loadings, order by if desired
  load = faResult$loadings
  class(load) = 'matrix'
  if (ordering[1] == 'absolute') {
    cluster <- apply(abs(load), 1, which.max)
    ord <- sort(cluster, index.return = TRUE)
    index = ord$ix
  } else if (ordering[1] == 'raw') {
    cluster <- apply(load, 1, which.max)
    ord <- sort(cluster, index.return = TRUE)
    index = ord$ix
  } else if (ordering[1] == 'first'){
    load = data.frame(var=rownames(load), load)
    index = order(load[,1], decreasing=T)
  } else {
    sortload = psych::fa.sort(load)
    index = rownames(sortload)
  }

  ## reorder x
  ##=======================
  x <- x[index, index]

  ## cellnote
  ##====================
  if (!missing(cellnote))
    cellnote <- cellnote[index, index]
  if(missing(cellnote)) {
    cellnote <- round(x, digits = digits)
  }

  # Check that cellnote is o.k.:
  if (is.null(dim(cellnote))) {
    if (length(cellnote) != nr*nc) {
      stop("Incorrect number of cellnote values")
    }
    dim(cellnote) <- dim(x)
  }
  if (!identical(dim(x), dim(cellnote))) {
    stop("cellnote matrix must have same dimensions as x")
  }


  ## Final touches before htmlwidgets
  ##=======================

  mtx <- list(data = as.character(t(cellnote)),
              dim = dim(x),
              rows = rownames(x),
              cols = colnames(x)
  )

  # create colors for pos and neg; create colorMatrix
  colscalePos = scales::col_numeric(RColorBrewer::brewer.pal(9, 'Blues'), c(0,1), na.color = "transparent")
  colscaleNeg = scales::col_numeric(rev(RColorBrewer::brewer.pal(9, 'Reds')), c(0,-1), na.color = "transparent")

  colorMatrix = matrix(NA, ncol(x), nrow(x))
  colorMatrix[x>0] = colscalePos(x[x>0])
  colorMatrix[x<0] = colscaleNeg(x[x<0])

  imgUri <- encodeAsPNG(t(x), colorMatrix)

  options <- NULL   # required

  options <- c(options, list(
    xaxis_height = xaxis_height,
    yaxis_width = yaxis_width,
    xaxis_font_size = xaxis_font_size,
    yaxis_font_size = yaxis_font_size,
    brush_color = brush_color,
    show_grid = show_grid,
    anim_duration = anim_duration,
    yclust_width = 0,
    xclust_height = 0
  ))

  payload <- list(rows = NULL, cols = NULL, matrix = mtx, image = imgUri,
                  theme = theme, options = options)

  # create widget
  htmlwidgets::createWidget(
    name = 'd3heatmap',
    payload,
    width = width,
    height = height,
    package = 'd3heatmap',
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE)
  )
}

encodeAsPNG <- function(x, colors) {
  colorData <- as.raw(col2rgb(colors, alpha=TRUE))  # Need to fix to 0 white
  dim(colorData) <- c(4, ncol(x), nrow(x))
  pngData <- png::writePNG(colorData)
  encoded <- base64enc::base64encode(pngData)
  paste0("data:image/png;base64,", encoded)
}

#' Wrapper functions for using corrheat in shiny
#'
#' Use \code{corrheatOutput} to create a UI element, and \code{renderCorrheat}
#' to render the heatmap.
#'
#' @param outputId Output variable to read from
#' @param width,height The width and height of the map (see
#' \link[htmlwidgets]{shinyWidgetOutput})
#' @param expr An expression that generates a \code{\link[d3heatmap]{d3heatmap}} object
#' @param env The environment in which to evaluate \code{expr}
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @details Note that this is not a priority in the least for me at this time,
#'   but I went ahead and ported the d3heatmap code and at least the following
#'   example will work fine.
#'
#' @seealso \code{\link{corrheat}}, \code{\link[d3heatmap]{d3heatmapOutput}},
#' \code{\link[d3heatmap]{renderD3heatmap}}
#'
#' @importFrom grDevices col2rgb
#'
#' @examples
#' \donttest{
#' library(lazerhawk)
#' library(shiny)
#'
#' ui <- fluidPage(
#'   h1("A corrheat demo"),
#'   selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
#'   corrheatOutput("heatmap")
#' )
#'
#' server <- function(input, output, session) {
#'   output$heatmap <- renderCorrheat({
#'     corrheat(cor(mtcars), colors = input$palette)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'

#' @export
corrheatOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'd3heatmap', width, height, package = 'd3heatmap')
}

#' @rdname corrheatOutput
#' @export
renderCorrheat <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, corrheatOutput, env, quoted = TRUE)
}
