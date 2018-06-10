context('test visualization themes')

require(ggplot2)  # because travis

test_that('Error returned for non-plotly class for theme_plotly', {
  suppressWarnings(expect_error(data.frame(x=1:3, y=4:6) %>% theme_plotly()))
})

test_that('Check if plotly object returned', {
  suppressWarnings(expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_plotly(), 'plotly'))
})

test_that('Check if plotly object returned', {
  suppressWarnings(expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_blank(), 'plotly'))
})


test_that('Check if ggplot object returned', {
  suppressWarnings(expect_s3_class(ggplot2::qplot(x = rnorm(100)) + theme_trueMinimal(), 'ggplot'))
})
