context('ggclean error for inappropriate input')
test_that('Error returned for non-plotly class for theme_plotly', {
  expect_error(data.frame(x=1:3, y=4:6) %>% theme_plotly())
})

context('theme_plotly returns a plotly object')
test_that('Check if plotly object returned', {
  expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_plotly(), 'plotly')
})

context('theme_trueMinimal returns a ggplot object')
test_that('Check if ggplot object returned', {
  expect_s3_class(ggplot2::qplot(x = rnorm(100)) + theme_trueMinimal(), 'ggplot')
})
