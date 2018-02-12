context('test visualization themes')

test_that('Error returned for non-plotly class for theme_plotly', {
  expect_error(data.frame(x=1:3, y=4:6) %>% theme_plotly())
})

test_that('Test theme_plotly works', {
  expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_plotly(), 'plotly')
})

test_that('Test theme_blank works', {
  expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_blank(), 'plotly')
})

test_that('Test mode bar for theme_plotly', {
  expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_plotly(MB=TRUE), 'plotly')
})

test_that('Test mode bar for theme_blank', {
  expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_blank(MB=TRUE), 'plotly')
})


test_that('Test theme_trueMinimal', {
  expect_s3_class(ggplot2::qplot(x = rnorm(100)) + theme_trueMinimal(), 'ggplot')
})
