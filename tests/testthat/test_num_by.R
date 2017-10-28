context('test num_by')

df1 <- tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 10),
  c = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE)
)
test_that('num_by returns a data frame', {
  expect_s3_class(num_by(df1, main_var = a), 'data.frame')
})

test_that('num_by takes a group var', {
  expect_s3_class(num_by(df1, main_var = a, group_var = g2), 'data.frame')
})

test_that('num_by will take digits', {
  expect_s3_class(num_by(df1, main_var = a, group_var = g2, digits=2), 'data.frame')
})

test_that('fails on non-numeric', {
  expect_error(num_by(df1, main_var = c))
})

test_that('num_by is ok with logical', {
  expect_s3_class(num_by(df1, main_var = d, group_var = g2), 'data.frame')
})
