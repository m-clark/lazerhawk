context('test describeAll')


df1 <- tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 10),
  c = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE),
  a_b = a*b,
  b_sq = b^2
)

test_that('describeAll returns a list', {
  expect_is(describeAll(df1), 'list')
})

test_that('describeAll handles no categorical', {
  expect_is(describeAll(df1[,c('g2', 'a', 'b')]), 'list')
})

test_that('describeAll handles no numeric', {
  expect_is(describeAll(df1[,c('g1', 'c', 'd')]), 'list')
})

test_that('describeAll fails if not a data frame', {
  expect_error(as.data.frame(describeAll)(df1[,c('g1')]))
})

test_that('describeAll can take digits argument', {
  res = describeAll(data.frame(x=pi), digits = 2)[[1]]
  expect_equal(nchar(as.character(res$Mean)), 4)
})
