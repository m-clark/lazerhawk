context('test cat_by')

set.seed(1)
df1 <- tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 10),
  c = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE)
)


test_that('num_by returns a data frame', {
  expect_s3_class(cat_by(df1, main_var = g1), 'data.frame')
})

test_that('num_by takes a group var', {
  expect_s3_class(cat_by(df1, main_var = g1, group_var = g2), 'data.frame')
})

test_that('cat_by will take digits', {
  expect_s3_class(cat_by(df1, main_var = g1, group_var = g2, digits=2), 'data.frame')
})


test_that('cat_by will do percentages out of total', {
  cb1 = cat_by(df1, main_var = g1, group_var = g2, digits=2)
  cb2 = cat_by(df1, main_var = g1, group_var = g2, perc_by_group=F, digits=2)
  expect_false(identical(cb1, cb2))
})


test_that('cat_by warns on numeric', {
  expect_warning(cat_by(df1, main_var = b))
})

test_that('cat_by warns on too many levels', {
  expect_warning(cat_by(df1, main_var = c))
})
