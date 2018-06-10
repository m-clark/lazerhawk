context('test cat_by')

set.seed(1)
df1 <- tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 1),
  c = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE)
)


test_that('cat_by returns a data frame', {
  suppressWarnings(expect_s3_class(cat_by(df1, main_var = g1), 'data.frame'))
})

test_that('cat_by takes multiple main vars', {
  suppressWarnings(expect_s3_class(cat_by(df1, main_var = vars(g1,d)), 'data.frame'))
})

test_that('cat_by takes a group var', {
  suppressWarnings(expect_s3_class(cat_by(df1, main_var = g1, group_var = g2), 'data.frame'))
})

test_that('cat_by takes multiple main_vars and a group var', {
  suppressWarnings(expect_s3_class(cat_by(df1, main_var = vars(g1,d), group_var = g2), 'data.frame'))
})

test_that('cat_by will take digits', {
  suppressWarnings(expect_s3_class(cat_by(df1, main_var = g1, group_var = g2, digits=2), 'data.frame'))
})


test_that('cat_by will do percentages out of total', {
  cb1 = suppressWarnings(cat_by(df1, main_var = g1, group_var = g2, digits=2))
  cb2 = suppressWarnings(cat_by(df1, main_var = g1, group_var = g2, perc_by_group=F, digits=2))
  suppressWarnings(expect_false(identical(cb1, cb2)))
})


test_that('cat_by warns on numeric', {
  suppressWarnings(expect_warning(cat_by(df1, main_var = g2)))
})

test_that('cat_by warns on too many levels', {
  suppressWarnings(expect_warning(cat_by(df1, main_var = c)))
})
