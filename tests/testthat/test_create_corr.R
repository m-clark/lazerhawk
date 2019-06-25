context('test create_corr')

cormat = create_corr(runif(3))

test_that('Error returned for non-numeric input', {
  expect_error(create_corr(factor(1:3)))
})

test_that('Error returned for non-numeric diagonal', {
  expect_error(create_corr(runif(3), diagonal = factor(1:3)))
})


test_that('Error returned for missing values', {
  expect_error(create_corr(c(runif(2), NA)))
})


test_that('create_corr returns matrix output', {
  expect_is(cormat, 'matrix')
})

test_that('create_corr returns symmetric matrix', {
  expect_equal(lower_tri(cormat, valuesOnly = T),
               upper_tri(cormat, valuesOnly = T))
})
