

context('createCorr error for non-numeric x')
test_that('Error returned for non-numeric input', {
  expect_error(createCorr(factor(1:3)))
})

context('createCorr error for non-numeric diagonal')
test_that('Error returned for non-numeric input', {
  expect_error(createCorr(runif(3), diagonal = factor(1:3)))
})

context('createCorr fails with missing values in x')
test_that('Error returned for missing values', {
  expect_error(createCorr(c(runif(2), NA)))
})

context('createCorr returns matrix output')
test_that('createCorr returns matrix output', {
  expect_is(createCorr(runif(3)), 'matrix')
})
