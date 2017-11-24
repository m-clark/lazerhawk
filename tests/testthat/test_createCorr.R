context('test createCorr')

test_that('Error returned for non-numeric input', {
  expect_error(createCorr(factor(1:3)))
})

test_that('Error returned for non-numeric diagonal', {
  expect_error(createCorr(runif(3), diagonal = factor(1:3)))
})


test_that('Error returned for missing values', {
  expect_error(createCorr(c(runif(2), NA)))
})


test_that('createCorr returns matrix output', {
  expect_is(createCorr(runif(3)), 'matrix')
})
