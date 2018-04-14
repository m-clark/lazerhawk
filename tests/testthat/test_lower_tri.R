context('test lower_tri')
test_that('Error returned for data.frame input', {
  expect_error(lower_tri(data.frame(matrix(1:9, 3, 3))))
})

test_that('Error returned for non-square input', {
  expect_error(lower_tri(matrix(1:10, ncol=5)))
})

test_that('Error returned for non-numeric input', {
  expect_error(lower_tri(matrix(1:9, ncol=3), otherVal = 'A'))
})

test_that('lower_tri returns matrix.', {
  expect_true(is.matrix(lower_tri(matrix(1:9, ncol=3))))
})

test_that('lower_tri returns vector.', {
  expect_true(is.numeric(lower_tri(matrix(1:9, ncol=3), valuesOnly = TRUE)))
})

test_that('upper_tri returns matrix.', {
  expect_true(is.matrix(upper_tri(matrix(1:9, ncol=3))))
})

test_that('upper_tri returns vector.', {
  expect_true(is.numeric(upper_tri(matrix(1:9, ncol=3), valuesOnly = TRUE)))
})
