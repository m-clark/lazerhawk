context('lowerTri error for data.frame input')
test_that('Error returned for data.frame input', {
  expect_error(lowerTri(data.frame(matrix(1:9, 3, 3))))
})

test_that('Error returned for non-square input', {
  expect_error(lowerTri(matrix(1:10, ncol=5)))
})

test_that('Error returned for non-numeric input', {
  expect_error(lowerTri(matrix(1:9, ncol=3), otherVal = 'A'))
})

test_that('lowerTri returns matrix.', {
  expect_true(is.matrix(lowerTri(matrix(1:9, ncol=3))))
})

test_that('lowerTri returns vector.', {
  expect_true(is.numeric(lowerTri(matrix(1:9, ncol=3), valuesOnly = TRUE)))
})

test_that('upperTri returns matrix.', {
  expect_true(is.matrix(upperTri(matrix(1:9, ncol=3))))
})

test_that('upperTri returns vector.', {
  expect_true(is.numeric(upperTri(matrix(1:9, ncol=3), valuesOnly = TRUE)))
})
