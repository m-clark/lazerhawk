context('lowerTri error for data.frame input')
test_that('Error returned for data.frame input', {
  expect_error(lowerTri(data.frame(matrix(1:9))))
})

context('lowerTri error for non-square input')
test_that('Error returned for non-square input', {
  expect_error(lowerTri(matrix(1:10, ncol=5)))
})

context('lowerTri error for non-numeric input of otherVal argument')
test_that('Error returned for non-numeric input', {
  expect_error(lowerTri(matrix(1:9, ncol=3), otherVal = 'A'))
})
