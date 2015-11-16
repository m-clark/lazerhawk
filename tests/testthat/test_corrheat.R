

context('Silent return of corrheat')
test_that('output of corrheat is silent',{
  expect_silent(corrheat(cor(mtcars)))
})

context('corrheat error for non-square matrix input')
test_that('Error returned for non-square matrices', {
  expect_error(corrheat(mtcars))
})
