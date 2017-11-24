context('test pairwise operations')

test_that('Testing margins return appropriate dimension object',{
  expect_equal(dim(pairwise(mtcars, 1, cor)), c(nrow(mtcars), nrow(mtcars)))
  expect_equal(dim(pairwise(mtcars, 2, cor)), c(ncol(mtcars), ncol(mtcars)))
})

test_that('pairwise fails on character', {
  expect_error(pairwise(cbind(letters, LETTERS), 1, cor))
})
