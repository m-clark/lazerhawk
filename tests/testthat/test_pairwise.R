

context('pairwise object dimensions')
test_that('Testing margins return appropriate dimension object',{
  expect_equal(dim(pairwise(mtcars, 1, cor)), c(nrow(mtcars), nrow(mtcars)))
  expect_equal(dim(pairwise(mtcars, 2, cor)), c(ncol(mtcars), ncol(mtcars)))
})


context('pairwise fails on character')
test_that('Characters will throw error', {
  expect_error(pairwise(cbind(letters, LETTERS), 1, cor))
})
