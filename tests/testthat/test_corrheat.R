

context('Message for NULL psych options')
test_that('Message for NULL psych options',{
  expect_message(corrheat(cor(mtcars)))
})

# test_that('output of corrheat is silent',{
#   expect_silent(corrheat(cor(mtcars), psychOptions = list(fm='ml')))
# })


context('corrheat error for non-square matrix input')
test_that('Error returned for non-square matrices', {
  expect_error(corrheat(mtcars))
})
