context('test brms_SummaryTable')

test_that('brms_SummaryTable returns a data frame by default', {
  expect_s3_class(brms_SummaryTable(fit1), 'data.frame')
})

test_that('brms_SummaryTable can round', {
  rounded = brms_SummaryTable(fit1, round=4)
  expect_equal(nchar(as.character(rounded$Estimate)[1]), 6)
})

test_that('brms_SummaryTable can see the stars', {
  expect_match(levels(factor(brms_SummaryTable(fit1, astrology = TRUE)$Notable)), '*')
})

test_that('brms_SummaryTable can do ratios', {
  expect_equal(ncol(brms_SummaryTable(fit1, hype = TRUE)), 7)
})

test_that('brms_SummaryTable can do both', {
  expect_equal(ncol(brms_SummaryTable(fit1, astrology = TRUE, hype = TRUE)), 8)
})

test_that('brms_SummaryTable can panderize', {
  # file.remove('brms_pander_output') # if you need to reset
  expect_known_output(brms_SummaryTable(fit1, panderize = TRUE), 'brms_pander_output', print = TRUE)
})


test_that('brms_SummaryTable can justify', {
  # file.remove('brms_pander_output1') # if you need to reset
  expect_known_output(brms_SummaryTable(fit1, panderize = TRUE, justify = 'lrrrr'),
                      'brms_pander_output1',
                      print = TRUE)
})


test_that('brms_SummaryTable can justify with hype stars', {
  # file.remove('brms_pander_output2') # if you need to reset
  expect_known_output(brms_SummaryTable(fit1,
                                        astrology = TRUE,
                                        hype = TRUE,
                                        panderize = TRUE,
                                        justify = 'lrrrrrrr'),
                      'brms_pander_output2',
                      print = TRUE)
})

test_that('brms_SummaryTable can justify with just hype', {
  expect_known_output(brms_SummaryTable(fit1, astrology = FALSE, hype = TRUE, panderize = TRUE, justify = 'lrrrrrr'),
                      'brms_pander_output3',
                      print = TRUE)
})

test_that('brms_SummaryTable can add justify with just astro', {
  expect_known_output(brms_SummaryTable(fit1, astrology = TRUE, hype = FALSE, panderize = TRUE, justify = NULL),
                      'brms_pander_output4',
                      print = TRUE)
})

test_that('brms_SummaryTable will error with non-brms object', {
  expect_error(brms_SummaryTable(lm(mpg ~ wt, mtcars)))
})
