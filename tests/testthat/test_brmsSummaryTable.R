context('test brmsSummaryTable')

test_that('brmsSummaryTable returns a data frame', {
  expect_s3_class(brms_SummaryTable(fit1), 'data.frame')
})

test_that('brmsSummaryTable can see the stars', {
  expect_match(levels(brms_SummaryTable(fit1, astrology = TRUE)$Notable), '*')
})

test_that('brmsSummaryTable can do ratios', {
  expect_identical(!is.null(brms_SummaryTable(fit1, astrology = F, hype = TRUE)$`Evidence Ratio`), TRUE)
})

test_that('brmsSummaryTable can do both', {
  expect_identical(!is.null(brms_SummaryTable(fit1, astrology = TRUE, hype = TRUE)$`Evidence Ratio`), TRUE)
})


test_that('brmsSummaryTable can justify', {
  expect_is(brms_SummaryTable(fit1, astrology = TRUE, hype = TRUE, panderize = T, justify = 'lrrrrrrr'), 'NULL')
})
