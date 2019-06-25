context('test combn_2_col')

d = data.frame(id = 1:4,
               labs = c('A,B', 'B,C,D,E', 'A,E', 'D,E'))
basic = suppressWarnings(combn_2_col(data=d, var='labs', max_m=3))

test_that('combn_2_col returns a data.frame', {
  suppressWarnings(expect_s3_class(basic, 'data.frame'))
})

test_that('combn_2_col returns expected number of columns', {
  init = suppressWarnings(combn_2_col(data=d, var='labs', max_m=1))
  suppressWarnings(expect_equal(ncol(init), ncol(d) + 6))  # 5 labels + combo column
})

test_that('combn_2_col handles factors, NAs, other separators', {
  init = data.frame(id = 1:5,
                    labs = factor(c('AB', 'B/C/D/E', 'A/E', 'D/E', NA)))
  suppressWarnings(expect_s3_class(combn_2_col(data=d, var='labs', sep='/', max_m=3), 'data.frame'))
})

test_that('combn_2_col handles other collapse', {
  suppressWarnings(expect_s3_class(combn_2_col(data=d, var='labs', collapse='-'), 'data.frame'))
})

test_that('combn_2_col can do 01', {
  suppressWarnings(expect_is(combn_2_col(data=d, var='labs', toInteger=TRUE)[['A']], 'integer'))
})

test_that('combn_2_col can do sparse', {
  suppressWarnings(expect_is(combn_2_col(data=d, var='labs', sparse=TRUE), 'dgCMatrix'))
})


