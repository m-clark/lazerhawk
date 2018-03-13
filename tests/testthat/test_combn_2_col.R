context('test combn_2_col')

d = data.frame(id = 1:4,
               labs = c('A,B', 'B,C,D,E', 'A,E', 'D,E'))
basic = combn_2_col(data=d, var='labs', max_m=3)

test_that('combn_2_col returns a data.frame', {
  expect_s3_class(basic, 'data.frame')
})

test_that('combn_2_col returns expected number of columns', {
  init = combn_2_col(data=d, var='labs', max_m=1)
  expect_equal(ncol(init), ncol(d) + 6)  # 5 labels + combo column
})

test_that('combn_2_col handles factors, NAs, other separators', {
  init = data_frame(id = 1:5,
                    labs = factor(c('AB', 'B/C/D/E', 'A/E', 'D/E', NA)))
  expect_s3_class(combn_2_col(data=d, var='labs', sep='/', max_m=3), 'data.frame')
})

test_that('combn_2_col handles other collapse', {
  expect_s3_class(combn_2_col(data=d, var='labs', collapse='-'), 'data.frame')
})

test_that('combn_2_col can do 01', {
  expect_is(combn_2_col(data=d, var='labs', toInteger=TRUE)[['A']], 'integer')
})

test_that('combn_2_col can do sparse', {
  expect_is(combn_2_col(data=d, var='labs', sparse=TRUE), 'dgCMatrix')
})


