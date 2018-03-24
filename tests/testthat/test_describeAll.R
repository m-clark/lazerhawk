context('test describe_all')


df1 <- tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 10),
  c = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE),
  a_b = a*b,
  b_sq = b^2
)

test_that('describe_all returns a list', {
  expect_is(describe_all(df1), 'list')
})

test_that('describe_all handles no categorical', {
  expect_is(describe_all(df1[,c('g2', 'a', 'b')]), 'list')
})

test_that('describe_all handles no numeric', {
  expect_is(describe_all(df1[,c('g1', 'c', 'd')]), 'list')
})

test_that('describe_all fails if not a data frame', {
  expect_error(describe_all(df1 %>% pull(g1)))
})

test_that('describe_all can take digits argument', {
  res = describe_all(data.frame(x=pi), digits = 2)[[1]]
  expect_equal(nchar(as.character(res$Mean)), 4)
})

test_that('describe_all can handle empty levels', {
  res = describe_all(iris %>% filter(Species != 'setosa'))[[2]]
  expect_equal(nrow(res), 2)
})

test_that('describe_all can handle empty categorical variables', {
  expect_warning(describe_all(iris %>% mutate(Species = NA)))
})

test_that('describe_all can drop NA', {
  expect_equal(nrow(describe_all(iris %>% rbind(NA), NAcat_include = T)[[2]]), 4)
})




test_that('describe_all_num returns a data.frame', {
  expect_is(describe_all_num(df1 %>% select_if(is.numeric)), 'data.frame')
})

test_that('describe_all_num fails if not a data frame', {
  expect_error(describe_all_num(df1 %>% pull(a)))
})

test_that('describe_all_num returns message if no numeric', {
  expect_message(describe_all_num(df1 %>% select_if(function(x) !is.numeric(x))))
})



test_that('describe_all_cat returns a data.frame', {
  expect_is(describe_all_cat(df1) %>% select_if(function(x) !is.numeric(x)), 'data.frame')
})

test_that('describe_all_cat fails if not a data frame', {
  expect_error(describe_all_cat(df1 %>% pull(a)))
})

test_that('describe_all_cat returns message if no categorical', {
  expect_message(describe_all_cat(df1 %>% select_if(is.numeric)))
})

test_that('describe_all_cat returns warning if no levels', {
  expect_warning(describe_all_cat(df1 %>% select(g1) %>% filter(g1=='c')))
})

test_that('describe_all_cat can drop NA', {
  expect_equal(nrow(describe_all_cat(iris %>% rbind(NA), NAcat_include = T)), 4)
})


test_that('describeAll works', {
  expect_is(describeAll(df1), 'list')
})


