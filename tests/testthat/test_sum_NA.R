context('test sum_na')

df1 <- tibble(
  g1 = factor(sample(1:2, 50, replace = TRUE), labels=c('a','b')),
  g2 = sample(1:4, 50, replace = TRUE),
  a = rnorm(50),
  b = rpois(50, 10),
  c = sample(letters, 50, replace=TRUE),
  d = sample(c(T,F), 50, replace=TRUE)
)

df_miss = df1 %>% mutate_all(function(x) {x[sample(1:length(x), 5)] = NA; x})
df_nan = df1 %>% mutate_at(dplyr::vars(a,b), function(x) {x[sample(1:length(x), 5)] = NaN; x})
df_blank = df1 %>% mutate_at(dplyr::vars(c), function(x)  {x[sample(1:length(x), 5)] = ''; x})

test_that('sum_NA correctly returns value', {
  expect_equal(df_miss %>% summarise_all(sum_NA) %>% sum(), 30)
})

test_that('sum_NA correctly returns value', {
  expect_equal(df1 %>% summarise_all(sum_NA) %>% sum(), 0)
})


test_that('sum_NaN correctly returns value', {
  expect_equal(df_nan %>% summarise_at(dplyr::vars(a,b), sum_NaN) %>% sum(), 10)
})

test_that('sum_NaN correctly returns value', {
  expect_equal(df1 %>% summarise_at(dplyr::vars(a,b), sum_NaN) %>% sum(), 0)
})

test_that('sum_NaN fails if not a numeric', {
  expect_error(df1 %>% summarise_at(dplyr::vars(g1), sum_NaN) %>% sum())
})

test_that('sum_blank correctly returns value', {
  expect_equal(df_blank %>% summarise_at(dplyr::vars(c), sum_blank) %>% sum(), 5)
})

test_that('sum_NaN correctly returns value', {
  expect_equal(df1 %>% summarise_at(dplyr::vars(c), sum_blank) %>% sum(), 0)
})

