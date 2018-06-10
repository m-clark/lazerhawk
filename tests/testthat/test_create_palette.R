context('test create_palette')

test_that('create_palette returns a list',{
  suppressWarnings(expect_type(create_palette('#ff5500', name='orange', toHCL=F), 'list'))
})

test_that('create_palette takes HCL arg and plot',{
  suppressWarnings(expect_type(create_palette('#ff5500', toHCL=T, plot=T), 'list'))
})

test_that('create_palette takes alpha arg',{
  suppressWarnings(expect_type(create_palette('#ff5500', name='orange', alpha=.5, plot=T), 'list'))
})

test_that('create_palette errs on bad alpha',{
  suppressWarnings(expect_error(create_palette('#ff5500', name='orange', alpha=2)))
})

test_that('create_palette errs on bad color',{
  suppressWarnings(expect_error(create_palette('barf')))
})
