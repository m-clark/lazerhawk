

context('test adjacency and edgelist')
df = data.frame(n1 = 1:10, n2=letters[1:10])

test_that('create_adjacency returns numeric.', {
  expect_true(is.numeric(create_adjacency(df, n1 = 'n1', n2 = 'n2')))
})

test_that('create_adjacency can handle character.', {
  expect_true(is.numeric(create_adjacency(df, n1 = 'n1', n2 = 'n2', diagonal = rep('', 20))))
})


test_that('create_edges returns a df', {
  adj = create_adjacency(df, n1 = 'n1', n2 = 'n2')
  expect_s3_class(create_edges(adj), 'data.frame')
})

test_that('create_edges takes zeroEdges arg', {
  adj = create_adjacency(df, n1 = 'n1', n2 = 'n2')
  expect_s3_class(create_edges(adj, zeroEdges = T), 'data.frame')
})
