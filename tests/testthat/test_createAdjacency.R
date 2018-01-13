

context('test adjacency and edgelist')
df = data.frame(n1 = 1:10, n2=letters[1:10])

test_that('createAdjacency returns numeric.', {
  expect_true(is.numeric(createAdjacency(df, n1 = 'n1', n2 = 'n2')))
})

test_that('createAdjacency can handle character.', {
  expect_true(is.numeric(createAdjacency(df, n1 = 'n1', n2 = 'n2', diagonal = rep('', 20))))
})


test_that('createEdges returns a df', {
  adj = createAdjacency(df, n1 = 'n1', n2 = 'n2')
  expect_s3_class(createEdges(adj), 'data.frame')
})

test_that('createEdges takes zeroEdges arg', {
  adj = createAdjacency(df, n1 = 'n1', n2 = 'n2')
  expect_s3_class(createEdges(adj, zeroEdges = T), 'data.frame')
})
