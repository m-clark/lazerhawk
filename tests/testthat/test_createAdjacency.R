

context('Adjacency and Edgelist tests')
test_that('createAdjacency returns numeric.', {
  df = data.frame(n1 = 1:10, n2=letters[1:10])
  expect_true(is.numeric(createAdjacency(df, n1 = 'n1', n2 = 'n2')))
})


# test_that('createEdges returns identical df that goes into createAdjacency', {
#   df = data.frame(n1 = 1:10, n2=letters[1:10])
#   adj = createAdjacency(df, n1 = 'n1', n2 = 'n2')
#   expect_true(createEdges(adj) == df)
# })
