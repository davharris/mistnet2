context("inflating vectors to matrices")

test_that("expansion doesn't affect non-inflatables", {
  expect_identical(1:10, inflate(1:10))
  expect_identical(matrix(1:10), inflate(matrix(1:10)))
})

test_that("expansion rejects matrices", {
  expect_error(make_inflatable(matrix(1:10), by = "row", n = 5), "dim\\(x\\)")
})

test_that("inflate() inflates correctly", {
  x = 1:11
  n = 7

  r = inflate(make_inflatable(x, by = "row", n = n))
  c = inflate(make_inflatable(x, by = "col", n = n))


  expect_equal(dim(r), c(length(x), n))

  # All elements of each row should be the same for `r`
  expect_true(
    all(apply(r, 1, function(x) x == x[1]))
  )

  # A column should match `x`
  expect_true(
    all(r[ , 1] == x)
  )

  expect_equal(ncol(r), n)

  # inflateing by rows gives the transpose of inflateing by columns
  expect_equal(r, t(c))
})
