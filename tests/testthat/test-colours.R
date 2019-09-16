test_that("n is at least 1", {
  expect_error(
    make_shades("goldenrod", -1),
    "n must be at least 1"
    )
  expect_error(
    make_shades("goldenrod", 0),
    "n must be at least 1"
  )
})

test_that("colour is a valid name", {
  expect_error(
    make_shades("apple", 5),
    "The colour provided is not a valid name"
  )
})
