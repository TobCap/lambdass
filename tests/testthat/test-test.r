context("test for lambda syntax-sugar")

test_that("test", {
  expect_identical(function(x) x, f.(x, x))
  expect_identical(function(x, y) x + y, f.(x, y, x + y))
  expect_error(f.(x = 1, y = x+1)) # body should not be named
  expect_identical(function(..) .., ~~ ..)
  expect_identical(function(._1, ._2) ._1 + ._2, ~~ ..1 + ..2)
  expect_error(~~ ..1 + ..3)
  expect_error(~~ ..2 + ..3)
  expect_error(~~ .. + ..1)
  expect_error(~~ .. + ..2)
  expect_equal(function(x) x, x %->% x)
  expect_equal(function(x) {x}, x %->% {x},
               label = FALSE, expected.label = FALSE)
  expect_equal(function(x, y) {x + y}, {x; y} %->% {x + y},
               label = FALSE, expected.label = FALSE)
  expect_equal(function(x, y) {x + y}, f(x, y) %->% {x + y},
               label = FALSE, expected.label = FALSE)
})
