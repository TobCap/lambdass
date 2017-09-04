context("test for double tilda")

test_that("test-doubletilda", {
  expect_identical(~~ .., function(..) ..)
  
  f1 <- (~~ (~~ ..1 + ..2)(..1, ..2))
  expect_identical(f1(10, 20), 30)

  g1 <- function(x) x + (function(y) y * 2)(x)
  g2 <- ~~ .. + (~~ .. * 2)(..)
  g3 <- ~~ ..1 + (~~ ..1 * 2)(..1)

  expect_identical(g1(10), g2(10))
  expect_identical(g2(10), g3(10))
  expect_identical(g3(10), g1(10))
  
})

test_that("dot variable treatment", {
  ff <- (function(...) ~..1 + ..2)(10, 20)
  expect_identical(eval(ff[[2]], environment(ff)), 30)

  gg <- function(...) (~~..1 + ..2)(..1, ..2)
  expect_identical(gg(10, 20), 30)

  hh <- function(...) ..1 + (~~..1 + ..2)(..1, ..2)
  expect_identical(hh(10, 20), 40)
})
