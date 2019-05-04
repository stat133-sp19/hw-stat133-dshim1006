context("check for checkers")


test_that("check_prob returns true", {

  expect_true(check_prob(0.4))
  expect_true(check_prob(0.5))
  expect_error(check_prob(1.3))
  expect_error(check_prob(-2))
  expect_true(check_prob(1))
  expect_true(check_prob(0))
})


test_that("check_trials returns true", {
  expect_true(check_trials(1))
  expect_true(check_trials(5))
  expect_error(check_trials(-3))
  expect_true(check_trials(0))
})


test_that("check_success returns error", {
  expect_error(check_success(c(-1, 6), 1), 'invalid success value')
  expect_error(check_success(c(1, 2, 3), 1))
  expect_error(check_success(c(-1, 2, 4, 5), 2), 'invalid success value')
})
