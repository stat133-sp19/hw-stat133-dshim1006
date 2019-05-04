context("check for summary measures")

test_that("aux_mean returns mean", {

  expect_equal(aux_mean(10, 0.3), 3)
  expect_length(aux_mean(10, 0.3), 1)
  expect_type(aux_mean(10, 0.3), "double")

})

test_that("aux_variance returns variance", {

  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_length(aux_variance(10, 0.3), 1)
  expect_type(aux_variance(10, 0.3), "double")
})

test_that("aux_mode returns mode", {

  expect_equal(aux_mode(10, 0.3), 3)
  expect_length(aux_mode(10, 0.3), 1)
  expect_type(aux_mode(10, 0.3), "integer")
})

test_that("aux_skewness return skewness", {

  expect_equal(round(aux_skewness(10, 0.3), 3), round(0.2760262, 3))
  expect_length(aux_skewness(10, 0.3), 1)
  expect_type(aux_skewness(10, 0.3), "double")
})

test_that("aux_kurtosis returns kurtosis", {

  expect_equal(round(aux_kurtosis(10, 0.3), 3), round(-0.267, 3))
  expect_length(aux_kurtosis(10, 0.3), 1)
  expect_type(aux_kurtosis(10, 0.3), "double")
})
