context("check for binomial")

test_that("bin_choose correctly chooses bin", {


  expect_equal(bin_choose(5, 2), 10 )
  expect_equal(bin_choose(5, 0), 1)
  expect_equal(bin_choose(5, 1:3), c(choose(5, 1), choose(5, 2), choose(5, 3)))
  expect_length(bin_choose(5, 1:3), 3)
})

test_that("bin_probability returns true probability", {

  expect_equal(round(bin_probability(2, 5, 0.5), 3), round(0.312, 3))
  expect_equal(round(bin_probability(0:2, 5, 0.5), 3),  round(c(0.03125, 0.15625, 0.31250), 3))
  expect_length(bin_probability(0:2, 5, 0.5), 3)
  expect_equal(round(bin_probability(55, 100, 0.45), 3), round(0.01075277, 3))
})


test_that("bin_distribution is distributed correctly", {

  expect_length(bin_distribution(5, 0.5), 2)
  expect_equal(sum(bin_distribution(5, 0.5)$probability), 1)
  expect_type(bin_distribution(5, 0.5), "list")
})

test_that("bin_cumulative is cumulative is cumulated correctly", {

  expect_length(bin_cumulative(5, 0.5), 3)
  expect_equal(bin_cumulative(5, 0.5)$cumulative[length(bin_cumulative(5, 0.5)$cumulative)], 1)
  expect_type(bin_cumulative(5, 0.5), "list")
})






