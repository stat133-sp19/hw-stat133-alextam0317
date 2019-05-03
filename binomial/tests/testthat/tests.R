context("Tests")


test_that("the conditions are valid",{
  expect_true(check_prob(0.5))
  expect_error(check_prob(2))
  expect_length(check_prob(0.2),1)
})

test_that("the conditions are valid",{
  expect_true(check_trials(5))
  expect_error(check_trials(-1))
  expect_length(check_trials(0.2),1)
})

test_that("the conditions are valid",{
  expect_true(check_success(2,1))
  expect_error(check_success(1,2))
  expect_error(check_success(5,-1))
})

test_that("the conditions are valid",{
  np <- 5
  expect_equal(aux_mean(10,0.5),np)
  expect_length(aux_mean(10,0.5),length(np))
  expect_type(aux_mean(10,0.5),'double')
})

test_that("the conditions are valid",{
  v <- 10*0.3*0.7
  expect_equal(aux_variance(10,0.3),v)
  expect_length(aux_variance(10,0.3),length(v))
  expect_type(aux_variance(10,0.3),'double')
})

test_that("the conditions are valid",{
  m <- c(3,2)
  expect_equal(aux_mode(5,0.5),m)
  expect_length(aux_mode(5,0.5),length(m))
  expect_type(aux_mode(5,0.5),'double')
})

test_that("the conditions are valid",{
  s <- (1-2*0.3)/(sqrt(10*0.3*(1-0.3)))
  expect_equal(aux_skewness(10,0.3),s)
  expect_length(aux_skewness(10,0.3),length(s))
  expect_type(aux_skewness(10,0.3),'double')
})

test_that("the conditions are valid",{
  k <- (1-6*0.3*(1-0.3))/(10*0.3*(1-0.3))
  a <- -0.1238095
  expect_equal(aux_kurtosis(10,0.3),k)
  expect_length(aux_kurtosis(10,0.3),length(a))
  expect_type(aux_kurtosis(10,0.3), 'double')
})

test_that("the conditions are valid",{
  f <- 3*2/(1*2)
  expect_equal(bin_choose(3,1),f)
  expect_error(bin_choose(-1,2))
  expect_error(bin_choose(5,-1))
})

test_that("the conditions are valid",{
  y <- (3*2/(1*2))*0.3^1*0.7^2
  expect_equal(bin_probability(1,3,0.3),y)
  expect_error(bin_probability(-2,5,0.5))
  expect_error(bin_probability(2,5,3))
})

test_that("the conditions are valid",{
  expect_type(bin_distribution(5,0.5),'list')
  expect_error(bin_distribution(-2,0.5))
  expect_error(bin_distribution(2,3))
})

test_that("the conditions are valid",{
  expect_type(bin_cumulative(5,0.5),'list')
  expect_error(bin_cumulative(-2,0.5))
  expect_error(bin_cumulative(2,3))
})
