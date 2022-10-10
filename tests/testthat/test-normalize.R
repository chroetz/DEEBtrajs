randomTrajs <- function(d, n) {
  makeTrajs(
    time = 0,
    state = matrix(rnorm(n*d), ncol=d))
}

testRandom <- function(d, n) {
  trajs <- randomTrajs(1,1)
  norm <- calculateNormalization(trajs)
  normedTrajs <- norm$normalize(trajs)
  normedMean <- colMeans(normedTrajs$state)
  normedCov <- cov(normedTrajs$state)
  expect_equal(normedMean, rep(0, length(normedMean)))
  if (nrow(trajs$state) >= 2) {
    expect_equal(normedCov, diag(nrow=nrow(normedCov)))
  }
}

test_that("random", {
  testRandom(1, 1)
  testRandom(2, 1)
  testRandom(1, 2)
  testRandom(2, 2)
  testRandom(3, 1)
  testRandom(1, 3)
  testRandom(3, 3)
  testRandom(2, 3)
  testRandom(3, 2)
  testRandom(3, 10)
  testRandom(10, 3)
})
